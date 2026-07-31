#file is a path to a raw txt file extracted using imessage-exporter
#will return one line per message, not one line per word

#' Parse a single imessage-exporter text file into a message-level data frame
#'
#' Reads one raw \code{.txt} export, reconstructs message boundaries from the
#' timestamp header lines, strips reply previews / tapbacks / read receipts /
#' (optionally) attachments, and labels each message with its speaker and the
#' other participant (recipient). Returns one row per message, not per word.
#'
#' Bails out (returning an empty \code{tibble()}) when the conversation cannot be
#' treated as a clean dyad: fewer than two identifiable speakers, or a participant
#' identified only by a phone number ("+1...") rather than a saved display name.
#'
#' @param file Path to a single raw \code{.txt} file from imessage-exporter.
#' @param speaker_name Display name to substitute for the literal "Me" (the
#'   account owner).
#' @param remove_attach Logical; if TRUE, drop lines that are attachment file
#'   paths. Defaults to TRUE.
#'
#' @return A data frame with columns \code{speaker}, \code{text},
#'   \code{datetime}, and \code{recipient}; or an empty \code{tibble()} if the
#'   file is not a usable two-person conversation.
#' @seealso \code{\link{message_to_list}}
#' @keywords internal
process_txt_2 <- function(file, speaker_name, remove_attach = TRUE) {

  # messages are separated by timestamp header lines like
  #   "Jan 26, 2024 10:38:36 AM (Read by you after 22 seconds)"
  # used to be blank-line separated, but that breaks on multi-paragraph
  # messages (they have blank lines inside them) and on messages whose text
  # happens to start with something that looks like a timestamp
  header_re <- "^[A-Z][a-z]{2} \\d{1,2}, \\d{4} +\\d{1,2}:\\d{2}:\\d{2} [AP]M( \\(Read by[^)]*\\))?$"

  # %b/%p in the format string below are locale-dependent, so parsing under
  # whatever locale the user happens to have would silently turn every
  # timestamp into NA on a non-English machine. Force C for the parse and put
  # it back after.
  old_lc_time <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", "C")
  on.exit(Sys.setlocale("LC_TIME", old_lc_time), add = TRUE)

  text_raw <- readLines(file)
  text_raw_df <- data.frame(raw_text = text_raw)
  text_df <- text_raw_df %>%
    mutate(
      # bump the chunk counter on every header line; everything after it
      # belongs to that message until the next header shows up
      chunk_id = cumsum(grepl(pattern = header_re, x = raw_text)),
      reply_text = case_when(
        # quoted-reply previews are indented 4 spaces, drop them so they
        # don't get glued onto the real message body
        grepl(pattern = "^    ", x = raw_text) == TRUE ~ 0,
        TRUE ~ 1
      )
    ) %>%
    filter(
      raw_text != "" &
      reply_text == 1
    ) %>%
    group_by(chunk_id) %>%
    summarize(
      # collapse everything in one chunk into a single string, keep the
      # newlines so multi-paragraph messages don't get flattened
      full_content = paste(raw_text, collapse = "\n"),
      .groups = "drop"
    ) %>%
    # tapback chunks are just this one line by itself
    filter(full_content != "This message responded to an earlier message.")

  text_df_split <- text_df %>%
    # chunk is timestamp \n sender \n body (maybe multi-line) - split on the
    # first two newlines and keep the rest of the body together
    separate(full_content,
             into = c("time", "speaker", "text"),
             sep = "\n",
             extra = "merge",
             fill = "right") %>%
    mutate(
      # sometimes this string survives inside the body itself, strip it
      text = gsub(pattern = "This message responded to an earlier message.",
                     replacement = "",
                     x = text),
      # inline tapback markers, e.g. "Tapbacks: ♥ 2"
      text = str_remove(pattern = "Tapbacks:\\D*",
                           string = text),
      # "(Read by you after X seconds)" needs to go before we can parse time
      time = str_remove(pattern = "\\s*\\(Read by.*?\\)",
                           string = time)
    ) %>%
    filter(!is.na(text)) %>%   # header with no body underneath it
    mutate(
      attach_text = case_when(
        # attachments show up as file paths into the Messages attachments dir
        grepl(pattern = "/Library/Messages/Attachments/", x = text, fixed = TRUE) == TRUE ~ 0,
        TRUE ~ 1
      )
    ) %>%
    { if(remove_attach == TRUE) filter(., attach_text == 1) else . } %>%
    #filter(attach_text == 1) %>%
    mutate(
      speaker = recode(speaker, Me = speaker_name),
      # POSIXct instead of strptime's POSIXlt - the list-based one doesn't
      # play well with summarize/unique or across parallel workers
      datetime = as.POSIXct(time,
                            format = "%b %d, %Y %I:%M:%S %p",
                            tz = Sys.timezone())
    ) %>%
    select(speaker, text, datetime)

  # 1:2 here is what actually guards against group chats - unique() on a
  # group thread returns more than 2 names
  speakers <- unique(text_df_split$speaker)[1:2]

  # fewer than two real speakers means this isn't a usable conversation
  if (length(speakers) == 1 | length(speakers) == 0 |
      is.na(speakers[[1]]) | is.na(speakers[[2]])) {
    return(tibble())
  }

  # no saved contact name means the export just has a phone number, and
  # everything downstream expects a real name
  if (str_detect(string = speakers[1], pattern = "\\+") |
      str_detect(string = speakers[2], pattern = "\\+")) {

    return(tibble())
  }


  text_df_names <- text_df_split %>%
    # recipient is just whichever of the two isn't the speaker on this row
    mutate(recipient = case_when(
      speaker == speakers[1] ~ speakers[2],
      speaker == speakers[2] ~ speakers[1],

    ))



  return(text_df_names)






}
