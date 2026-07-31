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

  # A real message always starts with a header line that is ONLY a timestamp
  # (optionally followed by a "(Read by ...)" receipt), e.g.
  #   "Jan 26, 2024 10:38:36 AM (Read by you after 22 seconds)"
  # We use this to mark message boundaries. Using blank lines as boundaries
  # (the previous approach) breaks two ways:
  #   1. multi-paragraph messages contain internal blank lines, so they were
  #      split apart and everything after the first paragraph was dropped, and
  #   2. the end-anchor ($) prevents a message whose *text* happens to begin
  #      with a timestamp from being mistaken for a new message.
  header_re <- "^[A-Z][a-z]{2} \\d{1,2}, \\d{4} +\\d{1,2}:\\d{2}:\\d{2} [AP]M( \\(Read by[^)]*\\))?$"

  # Timestamps are parsed with %b (month abbreviation) and %p (AM/PM), both of
  # which are locale-dependent. imessage-exporter always writes English, so on
  # a machine with a non-English LC_TIME every timestamp would silently parse
  # to NA — and NA datetimes are then dropped by date filtering, or corrupt the
  # cumulative exchange numbering downstream. Parsing under the C locale makes
  # the result independent of the user's system settings.
  old_lc_time <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", "C")
  on.exit(Sys.setlocale("LC_TIME", old_lc_time), add = TRUE)

  text_raw <- readLines(file)                        # read every line of the export as a character vector
  text_raw_df <- data.frame(raw_text = text_raw)    # wrap in a df so we can use dplyr verbs
  text_df <- text_raw_df %>%
    mutate(
      # every time we hit a timestamp header, increment the chunk counter — all
      # lines that follow belong to that message until the next header fires
      chunk_id = cumsum(grepl(pattern = header_re, x = raw_text)),
      reply_text = case_when(
        # iMessage quoted-reply previews are indented with 4 spaces; drop them
        # so they don't get concatenated into the sender's actual message body
        grepl(pattern = "^    ", x = raw_text) == TRUE ~ 0,
        TRUE ~ 1
      )
    ) %>%
    filter(
      raw_text != "" &        # drop blank lines (whitespace-only separators)
      reply_text == 1         # drop indented quoted-reply lines
    ) %>%
    group_by(chunk_id) %>%
    summarize(
      # collapse all lines belonging to a single message into one string,
      # preserving newlines so multi-paragraph messages stay intact
      full_content = paste(raw_text, collapse = "\n"),
      .groups = "drop"
    ) %>%
    # tapback acknowledgements ("reacted with a heart") produce a standalone
    # chunk with only this string — remove them before splitting
    filter(full_content != "This message responded to an earlier message.")

  text_df_split <- text_df %>%
    # each chunk is now: timestamp \n sender \n message body (possibly multi-line)
    # split on the first two newlines only; extra = "merge" keeps the body intact
    separate(full_content,
             into = c("time", "speaker", "text"),
             sep = "\n",
             extra = "merge",
             fill = "right") %>%   # chunks with no message body -> text = NA (dropped below)
    mutate(
      # strip residual "responded to earlier message" strings that survived inside body text
      text = gsub(pattern = "This message responded to an earlier message.",
                     replacement = "",
                     x = text),
      # remove tapback lines that appear inline (e.g. "Tapbacks: ♥ 2") — they
      # are metadata, not conversation content
      text = str_remove(pattern = "Tapbacks:\\D*",
                           string = text),
      # strip the "(Read by you after X seconds)" receipt from the timestamp
      # so strptime can parse it cleanly later
      time = str_remove(pattern = "\\s*\\(Read by.*?\\)",
                           string = time)
    ) %>%
    filter(!is.na(text)) %>%   # drop header-only chunks that had no body (fill = "right" left NA)
    mutate(
      attach_text = case_when(
        # attachment lines are file paths into ~/Library/Messages/Attachments/;
        # flag them so we can optionally strip them below
        grepl(pattern = "/Library/Messages/Attachments/", x = text, fixed = TRUE) == TRUE ~ 0,
        TRUE ~ 1
      )
    ) %>%
    { if(remove_attach == TRUE) filter(., attach_text == 1) else . } %>%   # conditionally drop attachment lines
    #filter(attach_text == 1) %>%
    mutate(
      speaker = recode(speaker, Me = speaker_name),                              # replace the literal "Me" with the caller-supplied name
      # as.POSIXct, not strptime: strptime returns a list-based POSIXlt, which
      # is fragile through summarize()/unique() and across parallel workers.
      # The timestamps are local wall-clock time as displayed by the exporter,
      # so they are interpreted in the system timezone, stated explicitly
      # rather than left implicit.
      datetime = as.POSIXct(time,
                            format = "%b %d, %Y %I:%M:%S %p",
                            tz = Sys.timezone())
    ) %>%
    select(speaker, text, datetime)

  # grab the two participants; index 1:2 guards against group chats where
  # unique() could return more than two names
  speakers <- unique(text_df_split$speaker)[1:2]

  # bail out if the conversation has fewer than two identifiable speakers
  # (e.g. an empty export or a one-sided log)
  if (length(speakers) == 1 | length(speakers) == 0 |
      is.na(speakers[[1]]) | is.na(speakers[[2]])) {
    return(tibble())
  }

  # phone numbers appear when a contact has no saved display name; skip these
  # because downstream code expects real names, not "+1XXXXXXXXXX" strings
  if (str_detect(string = speakers[1], pattern = "\\+") |
      str_detect(string = speakers[2], pattern = "\\+")) {

    return(tibble())
  }


  text_df_names <- text_df_split %>%
    # derive recipient from speaker — each row gets the *other* participant's name
    mutate(recipient = case_when(
      speaker == speakers[1] ~ speakers[2],
      speaker == speakers[2] ~ speakers[1],

    ))



  return(text_df_names)






}
