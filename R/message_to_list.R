#' Parse iMessage thread files into a list of per-conversation data frames
#'
#' Reads each exported thread with \code{\link{process_txt_2}}, tags it with a
#' conversation number, and keeps only true dyads (conversations with exactly two
#' named speakers).
#'
#' @param threads Character vector of file paths, e.g. the output of
#'   \code{\link{list_threads}}.
#' @param speaker Name to substitute for the literal "Me" in the exports
#'   (i.e. the account owner's display name).
#' @param remove_attach Logical; if TRUE, drop attachment placeholder lines.
#'   Defaults to TRUE. Passed through to \code{\link{process_txt_2}}.
#'
#' @return A list of data frames, one per conversation, each carrying a
#'   \code{convo_num} column. Threads that fail to parse, are one-sided, or have
#'   more than two speakers are dropped.
#' @seealso \code{\link{process_txt_2}}, \code{\link{list_to_df}}
#' @keywords internal
message_to_list <- function(threads, speaker, remove_attach = TRUE) {

  msg_list <- threads %>%
    # parse each export into one row per message
    map(~ process_txt_2(.x, speaker_name = speaker, remove_attach = remove_attach)) %>%
    # number conversations by position in the list
    imap( ~ .x %>% mutate(convo_num = .y)) %>%
    # the real dyad check, on parsed content rather than filenames. the
    # "speaker" %in% names(.x) part short-circuits: process_txt_2 hands back
    # empty tibbles for failed parses, and touching $speaker on those throws
    # an "unknown column" warning. na.omit ignores stray NA speakers.
    keep(~ "speaker" %in% names(.x) &&
           length(unique(na.omit(.x$speaker))) == 2) %>%
    keep(~ any(grepl(speaker, unique(na.omit(.x$speaker)))))

  return(msg_list)
}

