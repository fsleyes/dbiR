#' Combine a list of conversation data frames into one tidy data frame
#'
#' Row-binds the per-conversation data frames produced by
#' \code{\link{message_to_list}} into a single message-level data frame, then
#' drops contacts whose name contains no alphabetic characters (e.g. raw
#' phone-number handles that were never resolved to a display name).
#'
#' @param msg_list A list of conversation data frames, e.g. the output of
#'   \code{\link{message_to_list}}.
#'
#' @return A single data frame with one row per message across all conversations
#'   (columns: speaker, text, datetime, recipient, convo_num). Returns an empty
#'   data frame, with a warning, when no conversations survived parsing.
#' @seealso \code{\link{message_to_list}}, \code{\link{read_imessages}}
#' @keywords internal
list_to_df <- function(msg_list) {

  # An empty list means every thread was filtered out upstream — wrong
  # directory, group chats only, or no contacts with saved display names.
  # Without this guard the filters below fail on a zero-column data frame with
  # "object 'speaker' not found", which says nothing about the real cause.
  if (length(msg_list) == 0) {
    warning("No valid dyadic conversations were found. Threads are dropped ",
            "when the file is a group chat, has only one speaker, or names a ",
            "participant by phone number rather than a saved contact name.",
            call. = FALSE)
    return(data.frame(speaker = character(0),
                      text = character(0),
                      datetime = as.POSIXct(character(0)),
                      recipient = character(0),
                      convo_num = integer(0),
                      stringsAsFactors = FALSE))
  }

  df <- map_dfr(msg_list, rbind) %>%
    filter(str_detect(speaker, "[A-Za-z]"))  %>% #keep only named contacts
    filter(!str_detect(speaker, "@")) %>%
    filter(!str_detect(speaker, ":"))

  return(df)

}