#' Read a directory of iMessage exports into one tidy data frame
#'
#' End-to-end ingestion wrapper and the package's main entry point for loading
#' data. Discovers dyadic thread files, parses each export, and binds them into a
#' single message-level data frame by chaining \code{\link{list_threads}} ->
#' \code{\link{message_to_list}} -> \code{\link{list_to_df}}.
#'
#' @param directory_name Path to the folder of exported \code{.txt} files.
#' @param speaker_name Display name to substitute for the literal "Me" (the
#'   account owner).
#' @param remove_attach Logical; if TRUE, drop attachment placeholder lines.
#'   Defaults to TRUE.
#'
#' @return A data frame with one row per message: speaker, text, datetime,
#'   recipient, and convo_num.
#' @seealso \code{\link{list_threads}}, \code{\link{message_to_list}},
#'   \code{\link{list_to_df}}
#' @examples
#' # The package ships a small synthetic corpus of four dyadic threads.
#' corpus <- system.file("extdata", package = "dbiR")
#'
#' messages <- read_imessages(corpus, speaker_name = "Shawn Wang")
#' head(messages)
#'
#' # Group chats, one-sided threads and unsaved contacts are filtered out,
#' # leaving four conversations.
#' length(unique(messages$convo_num))
#'
#' \dontrun{
#' # On your own data, point it at an imessage-exporter output folder:
#' messages <- read_imessages("~/imessage_export", speaker_name = "Your Name")
#' }
#' @export
read_imessages <- function(directory_name, speaker_name, remove_attach = TRUE) {
  
  thread_list <- list_threads(directory = directory_name)
  message_list <- message_to_list(threads = thread_list,
                                  speaker = speaker_name,
                                  remove_attach = remove_attach)
  message_df <- list_to_df(msg_list = message_list)
  
  return(message_df)
  
  
  
}