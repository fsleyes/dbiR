#' List one-on-one iMessage thread files in a directory
#'
#' Scans \code{directory} for iMessage exports (the \code{.txt} files produced by
#' imessage-exporter) and returns only the paths to one-on-one (dyadic)
#' conversations, excluding group chats.
#'
#' Group chats are detected from the filename: imessage-exporter names each export
#' after the participants' handles, so a group thread contains multiple
#' comma-separated handles (e.g. "+1..., +1....txt" or "..., and 7 others.txt"),
#' whereas a one-on-one thread is a single handle with no comma. This is a cheap
#' pre-filter; the authoritative dyad check happens later in
#' \code{\link{message_to_list}} on the parsed content.
#'
#' @param directory Path to the folder containing the exported \code{.txt} files.
#'
#' @return A character vector of full file paths to dyadic thread exports.
#' @seealso \code{\link{message_to_list}}, \code{\link{read_imessages}}
#' @keywords internal
list_threads <- function(directory) {

  list.files(directory, pattern = "\\.txt$", full.names = TRUE) %>%
    # group chats are named with multiple comma-separated handles
    # (e.g. "+1..., +1....txt" or "..., and 7 others.txt");
    # a 1-on-1 chat is a single handle with no comma
    str_subset(",", negate = TRUE) %>%
    str_subset("\\+")
}