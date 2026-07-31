#' Plot several emotion dimensions for a dyad
#'
#' Convenience wrapper around \code{\link{plot_convo}}: builds one plot per
#' requested emotion dimension and prints each in turn (printing is what makes
#' the plots render inside knitr chunks and loops).
#'
#' @param data Word-level data frame (output of \code{\link{clean_messages}}).
#' @param people Character vector of exactly two display names.
#' @param sentiment Character vector of emotion column names to plot, one plot
#'   each (e.g. c("emo_happiness", "emo_anxiety")).
#'
#' @return Called for its side effect (printing plots); no useful return
#'   value.
#' @seealso \code{\link{plot_dyadic_emotion_grid}}, which returns a single
#'   faceted ggplot object instead of printing several.
#' @examples
#' messages <- read_imessages(system.file("extdata", package = "dbiR"),
#'                            speaker_name = "Shawn Wang")
#' words <- clean_messages(messages, wordcol = "text",
#'                         omit_stops = TRUE, lemmatize = TRUE)
#'
#' # Prints one full-size plot per dimension.
#' plot_dyadic_emotion(words,
#'                     people = c("Shawn Wang", "Alice Test"),
#'                     sentiment = c("emo_happiness", "emo_anxiety"))
#' @export
plot_dyadic_emotion <- function(data, people, sentiment) {



  # Build one ggplot per sentiment...
  plots <- sentiment %>%
    map(~ plot_convo(data = data, people = people, sentiment = .x))

  # ...then print explicitly: inside loops/functions ggplots are not
  # auto-printed, so without this the plots would silently not appear.
  for (i in seq_along(plots)) {

    print(plots[[i]])
  }
  
  
  
  
  
}