#' Plot one emotion dimension over time for a dyad
#'
#' Draws smoothed trajectories of a single emotion/sentiment dimension for
#' both members of a conversation, on a shared z-scored scale, colored by
#' speaker. Word-level emotion values come from joining cleaned words against
#' the \code{lookup_Jul25} lexical norms table.
#'
#' @param data Word-level data frame (output of \code{\link{clean_messages}})
#'   with \code{word_clean}, \code{speaker}, \code{recipient},
#'   \code{datetime}.
#' @param people Character vector of exactly two display names; only messages
#'   between these two people are plotted.
#' @param sentiment Name of one emotion column in the norms lookup
#'   (e.g. "emo_happiness", "emo_anxiety").
#'
#' @return A ggplot object.
#' @seealso \code{\link{plot_dyadic_emotion}} to plot several dimensions at
#'   once, \code{\link{plot_dyadic_emotion_grid}} for a faceted version.
#' @examples
#' messages <- read_imessages(system.file("extdata", package = "dbiR"),
#'                            speaker_name = "Shawn Wang")
#'
#' # Emotion norms are attached per word, so the text must be cleaned first.
#' words <- clean_messages(messages, wordcol = "text",
#'                         omit_stops = TRUE, lemmatize = TRUE)
#'
#' plot_convo(words,
#'            people = c("Shawn Wang", "Alice Test"),
#'            sentiment = "emo_happiness")
#'
#' # names(lookup_Jul25) lists every dimension available.
#' plot_convo(words,
#'            people = c("Shawn Wang", "Bob Chill"),
#'            sentiment = "emo_anxiety")
#' @export
plot_convo <- function(data, people, sentiment) {

  sent_label <- sentiment


  # both speaker AND recipient have to be in `people`, otherwise messages
  # either of them sent to someone else leak in. words missing from the norms
  # table join to NA, which geom_smooth drops on its own.
  data_prep <- data %>%
    filter(!is.na(word_clean)) %>%
    filter(speaker %in% people & recipient %in% people) %>%
    left_join(lookup_Jul25, by = c("word_clean" = "word"))





  # z-scoring puts the dimensions on a comparable scale, so two plots of
  # different sentiments can be read against each other. colors go in
  # `people` order.
  plot <- data_prep %>%
    mutate(sentiment_z = scale(.data[[sentiment]])) %>%
    ggplot(mapping = aes(x = datetime, y = sentiment_z, color = speaker)) +
    geom_smooth(se = FALSE) +
    scale_color_manual(values = setNames(c("forestgreen", "red"), people)) +
    labs(
      y = sent_label
    )

  return(plot)
}
