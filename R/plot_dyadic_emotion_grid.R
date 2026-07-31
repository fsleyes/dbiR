#' Plot several emotion dimensions for a dyad as a small-multiple grid
#'
#' A presentation-oriented alternative to \code{\link{plot_dyadic_emotion}}.
#' Where that function prints one full-size plot per emotion, this draws every
#' requested dimension as a facet of a \emph{single} figure on shared axes, so
#' the dyad's emotional trajectories can be compared at a glance rather than by
#' scrolling between plots.
#'
#' Differences from \code{\link{plot_dyadic_emotion}}, all deliberate:
#' \itemize{
#'   \item Returns one ggplot object instead of printing N of them, so the
#'     result can be further modified, saved with \code{ggsave}, or arranged.
#'   \item Each emotion is z-scored \emph{within} its own facet, which is what
#'     makes a shared y-axis meaningful across dimensions.
#'   \item Speakers are colored blue/orange rather than green/red. Green/red
#'     is the single least colorblind-safe pair available, and it reads as a
#'     value judgement (good speaker / bad speaker) on what is really just an
#'     identity distinction.
#'   \item Facet strips show "Happiness", not "emo_happiness"; the y-axis is
#'     labelled in words; a hairline at z = 0 marks each speaker's own average
#'     so deviations are readable rather than merely visible.
#' }
#'
#' @param data Word-level data frame (output of \code{\link{clean_messages}})
#'   with \code{word_clean}, \code{speaker}, \code{recipient},
#'   \code{datetime}.
#' @param people Character vector of exactly two display names; only messages
#'   exchanged between these two are plotted. Color is assigned in this order,
#'   so passing the same vector always yields the same colors.
#' @param sentiment Character vector of emotion column names in the norms
#'   lookup (e.g. c("emo_happiness", "emo_anxiety")).
#' @param se Draw the smoother's confidence ribbon. Defaults to \code{FALSE},
#'   matching the rest of the package; \code{TRUE} is worth it when you need to
#'   see how much data is actually behind a trend.
#' @param ncol Number of facet columns. Defaults to 3, or fewer when fewer
#'   emotions were requested.
#' @param lookup Norms table to join against. Defaults to \code{lookup_Jul25}.
#' @param subtitle Optional subtitle. Defaults to \code{NULL} (none).
#'
#' @return A ggplot object.
#' @seealso \code{\link{plot_convo}} for a single emotion,
#'   \code{\link{plot_dyadic_emotion}} for the original one-plot-per-emotion
#'   version.
#' @examples
#' messages <- read_imessages(system.file("extdata", package = "dbiR"),
#'                            speaker_name = "Shawn Wang")
#' words <- clean_messages(messages, wordcol = "text",
#'                         omit_stops = TRUE, lemmatize = TRUE)
#'
#' # Every dimension as a facet of one figure, on a shared z-scored axis.
#' plot_dyadic_emotion_grid(words,
#'                          people = c("Shawn Wang", "Alice Test"),
#'                          sentiment = c("emo_happiness", "emo_anxiety",
#'                                        "emo_sadness"))
#'
#' # Returns a ggplot, so it can be modified or saved.
#' p <- plot_dyadic_emotion_grid(words,
#'                               people = c("Shawn Wang", "Bob Chill"),
#'                               sentiment = c("emo_happiness", "emo_anger"),
#'                               ncol = 2,
#'                               subtitle = "Bob thread")
#' class(p)
#' @export
plot_dyadic_emotion_grid <- function(data,
                                     people,
                                     sentiment,
                                     se = FALSE,
                                     ncol = 3,
                                     lookup = lookup_Jul25,
                                     subtitle = NULL) {

  # ---- 0. validate ---------------------------------------------------------
  # cheap checks that turn three different downstream failures (empty plot,
  # recycled color vector, "object not found") into one readable message
  if (length(people) != 2) {
    stop("`people` must contain exactly two names; got ", length(people), ".")
  }

  missing_cols <- setdiff(sentiment, names(lookup))
  if (length(missing_cols) > 0) {
    stop("`sentiment` columns not found in lookup: ",
         paste(missing_cols, collapse = ", "))
  }

  # ---- 1. narrow to the pair, attach norms ---------------------------------
  # both speaker AND recipient have to be in `people`, otherwise messages
  # either of them sent to someone else leak in
  data_prep <- data %>%
    filter(!is.na(word_clean)) %>%
    filter(speaker %in% people & recipient %in% people) %>%
    left_join(lookup, by = c("word_clean" = "word"))

  if (nrow(data_prep) == 0) {
    stop("No messages found between ", people[1], " and ", people[2], ".")
  }

  # ---- 2. long format, z-score within each emotion -------------------------
  # scaling per emotion is what makes the shared y-axis legible at all - the
  # raw norm values sit on different ranges per dimension, so the facets
  # couldn't share a scale otherwise. words that missed the join are dropped
  # here rather than handed to the smoother.
  data_long <- data_prep %>%
    pivot_longer(cols = all_of(sentiment),
                 names_to = "emotion",
                 values_to = "raw_value") %>%
    filter(!is.na(raw_value)) %>%
    group_by(emotion) %>%
    mutate(sentiment_z = as.numeric(scale(raw_value))) %>%
    ungroup() %>%
    # facets follow the order the caller listed the emotions in, not
    # alphabetical - that order usually means something
    mutate(emotion = factor(emotion, levels = sentiment),
           emotion = fct_relabel(emotion, clean_emotion_label),
           # same for speakers, so colors stay put across calls
           speaker = factor(speaker, levels = people))

  # ---- 3. color ------------------------------------------------------------
  # identity palette, two hues that stay apart under the common forms of
  # color vision deficiency (green/red does not)
  speaker_colors <- setNames(c("#2a78d6", "#eb6834"), people)

  # ---- 4. plot -------------------------------------------------------------
  plot <- data_long %>%
    ggplot(mapping = aes(x = datetime, y = sentiment_z, color = speaker)) +
    # z = 0 is each speaker's own average for that emotion, so this line is
    # what the trends should be read against
    geom_hline(yintercept = 0,
               color = "#c3c2b7",
               linewidth = 0.3,
               linetype = "dashed") +
    geom_smooth(aes(fill = speaker),
                se = se,
                linewidth = 0.9,
                alpha = 0.12) +
    facet_wrap(~ emotion, ncol = min(ncol, length(sentiment))) +
    scale_color_manual(values = speaker_colors) +
    scale_fill_manual(values = speaker_colors, guide = "none") +
    # dates not datetimes - threads span months, so time of day is noise here
    scale_x_datetime(date_labels = "%b %Y") +
    # few round ticks. the shape of the trend is the point, not the third
    # decimal of a z-score.
    scale_y_continuous(n.breaks = 5) +
    labs(
      subtitle = subtitle,
      x = NULL,
      y = "Relative emotion (z-scored within dimension)",
      color = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      # keep the chrome light so the data is the only thing with weight.
      # vertical gridlines go because trends here are read left to right.
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "#e1e0d9", linewidth = 0.3),
      panel.spacing = unit(1.1, "lines"),
      strip.text = element_text(face = "bold", size = 10,
                                color = "#0b0b0b", hjust = 0,
                                margin = margin(b = 4)),
      plot.subtitle = element_text(size = 10, color = "#52514e",
                                   margin = margin(b = 10)),
      plot.title.position = "plot",
      axis.title.y = element_text(size = 9, color = "#52514e"),
      axis.text = element_text(size = 8, color = "#898781"),
      # legend on top reads as a key to the whole figure. on the right it
      # tends to look like it belongs to the last facet.
      legend.position = "top",
      legend.justification = "left",
      legend.text = element_text(size = 10, color = "#0b0b0b")
    )

  return(plot)
}


#' Turn an emotion column name into a display label
#'
#' Strips the \code{emo_} / \code{lex_} prefix the norms table uses and
#' title-cases what is left, so facet strips read "Happiness" instead of
#' "emo_happiness". Unprefixed names pass through unchanged.
#'
#' @param x Character vector of column names.
#' @return Character vector of display labels.
#' @keywords internal
clean_emotion_label <- function(x) {
  x %>%
    str_remove("^(emo|lex)_") %>%
    str_replace_all("_", " ") %>%
    str_to_title()
}
