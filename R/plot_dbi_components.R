#' Plot the five DBI components as a diverging profile per partner
#'
#' The ranked table \code{\link{dbi}} returns tells you \emph{who} you are most
#' down bad for; it does not tell you \emph{why}. Two partners can land on the
#' same composite score for completely different reasons -- one because you
#' always text first, another because you always write more. This draws each
#' partner's five component scores as bars diverging from zero, so the shape of
#' each relationship is readable at a glance.
#'
#' Zero is "perfectly balanced": \code{\link{dbi}} scales the log ratios by SD
#' \emph{without} centering, so 0 keeps its meaning rather than drifting to
#' "average for this corpus". All five components are constructed to point the
#' same way -- \code{end_ratio} and \code{rt_ratio} are deliberately inverted
#' (\code{other / speaker}) upstream so that for every component, positive means
#' the focal speaker is the one leaning in.
#'
#' Components are ordered by their default CFA weight (initiation heaviest,
#' word count lightest), so the bars that move the score most sit at the top of
#' each panel. If you passed custom \code{weights} to \code{\link{dbi}}, that
#' ordering no longer reflects your weighting -- the bars are still correct,
#' but read the order as informational only.
#'
#' @param dbi_df Output of \code{\link{dbi}}.
#' @param n Number of partners to show, taken from the top of the DBI ranking.
#'   Defaults to 12. Use \code{Inf} for all.
#' @param rt Which response-time column to plot, "median" (default) or "mean".
#'   Match this to the \code{rt} you passed to \code{\link{dbi}}, otherwise the
#'   panel shows a component that did not feed the score.
#' @param speaker_label,partner_label Words for the two directions of the axis.
#'   Default to "You" and "They".
#' @param ncol Number of facet columns. Defaults to 4.
#' @param subtitle Optional subtitle. Defaults to \code{NULL} (none).
#'
#' @return A ggplot object.
#' @seealso \code{\link{dbi}} for the index itself.
#' @examples
#' messages <- read_imessages(system.file("extdata", package = "dbiR"),
#'                            speaker_name = "Shawn Wang")
#' scored <- dbi(messages, speaker_str = "Shawn Wang")
#'
#' # Each partner's five components as bars diverging from "perfectly balanced".
#' plot_dbi_components(scored)
#'
#' # Fewer partners, custom labels, and a narrower grid.
#' plot_dbi_components(scored,
#'                     n = 2,
#'                     ncol = 2,
#'                     speaker_label = "Shawn",
#'                     partner_label = "Them")
#' @export
plot_dbi_components <- function(dbi_df,
                                n = 12,
                                rt = "median",
                                speaker_label = "You",
                                partner_label = "They",
                                ncol = 4,
                                subtitle = NULL) {


  if (!rt %in% c("median", "mean")) {
    stop("`rt` must be \"median\" or \"mean\".")
  }

  rt_col <- paste0("rt_ratio_", rt, "_log_z")

  # the z-scored logs are what actually get weighted and summed into dbi, so
  # they're the columns that explain the score. the raw ratios would just sit
  # next to it.
  component_cols <- c("init_ratio_log_z",
                      rt_col,
                      "message_ratio_log_z",
                      "end_ratio_log_z",
                      "word_ratio_log_z")

  required <- c("other_recipient", "dbi", component_cols)
  missing_cols <- setdiff(required, names(dbi_df))
  if (length(missing_cols) > 0) {
    stop("`dbi_df` is missing expected columns: ",
         paste(missing_cols, collapse = ", "),
         ". Was it produced by dbi()",
         if (rt == "median") " with rt = \"median\"" else " with rt = \"mean\"",
         "?")
  }


  # dbi() is keyed on (convo_num, other_recipient), so someone can turn up on
  # more than one row. sort first, then take the first occurrence, so each
  # person appears once at their highest score.
  dbi_ranked <- dbi_df %>%
    filter(!is.na(other_recipient), !is.na(dbi)) %>%
    arrange(desc(dbi)) %>%
    distinct(other_recipient, .keep_all = TRUE) %>%
    slice_head(n = n)

  if (nrow(dbi_ranked) == 0) {
    stop("No scored conversations to plot.")
  }


  # labels are phrased about the focal speaker so a positive bar can be read
  # straight off the strip without going back to the docs
  component_labels <- c(
    "init_ratio_log_z"    = "Starts conversations",
    "message_ratio_log_z" = "Sends more messages",
    "end_ratio_log_z"     = "Gets left on read",
    "word_ratio_log_z"    = "Writes more words"
  )
  component_labels[rt_col] <- "Replies faster"

  plot_data <- dbi_ranked %>%
    pivot_longer(cols = all_of(component_cols),
                 names_to = "component",
                 values_to = "value") %>%
    filter(!is.na(value)) %>%
    mutate(
      # facets in rank order, not alphabetical - the ranking is the point
      other_recipient = factor(other_recipient, levels = dbi_ranked$other_recipient),
      # components in weight order. fct_rev because horizontal bars build from
      # the bottom up and the heaviest ones should be at the top.
      component = factor(component, levels = component_cols),
      component = fct_rev(fct_relabel(component, ~ component_labels[.x])),
      # sign picks the fill color. exact ties at 0 are vanishingly rare but
      # would fall through to NA, so they land on `partner`.
      leaning = if_else(value > 0, "speaker", "partner")
    )


  # diverging rather than categorical - these are two ends of one axis, so a
  # warm/cool pair meeting at the zero line. warm marks the direction the
  # index is actually about.
  lean_colors <- c("speaker" = "#e34948", "partner" = "#2a78d6")
  lean_labels <- c("speaker" = paste(speaker_label, "lean in more"),
                   "partner" = paste(partner_label, "lean in more"))


  plot <- plot_data %>%
    ggplot(mapping = aes(x = value, y = component, fill = leaning)) +
    geom_col(width = 0.65) +
    # drawn after the bars so it sits on top - it's the reference the whole
    # chart gets read against

    geom_vline(xintercept = 0, color = "#52514e", linewidth = 0.4) +
    facet_wrap(~ other_recipient, ncol = ncol) +
    scale_fill_manual(values = lean_colors,
                      labels = lean_labels,
                      breaks = c("partner", "speaker")) +
    scale_x_continuous(n.breaks = 5) +
    labs(
      subtitle = subtitle,
      # \u2190 / \u2192 are just left and right arrows - escaped because R
      # package source has to be ASCII
      x = paste0("\u2190 ", partner_label, " lean in more",
                 "     |     ",
                 speaker_label, " lean in more \u2192"),
      y = NULL,
      fill = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      # bars run horizontally, so vertical gridlines are the ones that help
      # judge magnitude. horizontal ones would just box the bars in.
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "#e1e0d9", linewidth = 0.3),
      panel.spacing.x = unit(1.4, "lines"),
      panel.spacing.y = unit(1.1, "lines"),
      strip.text = element_text(face = "bold", size = 9.5,
                                color = "#0b0b0b", hjust = 0,
                                margin = margin(b = 4)),
      plot.subtitle = element_text(size = 10, color = "#52514e",
                                   margin = margin(b = 10)),
      plot.title.position = "plot",
      axis.title.x = element_text(size = 9, color = "#52514e",
                                  margin = margin(t = 8)),
      axis.text.y = element_text(size = 8.5, color = "#52514e", hjust = 1),
      axis.text.x = element_text(size = 8, color = "#898781"),
      legend.position = "top",
      legend.justification = "left",
      legend.text = element_text(size = 10, color = "#0b0b0b")
    )

  return(plot)
}
