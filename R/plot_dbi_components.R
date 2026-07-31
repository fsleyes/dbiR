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

  # ---- 0. Validate up front -------------------------------------------------
  if (!rt %in% c("median", "mean")) {
    stop("`rt` must be \"median\" or \"mean\".")
  }

  rt_col <- paste0("rt_ratio_", rt, "_log_z")

  # The z-scored logs are what actually get weighted and summed into `dbi`,
  # so those are the columns worth showing -- they explain the score rather
  # than merely accompanying it.
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

  # ---- 1. One row per partner, top n by DBI --------------------------------
  # dbi() is keyed by (convo_num, other_recipient), so the same person can in
  # principle appear on more than one row. Sorting by dbi first and keeping the
  # first occurrence means we show each partner once, at their strongest score.
  dbi_ranked <- dbi_df %>%
    filter(!is.na(other_recipient), !is.na(dbi)) %>%
    arrange(desc(dbi)) %>%
    distinct(other_recipient, .keep_all = TRUE) %>%
    slice_head(n = n)

  if (nrow(dbi_ranked) == 0) {
    stop("No scored conversations to plot.")
  }

  # ---- 2. Long format, one row per partner x component ---------------------
  # Labels are plain English about the focal speaker so a positive bar can be
  # read straight off the strip without consulting the docs.
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
      # Facets in DBI rank order, not alphabetical -- the ranking is the point.
      other_recipient = factor(other_recipient, levels = dbi_ranked$other_recipient),
      # Components in weight order. fct_rev because coord_flip-style horizontal
      # bars build from the bottom up, and we want the heaviest at the top.
      component = factor(component, levels = component_cols),
      component = fct_rev(fct_relabel(component, ~ component_labels[.x])),
      # Sign drives the diverging fill. Ties at exactly 0 are vanishingly rare
      # but would otherwise fall through to NA, so they resolve to `partner`.
      leaning = if_else(value > 0, "speaker", "partner")
    )

  # ---- 3. Color -------------------------------------------------------------
  # Diverging, not categorical: the two colors are poles of one axis, so they
  # are a warm/cool pair meeting at a neutral zero line. Warm marks the
  # direction the index is actually about (the focal speaker leaning in).
  lean_colors <- c("speaker" = "#e34948", "partner" = "#2a78d6")
  lean_labels <- c("speaker" = paste(speaker_label, "lean in more"),
                   "partner" = paste(partner_label, "lean in more"))

  # ---- 4. Build the plot ----------------------------------------------------
  plot <- plot_data %>%
    ggplot(mapping = aes(x = value, y = component, fill = leaning)) +
    geom_col(width = 0.65) +
    # The balance line is the reference the whole chart is read against, so it
    # sits above the fill rather than under it.
    geom_vline(xintercept = 0, color = "#52514e", linewidth = 0.4) +
    facet_wrap(~ other_recipient, ncol = ncol) +
    scale_fill_manual(values = lean_colors,
                      labels = lean_labels,
                      breaks = c("partner", "speaker")) +
    scale_x_continuous(n.breaks = 5) +
    labs(
      subtitle = subtitle,
      # \u2190 / \u2192 are left/right arrows, escaped so the source stays
      # ASCII-only (a portability requirement for R packages).
      x = paste0("\u2190 ", partner_label, " lean in more",
                 "     |     ",
                 speaker_label, " lean in more \u2192"),
      y = NULL,
      fill = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      # Bars run horizontally, so only vertical gridlines help read magnitude;
      # horizontal ones would just box in the bars.
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
