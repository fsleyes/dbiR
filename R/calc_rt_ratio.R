#' Response-time ratio per conversation
#'
#' For each conversation, compares how long the other person takes to respond
#' to the focal speaker versus how long the focal speaker takes to respond to
#' them. \code{rt_ratio > 1} means the other person is slower to reply than the
#' focal speaker — i.e. the focal speaker is the more eager responder.
#'
#' Messages are first collapsed into turns (consecutive messages by the same
#' sender), and a response time is the gap between the end of one turn and the
#' start of the other person's next turn. Gaps longer than
#' \code{sec_threshold} are treated as conversation breaks, not responses.
#'
#' @param sentences Message-level data frame (post
#'   \code{\link{prepare_sentences}}) with speaker, recipient, datetime,
#'   convo_num.
#' @param sec_threshold Max gap in seconds that still counts as a response;
#'   larger gaps split exchanges. Default 172800 (48h).
#' @param speaker_str Display name of the focal speaker.
#' @param arrange_by Sort output by "median" (default) or "mean" ratio.
#'
#' @return One row per conversation: mean/median response times for both
#'   parties, their ratios (and log ratio), and \code{other_recipient}.
#' @keywords internal
calc_rt_ratio <- function(sentences,
                          sec_threshold = 172800,
                          speaker_str,
                          arrange_by = "median") {



  # ---- 1. Segment messages into turns and exchanges -----------------------
  # valid_response_time (response_time with over-threshold gaps blanked out) is
  # the column this function is built on.
  data_prep <- segment_turns(sentences, sec_threshold = sec_threshold)


  # ---- 2. Summarize response times per responder ---------------------------
  # Grouping by recipient: the response_time on a turn measures how fast the
  # RECIPIENT of that turn replied, so recipient == speaker_str rows hold the
  # focal speaker's own response times. NaN (no valid responses at all)
  # becomes NA and those rows are dropped.
  data_sum <- data_prep %>%
    group_by(convo_num, recipient) %>%
    summarize(mean_rt = mean(valid_response_time, na.rm = TRUE),
              median_rt = median(valid_response_time, na.rm = TRUE),
              .groups = "drop_last") %>%
    ungroup() %>%
    mutate(mean_rt = ifelse(is.nan(mean_rt), NA, mean_rt),
           median_rt = ifelse(is.nan(median_rt), NA, median_rt)) %>%
    filter(!is.na(mean_rt))


  # ---- 3. Pivot to one row per conversation and compute ratios ------------
  # n() == 2 requires BOTH parties to have at least one valid response time;
  # conversations where only one side ever responded are excluded (no ratio
  # is defined). rt_ratio = other / speaker, so > 1 means the focal speaker
  # replies faster than the other person.
  data_rt_ratio <- data_sum %>%
    group_by(convo_num) %>%
    filter(!is.na(recipient)) %>%
    filter(n() == 2) %>%
    summarize(
      rt_speaker_mean = mean_rt[recipient == speaker_str],
      rt_speaker_median = median_rt[recipient == speaker_str],
      rt_other_mean = mean_rt[recipient != speaker_str],
      rt_other_median = median_rt[recipient != speaker_str],
      rt_ratio_mean = rt_other_mean/rt_speaker_mean,
      rt_ratio_mean_log = log(rt_other_mean) - log(rt_speaker_mean),
      rt_ratio_median = rt_other_median / rt_speaker_median,
      other_recipient = first(recipient[recipient != speaker_str]),
      .groups = "drop_last"
    ) %>%
    relocate(convo_num, other_recipient, rt_ratio_mean, rt_ratio_median)

  
  # ---- 4. Sort by the requested summary statistic -------------------------
  # NOTE: no else fallback — an unrecognized arrange_by value would leave
  # final_data undefined. dbi() validates rt upstream, so only "median"/"mean"
  # reach here in the normal pipeline.
  if (arrange_by == "median") {
    final_data <- data_rt_ratio %>%
      arrange(desc(rt_ratio_median))
  } else if (arrange_by == "mean") {
    final_data <- data_rt_ratio %>%
      arrange(desc(rt_ratio_mean))
  }
  
  return(final_data)
  
    
}
