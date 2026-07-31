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



  # ---- 1. turns and exchanges ---------------------------------------------
  # valid_response_time is the column that matters here - response_time with
  # the over-threshold gaps already blanked out
  data_prep <- segment_turns(sentences, sec_threshold = sec_threshold)


  # ---- 2. response times per person ----------------------------------------
  # grouping by recipient, not speaker: the response_time attached to a turn
  # measures how fast whoever RECEIVED it wrote back. so the rows where
  # recipient == speaker_str are the focal speaker's own reply times. NaN
  # means they never replied within the threshold at all.
  data_sum <- data_prep %>%
    group_by(convo_num, recipient) %>%
    summarize(mean_rt = mean(valid_response_time, na.rm = TRUE),
              median_rt = median(valid_response_time, na.rm = TRUE),
              .groups = "drop_last") %>%
    ungroup() %>%
    mutate(mean_rt = ifelse(is.nan(mean_rt), NA, mean_rt),
           median_rt = ifelse(is.nan(median_rt), NA, median_rt)) %>%
    filter(!is.na(mean_rt))


  # ---- 3. one row per conversation -----------------------------------------
  # n() == 2 needs both people to have at least one real response time.
  # other/speaker, so > 1 means the focal speaker is the faster replier.
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

  
  # ---- 4. sort -------------------------------------------------------------
  # no else branch: anything other than median/mean leaves final_data
  # undefined. dbi() validates rt before calling, so nothing else gets here
  # through the normal path.
  if (arrange_by == "median") {
    final_data <- data_rt_ratio %>%
      arrange(desc(rt_ratio_median))
  } else if (arrange_by == "mean") {
    final_data <- data_rt_ratio %>%
      arrange(desc(rt_ratio_mean))
  }
  
  return(final_data)
  
    
}
