#' Left-on-read (conversation-ending) ratio per conversation
#'
#' For each conversation, counts how many exchanges each person ended by going
#' silent — i.e. received the exchange's final turn and never replied within
#' \code{sec_threshold}. The ratio is other/speaker, so \code{end_ratio > 1}
#' means the OTHER person ended more exchanges, i.e. the focal speaker was
#' left on read more often — the direction the positive end_ratio weight in
#' \code{\link{dbi}} assumes (more asymmetry = more down bad).
#'
#' @param sentences Message-level data frame (post
#'   \code{\link{prepare_sentences}}) with speaker, recipient, datetime,
#'   convo_num.
#' @param sec_threshold Silence gap in seconds that ends an exchange.
#'   Default 172800 (48h).
#' @param speaker_str Display name of the focal speaker.
#'
#' @return One row per conversation: end counts for both parties,
#'   \code{end_ratio}, and \code{other_recipient}.
#' @keywords internal
calc_end_ratio <- function(sentences,
                            sec_threshold = 172800,
                            speaker_str) {




  # ---- 1. Segment messages into turns and exchanges -----------------------
  # The lag() inside segment_turns() is what this function depends on: it keeps
  # the final unanswered turn attached to the exchange it closed, so the last
  # row of each exchange identifies who let the conversation drop.
  data_prep <- segment_turns(sentences, sec_threshold = sec_threshold)


  # ---- 2. Identify who ended each exchange ---------------------------------
  # last(recipient) is the person who RECEIVED the exchange's final turn and
  # chose not to reply — i.e. the one who ended the exchange by going silent.
  # n() == 2 keeps only conversations where both people ended at least one
  # exchange (otherwise no ratio is defined).
  data_calc <- data_prep %>%
    group_by(convo_num, exchange_id) %>%
    summarize(speaker_end = last(recipient),
              .groups = "drop_last") %>%
    group_by(convo_num, speaker_end) %>%
    summarize(num_end = n(),
              .groups = "drop_last") %>%
    group_by(convo_num) %>%
    filter(n() == 2)
  
  # ---- 3. Compute the ratio -------------------------------------------------
  # speaker_end names the person who ENDED each exchange by going silent, so
  # end_speaker counts exchanges the focal speaker ended, and end_other counts
  # exchanges the other person ended (= times the focal speaker was left on
  # read). Ratio is other/speaker: higher = the other person ends more
  # conversations = focal speaker gets left on read more.
  data_ratio <- data_calc %>%
    group_by(convo_num) %>%
    summarize(end_speaker = num_end[speaker_end == speaker_str], #exchanges ENDED BY the focal speaker (focal went silent)
              end_other = num_end[speaker_end != speaker_str], #exchanges ENDED BY the other person (focal was left on read)

              end_ratio = end_other / end_speaker, #higher means the other person ended more exchanges (focal got left on read more)
              other_recipient = first(speaker_end[speaker_end != speaker_str]),
              .groups = "drop_last") %>%
    arrange(desc(end_ratio))
  
  
  return(data_ratio)
  
}