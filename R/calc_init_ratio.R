#' Conversation-initiation ratio per conversation
#'
#' For each conversation, counts how many exchanges each person started and
#' returns the ratio. An "exchange" is a burst of conversation separated from
#' the previous one by more than \code{sec_threshold} seconds of silence.
#' \code{init_ratio > 1} means the focal speaker initiates contact more often
#' than the other person.
#'
#' @param sentences Message-level data frame (post
#'   \code{\link{prepare_sentences}}) with speaker, recipient, datetime,
#'   convo_num.
#' @param sec_threshold Silence gap in seconds that marks the start of a new
#'   exchange. Default 172800 (48h).
#' @param speaker_str Display name of the focal speaker.
#'
#' @return One row per conversation: initiation counts for both parties,
#'   \code{init_ratio}, and \code{other_recipient}.
#' @keywords internal
calc_init_ratio <- function(sentences,
                            sec_threshold = 172800,
                            speaker_str) {



  # ---- 1. Segment messages into turns and exchanges -----------------------
  data_prep <- segment_turns(sentences, sec_threshold = sec_threshold)


  # ---- 2. Count initiations per person -------------------------------------
  # The initiator of an exchange is whoever sent its first turn. n() == 2
  # keeps only conversations where BOTH people initiated at least once — a
  # fully one-sided conversation has no defined ratio and is dropped.
  data_calc <- data_prep %>%
    group_by(convo_num, exchange_id) %>%
    summarize(speaker_init = first(speaker),
              .groups = "drop_last") %>%
    group_by(convo_num, speaker_init) %>%
    summarize(num_init = n(),
              .groups = "drop_last") %>%
    group_by(convo_num) %>%
    filter(n() == 2)
  
  # ---- 3. Compute the ratio -------------------------------------------------
  # init_ratio = speaker initiations / other's initiations; > 1 means the
  # focal speaker reaches out first more often.
  data_ratio <- data_calc %>%
    group_by(convo_num) %>%
    summarize(init_speaker = num_init[speaker_init == speaker_str],
              init_other = num_init[speaker_init != speaker_str],
              init_ratio = init_speaker / init_other,
              other_recipient = first(speaker_init[speaker_init != speaker_str]),
              .groups = "drop_last") %>%
    arrange(desc(init_ratio))
  
  return(data_ratio)
  
  
  
}