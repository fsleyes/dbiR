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





  # this one leans on the lag() inside segment_turns(): it keeps the final
  # unanswered turn attached to the exchange it closed, so the last row of
  # each exchange tells us who let it drop
  data_prep <- segment_turns(sentences, sec_threshold = sec_threshold)



  # last(recipient) is whoever got the final turn and didn't reply - they're
  # the one who ended it by going quiet. n() == 2 drops conversations where
  # only one person ever did that, since there's no ratio to take.
  data_calc <- data_prep %>%
    group_by(convo_num, exchange_id) %>%
    summarize(speaker_end = last(recipient),
              .groups = "drop_last") %>%
    group_by(convo_num, speaker_end) %>%
    summarize(num_end = n(),
              .groups = "drop_last") %>%
    group_by(convo_num) %>%
    filter(n() == 2)


  # speaker_end is the person who ended the exchange, so end_speaker counts
  # the ones the focal speaker let die and end_other counts the ones the other
  # person did. other/speaker, so higher = they drop conversations on you more
  # often than you drop them.
  data_ratio <- data_calc %>%
    group_by(convo_num) %>%
    summarize(end_speaker = num_end[speaker_end == speaker_str], #focal speaker went quiet
              end_other = num_end[speaker_end != speaker_str], #they went quiet (focal left on read)

              end_ratio = end_other / end_speaker, #higher = focal gets left on read more
              other_recipient = first(speaker_end[speaker_end != speaker_str]),
              .groups = "drop_last") %>%
    arrange(desc(end_ratio))


  return(data_ratio)

}
