#' Word and message volume ratios per conversation
#'
#' For each conversation, compares how much each person writes: total words
#' and total messages, expressed as focal-speaker / other ratios.
#' \code{word_ratio > 1} (or \code{message_ratio > 1}) means the focal speaker
#' produces more text volume than the other person.
#'
#' @param sentences Message-level data frame (post
#'   \code{\link{prepare_sentences}}) with speaker, recipient, text, convo_num.
#' @param speaker_str Display name of the focal speaker.
#'
#' @return One row per conversation: word/message counts for both parties,
#'   \code{word_ratio}, \code{message_ratio}, and \code{other_recipient}.
#' @keywords internal
word_message_ratio <- function(sentences,
                            speaker_str) {

  # ---- 1. count volume per person ------------------------------------------
  # counted off the raw text column, not cleaned words - "\\S+" is just runs
  # of non-whitespace. this is deliberate: volume asymmetry should count
  # everything someone typed, stopwords included.
  data_ratio <- sentences %>%
    group_by(convo_num, speaker) %>%
    summarize(
      word_n        = sum(str_count(text, "\\S+")),
      message_count = n(),
      recipient     = first(na.omit(recipient)),
      .groups       = "drop_last"
    ) %>%
    filter(!is.na(recipient)) %>%
    group_by(convo_num) %>%
    # ---- 2. one row per conversation ---------------------------------------
    # n() == 2 needs both people to have sent something. ratios are focal over
    # other, so > 1 means the focal speaker writes more.
    filter(n() == 2) %>%
    summarize(
      speaker_words   = word_n[speaker == speaker_str],
      other_words     = word_n[speaker != speaker_str],
      speaker_messages = message_count[speaker == speaker_str],
      other_messages  = message_count[speaker != speaker_str],
      word_ratio      = speaker_words / other_words,
      message_ratio   = speaker_messages / other_messages,
      other_recipient = first(speaker[speaker != speaker_str]),
      .groups         = "drop_last"
    ) %>%
    arrange(desc(word_ratio))
  
  return(data_ratio)
}
