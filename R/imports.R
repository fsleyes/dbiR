#' @keywords internal
"_PACKAGE"


# ---- imports -----------------------------------------------------------------
# nearly every function here is a dplyr pipeline, so the tidyverse namespaces
# get imported whole rather than tracking dozens of individual @importFrom
# tags. importing dplyr wholesale also settles the filter()/lag() collisions
# with stats in dplyr's favour, which is what every pipeline below assumes.
# the smaller dependencies are imported by name.

#' @import dplyr
#' @import tidyr
#' @import stringr
#' @import ggplot2
#' @import purrr
#' @importFrom stringi stri_enc_toutf8 stri_encode stri_replace_all_regex
#'   stri_isempty
#' @importFrom tibble tibble
#' @importFrom lubridate parse_date_time
#' @importFrom forcats fct_rev fct_relabel
#' @importFrom textstem lemmatize_words
#' @importFrom stats setNames median sd na.omit
NULL


# ---- NSE column names --------------------------------------------------------
# dplyr refers to columns as bare symbols and R CMD check can't tell those
# apart from undefined variables. listing them here kills the "no visible
# binding" notes. keeping the list explicit rather than blanket-suppressing
# means a real typo still shows up.
utils::globalVariables(c(
  # magrittr's "." placeholder
  ".",
  # raw parse (process_txt_2)
  "raw_text", "chunk_id", "reply_text", "full_content", "attach_text",
  "time", "text",
  # message-level columns
  "speaker", "recipient", "datetime", "convo_num", "num_messages",
  # word-level columns (clean_messages)
  "text_initialsplit", "word_clean", "replacement", "lemma", "is_stopword",
  "id_row_orig",
  # turn / exchange segmentation (calc_* helpers)
  "prev_gap", "new_turn", "turn_id", "turn_start", "turn_end",
  "message_count", "response_timestamp", "response_time", "exchange_id",
  "valid_response_time", "mean_rt", "median_rt",
  # ratio outputs
  "word_n", "speaker_words", "other_words", "speaker_messages",
  "other_messages", "word_ratio", "message_ratio", "other_recipient",
  "speaker_init", "num_init", "init_speaker", "init_other", "init_ratio",
  "speaker_end", "num_end", "end_speaker", "end_other",
  "end_ratio", "rt_ratio_mean", "rt_ratio_median", "rt_ratio_mean_log",
  "rt_speaker_mean", "rt_speaker_median", "rt_other_mean", "rt_other_median",
  # dbi transforms
  "dbi", "word_ratio_log", "message_ratio_log", "end_ratio_log",
  "init_ratio_log", "rt_ratio_mean_log", "rt_ratio_median_log",
  # plotting
  "sentiment_z", "emotion", "raw_value", "component", "value", "leaning",
  # bundled data
  "lookup_Jul25", "replacements_25", "Temple_stops25"
))
