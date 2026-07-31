#' @keywords internal
"_PACKAGE"


# ---- Package-level imports ---------------------------------------------------
# This package is written in tidyverse idiom: nearly every function is a dplyr
# pipeline, so importing those namespaces wholesale is both more readable and
# more maintainable than tracking dozens of individual @importFrom tags.
# Importing dplyr as a whole also resolves the classic filter()/lag() collisions
# with stats in dplyr's favour, which is the behaviour every pipeline here
# assumes. The narrower dependencies are imported by name.

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


# ---- Non-standard evaluation ------------------------------------------------
# dplyr pipelines refer to columns as bare symbols, which R CMD check cannot
# distinguish from undefined global variables. Declaring them here silences the
# "no visible binding for global variable" notes without obscuring real typos
# elsewhere in the package.
utils::globalVariables(c(
  # magrittr's "." placeholder, used in a few pipelines
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
