#' Tokenize and clean message text into one row per word
#'
#' Content-analysis preprocessor: explodes a message-level data frame into one
#' row per word and produces a normalized \code{word_clean} column suitable for
#' joining against lexical norm lookups (emotion/sentiment dictionaries).
#' Cleaning steps: UTF-8 normalization, lowercasing, apostrophe
#' standardization, punctuation stripping (punctuation acts as a word
#' separator, so hyphenates/slashes split into separate words), slang/contraction
#' replacement via the bundled \code{replacements_25} table, then optional
#' lemmatization and stopword removal.
#'
#' Rows are never dropped — words removed by cleaning or stopword filtering
#' become \code{NA} in \code{word_clean}, and \code{id_row_orig} always maps a
#' word back to its original message. This preserves message boundaries for
#' downstream regrouping.
#'
#' The two optional stages belong to different analyses, and the right setting
#' is opposite in each:
#' \itemize{
#'   \item \strong{Emotion / lexical analysis: both TRUE.} Stopwords carry no
#'     emotional signal and only dilute the averages, and lemmatizing lets
#'     "running" and "ran" both match "run" in the norms table.
#'   \item \strong{Behavioral metrics (\code{\link{dbi}}): both FALSE}, the
#'     defaults. \code{dbi()} reads the raw text column and does not need this
#'     function at all, but if cleaned output is fed in with
#'     \code{omit_stops = TRUE}, short messages made entirely of stopwords
#'     ("ok", "yeah") clean to nothing and vanish from the data. That shifts
#'     turn boundaries as well as counts, so response times, initiations and
#'     left-on-read tallies all move — silently.
#' }
#'
#' @param dat Data frame containing a text column (e.g. output of
#'   \code{\link{read_imessages}}).
#' @param wordcol Name of the text column, as a string (e.g. "text").
#' @param omit_stops Logical; if TRUE, words in the Temple stopword list are
#'   replaced with NA. Default FALSE.
#' @param lemmatize Logical; if TRUE, words are reduced to their lemma via
#'   \code{textstem::lemmatize_words}. Default FALSE.
#'
#' @return The input data frame exploded to one row per word, with added
#'   columns \code{id_row_orig} (original message id), \code{text_initialsplit}
#'   (pre-cleaning token), and \code{word_clean} (cleaned word, or NA where
#'   cleaning or stopword removal emptied it).
#' @examples
#' messages <- read_imessages(system.file("extdata", package = "dbiR"),
#'                            speaker_name = "Shawn Wang")
#'
#' # Defaults keep every word, which is what the behavioral metrics need.
#' words <- clean_messages(messages, wordcol = "text")
#' head(words[, c("speaker", "text_initialsplit", "word_clean")])
#'
#' # For lexical or emotion analysis, lemmatize and drop stopwords. Removed
#' # words become NA rather than disappearing, so message boundaries survive.
#' content <- clean_messages(messages, wordcol = "text",
#'                           omit_stops = TRUE, lemmatize = TRUE)
#' head(content$word_clean, 20)
#'
#' # Cleaned words join straight onto the bundled norms table.
#' scored <- merge(content, lookup_Jul25,
#'                 by.x = "word_clean", by.y = "word")
#' mean(scored$emo_happiness, na.rm = TRUE)
#' @export
clean_messages <- function(dat, wordcol, omit_stops = FALSE, lemmatize = FALSE) {

  # Input validation
  #need to tell R what column in your dataframe is the text in quotes 'wordcol', this error message checks
  if (!wordcol %in% names(dat)) {
    stop(paste("Column", wordcol, "not found in dataframe"))
  }

  #load stopword and replacement lists from github

  # load("~/Documents/Misc/shawn_text/text_cleaning/replacements_25.rda")
  # load("~/Documents/Misc/shawn_text/text_cleaning/Temple_stops25.rda")




  # id_row_orig has to be assigned here, before the split below, or there's no
  # way to get words back to the message they came from. tryCatch because
  # strict UTF-8 validation occasionally chokes on a weird string.
  dat_prep <- dat %>% dplyr::mutate(id_row_orig = factor(seq_len(nrow(dat))),
                                    text_initialsplit = tryCatch(stringi::stri_enc_toutf8(as.character(.[[wordcol]]),
                                                                                          is_unknown_8bit = TRUE,
                                                                                          validate = TRUE), error = function(e) stringi::stri_encode(as.character(.[[wordcol]]), to = "UTF-8")
                                    ) %>% tolower())


  # iOS uses curly quotes, keyboards use straight ones, and the replacement
  # table below is keyed on one of them. Normalize to ASCII ' so contractions
  # actually match.
  dat_prep <- dat_prep %>% dplyr::mutate(text_initialsplit = ifelse(
    is.na(text_initialsplit), NA_character_, stringi::stri_replace_all_regex(
      text_initialsplit,
      "[\u2018\u2019\u02BC\u201B\uFF07\u0092\u0091\u0060\u00B4\u2032\u2035]",
      "'"
    )
  )
  )


  # whitespace only, so contractions stay in one piece. empty tokens become NA
  # rather than getting dropped, which keeps the row and its id_row_orig.
  dat_prep <- dat_prep %>% tidyr::separate_rows(text_initialsplit, sep = "[[:space:]]+") %>%
    dplyr::mutate(
      text_initialsplit = ifelse(
        is.na(text_initialsplit) | stringi::stri_isempty(text_initialsplit),
        NA_character_,
        text_initialsplit
      )
    ) %>%
    # Remove original column
    dplyr::select(-all_of(wordcol))


  # punctuation becomes a SPACE, not nothing. deleting it fuses words together
  # ("don't-stop" -> "don'tstop"); turning it into a space lets step 6 split
  # them properly. text_initialsplit sticks around for debugging.
  dat_prep <- dat_prep %>%
    dplyr::mutate(
      word_clean = text_initialsplit,
      # Remove non-alphabetic characters except apostrophes
      word_clean = ifelse(
        is.na(word_clean),
        NA_character_,
        stringi::stri_replace_all_regex(word_clean, "[^a-zA-Z']", " ")
      ),
      # Squish whitespace
      word_clean = ifelse(
        is.na(word_clean),
        NA_character_,
        stringr::str_squish(word_clean)
      )
    )



  # expands slang and contractions ("gonna" -> "going to"). the table's own
  # word column gets the same apostrophe treatment as the data, otherwise the
  # join keys don't line up.
  replacements_25 <- replacements_25 %>%
    dplyr::mutate(word = stringi::stri_replace_all_regex(word,
                                                         "[\u2018\u2019\u02BC\u201B\uFF07\u0092\u0091\u0060\u00B4\u2032\u2035]",
                                                         "'",
                                                         vectorize_all = FALSE
    ))

  # coalesce keeps the original when there's no replacement (join gives NA).
  # separate_rows because one token can expand into several words.
  dat_prep <- dat_prep %>%
    left_join(replacements_25, by = c("word_clean" = "word")) %>%
    mutate(word_clean = coalesce(replacement, word_clean)) %>%
    select(-replacement) %>%
    separate_rows(word_clean, sep = " ")




  # picks up the spaces step 4 introduced. no convert = TRUE here on purpose:
  # it would turn tokens like "T", "F" and "NA" into logicals.
  dat_prep <- dat_prep %>%
    tidyr::separate_rows(word_clean, sep = "[[:space:]]+") %>%
    dplyr::mutate(
      word_clean = ifelse(
        is.na(word_clean) | stringi::stri_isempty(word_clean),
        NA_character_,
        word_clean
      )
    )





  # build the dictionary from unique words and join it back, rather than
  # lemmatizing row by row - words repeat thousands of times in a real corpus
  if (lemmatize) {
    lemma_dict <- tibble(word_clean = unique(dat_prep$word_clean)) %>%
    filter(!is.na(word_clean) & word_clean != "") %>%
      mutate(lemma = textstem::lemmatize_words(word_clean))

    dat_prep <- dat_prep %>%
      left_join(lemma_dict, by = "word_clean") %>%
      mutate(word_clean = coalesce(lemma, word_clean)) %>%
      select(-lemma)

  }




  # NA'd rather than filtered out, so the row and its id_row_orig link back to
  # the original message survive
  if (omit_stops) {
    stopwords <- tolower(Temple_stops25$word)
    dat_prep <- dat_prep %>%
      dplyr::mutate(
        is_stopword = ifelse(
          is.na(word_clean),
          NA,
          word_clean %in% stopwords
        ),
        word_clean = ifelse(is_stopword, NA_character_, word_clean)
      ) %>%
      dplyr::select(-is_stopword)  # Remove the is_stopword column
  }

  return(dat_prep)
}
