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
  
  
  
  # ---- 1. Normalize encoding + case ---------------------------------------
  # id_row_orig tags every row BEFORE the explosion below, so each word can be
  # traced back to (and regrouped into) its original message. The tryCatch
  # falls back to a plain re-encode if strict UTF-8 validation errors on a
  # pathological string.
  # Create working copy and perform initial split, standardizes text encoding so all characters are UTF8, adds an id variable
  # transform all text to lower
  dat_prep <- dat %>% dplyr::mutate(id_row_orig = factor(seq_len(nrow(dat))),
                                    text_initialsplit = tryCatch(stringi::stri_enc_toutf8(as.character(.[[wordcol]]),
                                                                                          is_unknown_8bit = TRUE,
                                                                                          validate = TRUE), error = function(e) stringi::stri_encode(as.character(.[[wordcol]]), to = "UTF-8")
                                    ) %>% tolower())
  
  # ---- 2. Standardize apostrophes -----------------------------------------
  # Map every unicode apostrophe/quote variant (curly quotes, primes, accents)
  # to the plain ASCII ' so contractions match the replacement table and
  # stopword list exactly.
  #Standardize apostrophes in original text
  dat_prep <- dat_prep %>% dplyr::mutate(text_initialsplit = ifelse(
    is.na(text_initialsplit), NA_character_, stringi::stri_replace_all_regex(
      text_initialsplit,
      "[\u2018\u2019\u02BC\u201B\uFF07\u0092\u0091\u0060\u00B4\u2032\u2035]",
      "'"
    )
  )
  )
  
  # ---- 3. Explode messages into words -------------------------------------
  # One row per whitespace-delimited token; contractions stay intact because
  # apostrophes are not separators. Empty tokens become NA rather than being
  # dropped, so message rows are preserved even when nothing survives.
  # Perform initial split into words (retains contractions)
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
  
  # ---- 4. Strip punctuation -----------------------------------------------
  # Non-letters (except apostrophes) become SPACES, not deletions, so embedded
  # punctuation splits words apart ("don't-stop" -> "don't stop") instead of
  # fusing them ("don'tstop"). The spaces are then split on in step 6.
  # text_initialsplit is kept alongside word_clean for debugging/traceability.
  # Initialize cleaning column with actual cleaned text
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
  
  
  # ---- 5. Apply the replacement dictionary --------------------------------
  # replacements_25 maps slang/abbreviations/contractions to expansions
  # ("gonna" -> "going to"). Its own 'word' column gets the same apostrophe
  # normalization as the data so join keys match exactly.
  replacements_25 <- replacements_25 %>%
    dplyr::mutate(word = stringi::stri_replace_all_regex(word,
                                                         "[\u2018\u2019\u02BC\u201B\uFF07\u0092\u0091\u0060\u00B4\u2032\u2035]",
                                                         "'", 
                                                         vectorize_all = FALSE
    ))
  
  # coalesce keeps the original word wherever no replacement exists (the join
  # yields NA); separate_rows re-splits because a replacement can expand one
  # token into several words ("gonna" -> "going to").
  dat_prep <- dat_prep %>%
    left_join(replacements_25, by = c("word_clean" = "word")) %>%
    mutate(word_clean = coalesce(replacement, word_clean)) %>%
    select(-replacement) %>%
    separate_rows(word_clean, sep = " ")



  # ---- 6. Final split ------------------------------------------------------
  # Split any remaining multi-word strings (from step 4's punctuation->space
  # substitution) and blank empty tokens to NA. convert is left off: type
  # conversion would coerce tokens like "T", "F" and "NA" away from character
  # and corrupt the column.
  dat_prep <- dat_prep %>%
    tidyr::separate_rows(word_clean, sep = "[[:space:]]+") %>%
    dplyr::mutate(
      word_clean = ifelse(
        is.na(word_clean) | stringi::stri_isempty(word_clean),
        NA_character_,
        word_clean
      )
    )
  
  
  
  
  # ---- 7. Optional lemmatization ------------------------------------------
  # Build the lemma dictionary from UNIQUE words then join it back — far
  # cheaper than lemmatizing every row when words repeat thousands of times.
  if (lemmatize) {
    lemma_dict <- tibble(word_clean = unique(dat_prep$word_clean)) %>%
    filter(!is.na(word_clean) & word_clean != "") %>%
      mutate(lemma = textstem::lemmatize_words(word_clean))
    
    dat_prep <- dat_prep %>%
      left_join(lemma_dict, by = "word_clean") %>%
      mutate(word_clean = coalesce(lemma, word_clean)) %>%
      select(-lemma)
    
  }
  
  
  
  # ---- 8. Optional stopword removal ---------------------------------------
  # Stopwords are NA'd, not filtered, so the row (and its id_row_orig link to
  # the original message) survives — important for message-level regrouping.
  # Stopword removal if requested (now keeps NAs)
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