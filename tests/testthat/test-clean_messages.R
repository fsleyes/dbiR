test_that("messages explode to one row per word", {
  dat <- data.frame(text = c("hello world", "one two three"),
                    stringsAsFactors = FALSE)

  out <- clean_messages(dat, wordcol = "text")

  expect_equal(nrow(out), 5)
  expect_equal(out$word_clean, c("hello", "world", "one", "two", "three"))
})


test_that("id_row_orig maps every word back to its source message", {
  dat <- data.frame(text = c("hello world", "one two three"),
                    stringsAsFactors = FALSE)

  out <- clean_messages(dat, wordcol = "text")

  # This column is what lets downstream code regroup words into messages, so
  # it has to survive the explosion intact.
  expect_equal(as.character(out$id_row_orig), c("1", "1", "2", "2", "2"))
})


test_that("internal punctuation separates words instead of fusing them", {
  # Regression test: an earlier version replaced punctuation with a space and
  # then stripped every non-letter INCLUDING that space, so "dont-stop"
  # collapsed to the single token "dontstop" and corrupted word counts.
  dat <- data.frame(text = "dont-stop believing", stringsAsFactors = FALSE)

  out <- clean_messages(dat, wordcol = "text")

  expect_equal(out$word_clean, c("dont", "stop", "believing"))
  expect_false(any(grepl("dontstop", out$word_clean)))
})


test_that("contractions in the replacement table are expanded", {
  # The bundled replacements_25 table rewrites contractions to their full
  # form, and the expansion is then split into separate word rows so each
  # part can be looked up in the norms table independently.
  dat <- data.frame(text = "i don't know", stringsAsFactors = FALSE)

  out <- clean_messages(dat, wordcol = "text")

  expect_equal(out$word_clean, c("i", "do", "not", "know"))
})


test_that("curly apostrophes are normalised before the replacement lookup", {
  # iOS substitutes a typographic apostrophe. The replacement table is keyed
  # on that form while typed text often uses the ASCII one, so both are
  # normalised to a single form -- otherwise whichever variant did not match
  # would silently pass through unexpanded.
  ascii <- clean_messages(data.frame(text = "i don't know", stringsAsFactors = FALSE), "text")
  curly <- clean_messages(data.frame(text = "i don’t know", stringsAsFactors = FALSE), "text")

  expect_equal(curly$word_clean, ascii$word_clean)
  expect_equal(curly$word_clean, c("i", "do", "not", "know"))
})


test_that("apostrophes are not word separators", {
  # Possessives are absent from the replacement table, so they exercise the
  # tokenizer directly: the apostrophe must stay inside the word rather than
  # splitting it, which is what keeps contractions matchable in the first place.
  dat <- data.frame(text = "alice's book", stringsAsFactors = FALSE)

  out <- clean_messages(dat, wordcol = "text")

  expect_equal(out$word_clean, c("alice's", "book"))
})


test_that("text is lowercased and stray punctuation removed", {
  dat <- data.frame(text = "HELLO, World!!!", stringsAsFactors = FALSE)

  out <- clean_messages(dat, wordcol = "text")

  expect_equal(out$word_clean, c("hello", "world"))
})


test_that("lemmatize reduces words to their lemma", {
  dat <- data.frame(text = "the running dogs are barking",
                    stringsAsFactors = FALSE)

  out <- clean_messages(dat, wordcol = "text", lemmatize = TRUE)

  expect_equal(out$word_clean, c("the", "run", "dog", "be", "bark"))
})


test_that("omit_stops blanks stopwords to NA without dropping rows", {
  dat <- data.frame(text = "the running dogs are barking",
                    stringsAsFactors = FALSE)

  kept    <- clean_messages(dat, "text", omit_stops = FALSE, lemmatize = TRUE)
  stopped <- clean_messages(dat, "text", omit_stops = TRUE,  lemmatize = TRUE)

  # Row count is identical: stopwords become NA rather than disappearing, so
  # message boundaries are preserved for regrouping.
  expect_equal(nrow(stopped), nrow(kept))
  expect_equal(stopped$word_clean, c(NA, "run", "dog", NA, "bark"))
})


test_that("a missing text column is reported by name", {
  dat <- data.frame(text = "hello", stringsAsFactors = FALSE)

  expect_error(clean_messages(dat, wordcol = "message"), "message")
})


test_that("the bundled corpus cleans without error", {
  # Exercises the real pipeline shape: read_imessages output straight into
  # clean_messages, with both optional stages switched on.
  out <- clean_messages(fixture_messages(), wordcol = "text",
                        omit_stops = TRUE, lemmatize = TRUE)

  expect_gt(nrow(out), 0)
  expect_true("word_clean" %in% names(out))
  # Cleaning must not invent or lose conversations.
  expect_equal(length(unique(out$convo_num)), 4)
})
