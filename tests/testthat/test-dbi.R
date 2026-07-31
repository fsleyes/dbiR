# Ground truth for every expectation in this file is derived by hand in
# inst/extdata/README.md. The fixtures were written so that the initiation and
# conversation-ending counts come out as small exact integers, which is why
# those are asserted exactly while the volume ratios are asserted by direction.

test_that("initiation ratios match the hand-counted fixtures", {
  scored <- fixture_dbi()

  # Shawn opens 3 of 4 exchanges with Alice, Alice opens 1.
  expect_equal(dbi_row(scored, "Alice Test")$init_ratio, 3)
  # Mirror image: Bob opens 3, Shawn opens 1.
  expect_equal(dbi_row(scored, "Bob Chill")$init_ratio, 1 / 3)
  # Carol is deliberately symmetric: one each.
  expect_equal(dbi_row(scored, "Carol Even")$init_ratio, 1)
  expect_equal(dbi_row(scored, "Dana Edgecase")$init_ratio, 2)
})


test_that("end ratios count who goes silent, oriented so higher = left on read", {
  scored <- fixture_dbi()

  # end_ratio is other/speaker: Alice goes silent 3 times to Shawn's 1, so
  # Shawn is the one being left on read and the ratio is above 1.
  expect_equal(dbi_row(scored, "Alice Test")$end_ratio, 3)
  expect_equal(dbi_row(scored, "Bob Chill")$end_ratio, 1 / 3)
  expect_equal(dbi_row(scored, "Carol Even")$end_ratio, 1)
  expect_equal(dbi_row(scored, "Dana Edgecase")$end_ratio, 2)
})


test_that("response-time ratios reflect who replies faster", {
  scored <- fixture_dbi()

  # Shawn answers Alice in about a minute; she takes hours. The ratio is
  # other/speaker, so a large value means the focal speaker is the fast one.
  expect_gt(dbi_row(scored, "Alice Test")$rt_ratio_median, 50)
  # Reversed for Bob, who is the fast replier.
  expect_lt(dbi_row(scored, "Bob Chill")$rt_ratio_median, 0.02)
  # Carol's fixture uses matched ~30 minute replies on both sides.
  expect_equal(dbi_row(scored, "Carol Even")$rt_ratio_median, 1)
})


test_that("volume ratios reflect who writes more", {
  scored <- fixture_dbi()

  alice <- dbi_row(scored, "Alice Test")
  expect_gt(alice$word_ratio, 1)      # Shawn writes paragraphs, Alice writes "cool"
  expect_gt(alice$message_ratio, 1)

  bob <- dbi_row(scored, "Bob Chill")
  expect_lt(bob$word_ratio, 1)        # mirror image
  expect_lt(bob$message_ratio, 1)

  carol <- dbi_row(scored, "Carol Even")
  expect_equal(carol$message_ratio, 1)
})


test_that("dbi ranks partners from most to least asymmetric", {
  scored <- fixture_dbi()

  # This is the headline behaviour: the composite has to order the four
  # designed scenarios correctly, whatever the individual weights are.
  expect_equal(
    scored$other_recipient,
    c("Alice Test", "Dana Edgecase", "Carol Even", "Bob Chill")
  )

  # A balanced relationship should land at the zero point, because dbi()
  # scales by SD without centering specifically to keep 0 meaningful.
  expect_equal(dbi_row(scored, "Carol Even")$dbi, 0, tolerance = 0.01)

  # The two mirror-image scenarios must fall on opposite sides of it.
  expect_gt(dbi_row(scored, "Alice Test")$dbi, 0)
  expect_lt(dbi_row(scored, "Bob Chill")$dbi, 0)
})


test_that("one row per conversation, no partner duplicated", {
  scored <- fixture_dbi()

  expect_equal(nrow(scored), 4)
  expect_false(any(duplicated(scored$other_recipient)))
  expect_false(any(is.na(scored$dbi)))
})


test_that("rt = 'mean' scores the corpus without changing the ranking", {
  # The fixtures have no response-time outliers, so mean and median should
  # agree on the ordering even though they weight a different column.
  by_median <- fixture_dbi(rt = "median")
  by_mean   <- fixture_dbi(rt = "mean")

  expect_equal(by_mean$other_recipient, by_median$other_recipient)
  expect_false(any(is.na(by_mean$dbi)))
})


test_that("custom weights are accepted and change the score", {
  # Loading everything onto initiation must still rank Alice first but should
  # not reproduce the default composite exactly.
  init_only <- list(word_ratio = 0, message_ratio = 0, end_ratio = 0,
                    init_ratio = 1, rt_ratio = 0)

  scored <- fixture_dbi(weights = init_only)

  expect_equal(scored$other_recipient[1], "Alice Test")
  expect_false(isTRUE(all.equal(scored$dbi, fixture_dbi()$dbi)))
})


test_that("a conversation with no long silence warns but keeps its row", {
  # With a threshold longer than the whole thread, nothing splits it: the
  # conversation is a single exchange with one initiator and one ender, so
  # init_ratio and end_ratio have no second party and come out NA. This is the
  # case that silently sank a real conversation to the bottom of the ranking.
  df <- fixture_messages()

  expect_warning(
    scored <- dbi(df, speaker_str = "Shawn Wang",
                  sec_threshold = 60 * 60 * 24 * 365,   # one year
                  message_min = NA, date_min = NA, date_max = NA),
    "could not be scored on every component"
  )

  # Kept, not dropped.
  expect_equal(nrow(scored), 4)
  expect_true(any(is.na(scored$dbi)))

  # The components that do not depend on exchange structure still computed.
  expect_false(any(is.na(scored$word_ratio)))
  expect_false(any(is.na(scored$message_ratio)))
})


test_that("the incomplete-score warning names the conversation and the lever", {
  df <- fixture_messages()

  w <- tryCatch(
    dbi(df, speaker_str = "Shawn Wang", sec_threshold = 60 * 60 * 24 * 365,
        message_min = NA, date_min = NA, date_max = NA),
    warning = function(w) conditionMessage(w)
  )

  expect_match(w, "Alice Test|Bob Chill|Carol Even|Dana Edgecase")
  expect_match(w, "missing:")
  expect_match(w, "sec_threshold")
})


test_that("no warning when every conversation scores completely", {
  # The fixtures use multi-day gaps, so the default threshold splits them all
  # into several exchanges and every component is defined.
  expect_no_warning(fixture_dbi())
})


test_that("invalid arguments are rejected with an explanatory message", {
  df <- fixture_messages()
  call_dbi <- function(...) {
    dbi(df, speaker_str = "Shawn Wang",
        message_min = NA, date_min = NA, date_max = NA, ...)
  }

  expect_error(call_dbi(rt = "mode"), "median")

  # A bare vector rather than a list.
  expect_error(
    call_dbi(weights = c(word_ratio = .2, message_ratio = .2, end_ratio = .2,
                         init_ratio = .2, rt_ratio = .2)),
    "must be a list"
  )

  # Right length, wrong names.
  expect_error(
    call_dbi(weights = list(a = .2, b = .2, c = .2, d = .2, e = .2)),
    "names must match"
  )

  # Right names, does not sum to 1.
  expect_error(
    call_dbi(weights = list(word_ratio = .5, message_ratio = .5, end_ratio = .5,
                            init_ratio = .5, rt_ratio = .5)),
    "sum to 1"
  )
})


test_that("message_min drops conversations below the threshold", {
  df <- fixture_messages()

  # Carol's thread is the shortest in the corpus at 8 messages.
  scored <- dbi(df, speaker_str = "Shawn Wang",
                message_min = 10, date_min = NA, date_max = NA)

  expect_false("Carol Even" %in% scored$other_recipient)
  expect_true("Alice Test" %in% scored$other_recipient)
})


test_that("date bounds restrict the corpus to the requested window", {
  df <- fixture_messages()

  # The fixtures are spread across 2024: Alice in January, Bob in February,
  # Carol in March, Dana in April. A February-only window keeps just Bob.
  scored <- dbi(df, speaker_str = "Shawn Wang", message_min = NA,
                date_min = "01-25-2024", date_max = "03-01-2024")

  expect_equal(scored$other_recipient, "Bob Chill")
})
