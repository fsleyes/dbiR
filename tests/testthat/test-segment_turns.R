# segment_turns() is the shared foundation of calc_rt_ratio, calc_init_ratio
# and calc_end_ratio. Testing it directly means the three ratio functions can
# be read as thin summaries over a structure that is already known to be right.

test_that("consecutive messages from one sender collapse into a single turn", {
  msgs <- data.frame(
    convo_num = 1L,
    speaker   = c("A", "A", "B", "A"),
    recipient = c("B", "B", "A", "B"),
    datetime  = as.POSIXct("2024-01-01 10:00:00", tz = "UTC") + c(0, 60, 120, 180),
    stringsAsFactors = FALSE
  )

  turns <- segment_turns(msgs)

  # A's two opening messages are one turn, then B, then A again.
  expect_equal(nrow(turns), 3)
  expect_equal(turns$speaker, c("A", "B", "A"))
  expect_equal(turns$message_count, c(2, 1, 1))
})


test_that("a silence longer than the threshold starts a new exchange", {
  msgs <- data.frame(
    convo_num = 1L,
    speaker   = c("A", "B", "A", "B"),
    recipient = c("B", "A", "B", "A"),
    datetime  = as.POSIXct("2024-01-01 10:00:00", tz = "UTC") +
      c(0, 60, 60 + 200000, 60 + 200060),   # third message lands days later
    stringsAsFactors = FALSE
  )

  turns <- segment_turns(msgs, sec_threshold = 172800)

  # Two bursts of conversation separated by the long gap.
  expect_equal(length(unique(turns$exchange_id)), 2)
  # The lag() keeps the unanswered turn attached to the exchange it closed,
  # which is what calc_end_ratio relies on to find who let it drop.
  expect_equal(turns$exchange_id, c(0, 0, 1, 1))
})


test_that("over-threshold gaps are excluded from valid_response_time", {
  msgs <- data.frame(
    convo_num = 1L,
    speaker   = c("A", "B", "A", "B"),
    recipient = c("B", "A", "B", "A"),
    datetime  = as.POSIXct("2024-01-01 10:00:00", tz = "UTC") +
      c(0, 60, 60 + 200000, 60 + 200060),
    stringsAsFactors = FALSE
  )

  turns <- segment_turns(msgs, sec_threshold = 172800)

  # The 200000s lapse is a conversation break, not a reply, so it must not
  # count towards anyone's response-time average.
  expect_true(is.na(turns$valid_response_time[2]))
  expect_equal(turns$valid_response_time[1], 60)
})


test_that("turns never span two conversations", {
  # Segmentation is per-conversation: identical speakers in different threads
  # must not be merged just because their messages are adjacent in the frame.
  msgs <- data.frame(
    convo_num = c(1L, 1L, 2L, 2L),
    speaker   = c("A", "A", "A", "A"),
    recipient = c("B", "B", "B", "B"),
    datetime  = as.POSIXct("2024-01-01 10:00:00", tz = "UTC") + c(0, 60, 120, 180),
    stringsAsFactors = FALSE
  )

  turns <- segment_turns(msgs)

  expect_equal(nrow(turns), 2)
  expect_equal(turns$convo_num, c(1L, 2L))
})


test_that("output is ungrouped so callers control their own grouping", {
  turns <- segment_turns(fixture_messages())

  expect_false(dplyr::is_grouped_df(turns))
  expect_true(all(c("convo_num", "turn_id", "speaker", "recipient",
                    "turn_start", "turn_end", "message_count",
                    "response_time", "valid_response_time",
                    "exchange_id") %in% names(turns)))
})
