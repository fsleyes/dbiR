test_that("only the four valid dyads survive ingestion", {
  df <- fixture_messages()

  expect_equal(length(unique(df$convo_num)), 4)
  expect_setequal(
    setdiff(unique(df$speaker), "Shawn Wang"),
    c("Alice Test", "Bob Chill", "Carol Even", "Dana Edgecase")
  )
})


test_that("every message carries the other participant as recipient", {
  df <- fixture_messages()

  # recipient is derived from speaker, so it must never be NA and must never
  # equal the speaker on the same row.
  expect_false(any(is.na(df$recipient)))
  expect_false(any(df$speaker == df$recipient))
})


test_that("unsaved contacts and one-sided threads are filtered out", {
  df <- fixture_messages()

  # +19995550123 is a plumber with no saved display name, so process_txt_2
  # rejects the thread outright rather than emitting "+1..." as a speaker.
  expect_false(any(grepl("\\+", df$speaker)))

  # +15550000005 is a notes-to-self thread: one speaker, so message_to_list
  # drops it for failing the dyad check. If it had leaked through, Shawn would
  # appear as his own recipient.
  expect_false(any(df$recipient == "Shawn Wang" & df$speaker == "Shawn Wang"))
})


test_that("list_threads rejects group chats by their comma-separated filename", {
  # A real group export is named "+1..., +1....txt". Commas and spaces are not
  # portable filenames, so the fixture ships under a portable name and the
  # pathological one is reconstructed here at test time.
  src <- system.file("extdata", "groupchat_two_handles.txtin", package = "dbiR")
  expect_true(file.exists(src))

  tmp <- withr::local_tempdir()
  file.copy(src, file.path(tmp, "+13105550001, +13105550002.txt"))
  file.copy(system.file("extdata", "+15550000003.txt", package = "dbiR"), tmp)

  found <- list_threads(tmp)

  # The dyad is kept, the group chat is not.
  expect_length(found, 1)
  expect_match(basename(found), "^\\+15550000003\\.txt$")
})


test_that("multi-paragraph messages stay a single message", {
  # Dana's first message spans three paragraphs separated by blank lines.
  # Blank lines are NOT message boundaries -- only timestamp headers are --
  # so all three paragraphs must land in one row, joined by newlines.
  dana <- process_txt_2(
    file.path(fixture_dir(), "+15550000004.txt"),
    speaker_name = "Shawn Wang"
  )

  first <- dana$text[1]
  expect_match(first, "hey dana, two things")
  expect_match(first, "happy april fools day")
  expect_match(first, "are we still on for trivia thursday")
  expect_true(grepl("\n", first))
})


test_that("tapbacks, read receipts and quoted replies are stripped", {
  dana <- process_txt_2(
    file.path(fixture_dir(), "+15550000004.txt"),
    speaker_name = "Shawn Wang"
  )

  # Tapback metadata lines are not conversation content.
  expect_false(any(grepl("Tapbacks:", dana$text, fixed = TRUE)))

  # "(Read by ...)" receipts must be stripped from the timestamp, otherwise
  # the datetime fails to parse and the row is silently dropped downstream.
  expect_false(any(grepl("Read by", dana$text, fixed = TRUE)))
  expect_false(any(is.na(dana$datetime)))

  # The quoted-reply block contributes neither its marker line nor its
  # 4-space-indented preview, but the actual reply text survives.
  expect_false(any(grepl("responded to an earlier message", dana$text)))
  expect_true(any(grepl("book the table for seven", dana$text)))
})


test_that("remove_attach controls whether attachment lines are kept", {
  path <- file.path(fixture_dir(), "+15550000004.txt")

  kept    <- process_txt_2(path, speaker_name = "Shawn Wang", remove_attach = FALSE)
  dropped <- process_txt_2(path, speaker_name = "Shawn Wang", remove_attach = TRUE)

  # The fixture holds exactly one attachment line.
  expect_equal(nrow(kept) - nrow(dropped), 1)
  expect_true(any(grepl("/Library/Messages/Attachments/", kept$text, fixed = TRUE)))
  expect_false(any(grepl("/Library/Messages/Attachments/", dropped$text, fixed = TRUE)))
})


test_that("datetime is POSIXct with an explicit timezone", {
  df <- fixture_messages()

  # POSIXct, not the list-based POSIXlt: POSIXlt is fragile through
  # summarize()/unique() and across parallel workers.
  expect_s3_class(df$datetime, "POSIXct")
  expect_true(nzchar(attr(df$datetime, "tzone")))
  expect_false(any(is.na(df$datetime)))
})


test_that("timestamps parse the same under a non-English locale", {
  # %b and %p are locale-dependent, so on a French system every timestamp
  # would parse to NA without the C-locale guard in process_txt_2 -- and NA
  # datetimes are silently dropped by date filtering downstream.
  skip_if(!nzchar(suppressWarnings(Sys.setlocale("LC_TIME", "fr_FR.UTF-8"))),
          "fr_FR.UTF-8 locale not available")
  withr::defer(Sys.setlocale("LC_TIME", "C"))

  df <- fixture_messages()

  expect_false(any(is.na(df$datetime)))
  expect_equal(format(min(df$datetime)), "2024-01-05 09:00:00")
})


test_that("an empty directory warns instead of failing cryptically", {
  # Previously this died with "object 'speaker' not found", which says nothing
  # about the actual problem (wrong folder, or everything filtered out).
  tmp <- withr::local_tempdir()

  expect_warning(
    out <- read_imessages(tmp, speaker_name = "Shawn Wang"),
    "No valid dyadic conversations"
  )

  expect_equal(nrow(out), 0)
  expect_true(all(c("speaker", "recipient", "datetime", "convo_num") %in% names(out)))
})


test_that("speaker_name replaces the literal 'Me' from the export", {
  dana <- process_txt_2(
    file.path(fixture_dir(), "+15550000004.txt"),
    speaker_name = "Some Other Name"
  )

  expect_true("Some Other Name" %in% dana$speaker)
  expect_false("Me" %in% dana$speaker)
})
