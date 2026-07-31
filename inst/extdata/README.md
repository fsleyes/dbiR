# Test text threads

Synthetic iMessage exports (imessage-exporter format) for testing the DBI
pipeline. Run with:

```r
df <- read_imessages("test_texts", speaker_name = "Shawn Wang")
result <- dbi(df, speaker_str = "Shawn Wang",
              message_min = NA, date_min = NA, date_max = NA)
```

All conversations use gaps well under the 48h `sec_threshold` within an
exchange and >3 days between exchanges, so exchange segmentation is
unambiguous.

Files are named by phone-number handle (like real imessage-exporter output)
because `list_threads()` keeps only filenames containing `+`. The contact's
display name inside each file identifies the scenario.

## Conversation files (should survive ingestion: 4 dyads)

### +15550000001.txt (Alice Test) — HIGH dbi expected (Shawn is down bad)
- 4 exchanges. **Initiations:** Shawn 3, Alice 1 → `init_ratio = 3.0`
- **Ends:** Alice goes silent 3x, Shawn 1x → `end_ratio (other/speaker) = 3.0`
- **Response times:** Shawn replies in ~1 min; Alice in ~2.5–5.75 h
  → `rt_ratio >> 1`
- **Volume:** Shawn 10 msgs / long; Alice 8 msgs / terse → word & message
  ratios > 1

### +15550000002.txt (Bob Chill) — LOW dbi expected (Bob is down bad, mirror image)
- 4 exchanges. **Initiations:** Bob 3, Shawn 1 → `init_ratio = 1/3`
- **Ends:** Shawn goes silent 3x, Bob 1x → `end_ratio = 1/3`
- **Response times:** Bob ~1 min; Shawn ~4–6 h → `rt_ratio << 1`
- **Volume:** Bob long/many; Shawn terse → word & message ratios < 1

### +15550000003.txt (Carol Even) — dbi ≈ 0 expected
- 2 exchanges: each person initiates 1, each ends 1 → both ratios = 1
- Response times ~28–32 min on both sides → `rt_ratio ≈ 1`
- Similar message lengths/counts → word & message ratios ≈ 1

### +15550000004.txt (Dana Edgecase) — parser gauntlet (also a valid dyad)
Exercises every special case in `process_txt_2`:
- multi-paragraph message with internal blank lines (must stay ONE message)
- `(Read by you after N seconds)` and `(Read by them after N seconds)`
  receipt suffixes on the timestamp header
- `Tapbacks: ♥ by Me` metadata line (must be stripped)
- attachment path (`/Library/Messages/Attachments/...`) — dropped when
  `remove_attach = TRUE`, kept otherwise
- quoted-reply block: `This message responded to an earlier message.` +
  4-space-indented preview line (both must be stripped; the real reply kept)
- Ground truth: initiations Shawn 2 / Dana 1; ends Dana 2 / Shawn 1

## Files that must be FILTERED OUT

| file | filtered by | reason |
|---|---|---|
| `groupchat_two_handles.txtin` | `list_threads` | group chat — see note below |
| `+19995550123.txt` | `process_txt_2` | unsaved contact (phone-number speaker) → empty tibble |
| `+15550000005.txt` | `message_to_list` | only one speaker (notes to self) → fails dyad check |

**Note on the group-chat fixture.** `list_threads()` detects group chats by the
comma in the filename, so testing it properly needs a file literally named
`+13105550001, +13105550002.txt`. Commas and spaces are not portable filenames
and `R CMD check` rejects them, so the content ships under the portable name
`groupchat_two_handles.txtin` — the `.txtin` extension also keeps
`list_threads()` from picking it up as a real thread. To exercise the filter,
copy it into a temp directory under the comma name at test time:

```r
src <- system.file("extdata", "groupchat_two_handles.txtin", package = "dbiR")
tmp <- withr::local_tempdir()
file.copy(src, file.path(tmp, "+13105550001, +13105550002.txt"))
expect_length(list_threads(tmp), 0)
```

So `read_imessages()` on this folder should return exactly **4 conversations**
(Alice, Bob, Carol, Dana) and `dbi()` should rank them
**Alice > Dana > Carol > Bob**.

## Verified output (2026-07-08, defaults, rt = "median")

| other_recipient | dbi | init_ratio | end_ratio | rt_ratio_median | word_ratio | message_ratio |
|---|---|---|---|---|---|---|
| Alice Test | 1.170 | 3.000 | 3.000 | 201.5 | 7.833 | 1.429 |
| Dana Edgecase | 0.679 | 2.000 | 2.000 | 15.0 | 1.511 | 1.250 |
| Carol Even | 0.001 | 1.000 | 1.000 | 1.0 | 1.060 | 1.000 |
| Bob Chill | -1.134 | 0.333 | 0.333 | 0.004 | 0.068 | 0.750 |
