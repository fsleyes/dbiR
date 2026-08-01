# dbiR

An R package for measuring behavioral asymmetry in text message conversations.

Have you ever felt like you wanted to quantify exactly how uninterested someone is in you as a potential romantic partner? Well now you can!

`dbiR` reads iMessage exports, computes five measures of behavioral asymmetry (eg. who initiates conversations more often), and combines them into one score — the Down Bad Index. A high
score means you're the one doing the reaching.

## Installing

```r
# install.packages("devtools")
devtools::install_github("fsleyes/dbiR")
```

## Getting your messages out of iMessage

Note: for now, you can only do this analysis if you have an iPhone and are able to pull your messages out of iCloud.

This is the annoying part, and it's not something the package can do for you.
Use [imessage-exporter](https://github.com/ReagentX/imessage-exporter):

```bash
imessage-exporter -f txt -c disabled -o ~/imessage_export
```

Those three flags are the ones that matter:

| flag | why |
|---|---|
| `-f txt` | The parser reads the plain-text format. HTML will not work. |
| `-c disabled` | Don't copy attachments. This is already the default, but it's worth being explicit — the alternatives (`clone`, `basic`, `full`) duplicate every photo and video you have ever sent, which can run to tens of gigabytes and takes a long time. The package doesn't look at the files either way. |
| `-o <dir>` | Where to put the export. Anywhere is fine; this is the directory you hand to `read_imessages()`. |

You'll get one `.txt` file per conversation, named after the participants'
phone numbers. `dbiR` expects that directory as-is.

Note that `-c disabled` stops the *files* being copied, but the exported text
still contains a line with the original attachment path wherever you sent one.
Those lines are stripped for you by `read_imessages(remove_attach = TRUE)`,
which is the default. Pass `remove_attach = FALSE` if you'd rather count them
as messages.

### Flags to avoid

Two options change how the exporter labels you, and both break the parser:

- **`-m` / `--custom-name`** renames "Me" to whatever you pass. The package
  does that rename itself, via `read_imessages(speaker_name = )`.
- **`-i` / `--use-caller-id`** replaces "Me" with your phone number. Every
  thread then looks like it has an unsaved contact in it, and all of them get
  dropped.

Leave both off and let the export say "Me".

### If you get back fewer conversations than you expected

Some threads are dropped on the way in, deliberately: group chats (more than
two people breaks every ratio the package computes), threads with only one
speaker, and threads where a participant is identified by a bare phone number
instead of a saved contact name.

That last one is usually the big one, and it is worth knowing about before you
conclude the package is broken. On my own export, 854 files came down to 221
usable conversations, and most of the loss was contacts my phone had never
resolved to a name. If your contacts aren't being picked up at all, the
exporter takes `-n <path>` to point at an address book database
(`AddressBook-v22.abcddb` on macOS) explicitly.

You can also narrow the export up front with `-t` (filter to specific
contacts) or `-s` / `-e` (date bounds), though `dbi()` can do the date
filtering later with `date_min` and `date_max`.

## Using it

```r
library(dbiR)

messages <- read_imessages("~/imessage_export", speaker_name = "Your Name")
scored   <- dbi(messages, speaker_str = "Your Name")
```

`scored` is one row per conversation, sorted worst-first. The columns you
probably care about:

```r
scored[, c("other_recipient", "dbi", "init_ratio", "rt_ratio_median",
           "end_ratio", "word_ratio", "message_ratio")]
```

Most of the arguments are about deciding what counts:

```r
dbi(messages,
    speaker_str   = "Your Name",
    sec_threshold = 172800,        # 48h of silence ends a conversation
    message_min   = 1000,          # ignore threads shorter than this
    date_min      = "01-01-2022",
    date_max      = "01-01-2024")
```

`sec_threshold` matters and it will differ based on your own texting rhythms. It defines where one
conversation stops and the next begins, which in turn defines who initiated and
who got left hanging. Two days may be a reasonable default for close friends and may be
too short for people you talk to a few times a year. You will have to pick one for yourself.

You can ignore messages that are shorter (eg. maybe if you don't care about data from acquaintances) using the message_min parameter, which allows you to filter out contacts with a minimum no. of messages

## What the five components are

Every component is a ratio oriented the same way, so above 1 always means
you're the one leaning in (ie. you are more down bad for them):

- **`init_ratio`** — how often you start a conversation vs. how often they do
- **`rt_ratio`** — how long they take to reply vs. how long you take
- **`end_ratio`** — how often they let the conversation drop vs. how often you do
- **`message_ratio`** — messages you send vs. messages they send
- **`word_ratio`** — words you write vs. words they write

The index takes the log of each (so texting twice as much and half as much are
the same distance from balanced), divides by the standard deviation without
centering, and takes a weighted sum. Not centering is deliberate: it keeps zero
meaning "actually balanced" rather than "average for whoever happens to be in
your phone."

To see why a particular person scored the way they did, rather than just that
they did:

```r
plot_dbi_components(scored)
```

Two people can land on the same score for opposite reasons — one because you
always text first, another because you write paragraphs and get back "k". The
component plot separates those.

## About the weights

The defaults come from a confirmatory factor analysis, loading initiation
heaviest and word count lightest:

```
init_ratio 0.30, rt_ratio 0.25, message_ratio 0.25, end_ratio 0.15, word_ratio 0.05
```

That CFA was run on my own message history. Treat the weights as a
reasonable starting point, not as a validated instrument. If you disagree with
them, pass your own — they just have to be a named list summing to 1:

```r
dbi(messages, speaker_str = "Your Name",
    weights = list(init_ratio = 0.5, rt_ratio = 0.5, message_ratio = 0,
                   end_ratio = 0, word_ratio = 0))
```

Feel free to play around with the weights if you feel like you think some constructs are more important than others.

## The other half: message content

Separately from the behavioral metrics, `clean_messages()` tokenizes message
text and normalizes it against a bundled table of lexical and emotion norms
(`lookup_Jul25`, ~156k words):

```r
words <- clean_messages(messages, wordcol = "text",
                        omit_stops = TRUE, lemmatize = TRUE)

plot_dyadic_emotion_grid(words,
                         people = c("Your Name", "Someone Else"),
                         sentiment = c("emo_happiness", "emo_anxiety"))
```

`names(lookup_Jul25)` lists the available dimensions.

This is a fun way to visualize the emotional context of your text messages with other people across time. Sometimes you can pinpoint inflection points that symbolize 

### Two pipelines, two settings

These are separate paths through the package and they want opposite cleaning
settings. Getting this backwards is the easiest way to produce numbers that
look fine and aren't.

**Emotion analysis — `omit_stops = TRUE`, `lemmatize = TRUE`.**

```r
messages <- read_imessages(dir, speaker_name = "Your Name")
words    <- clean_messages(messages, wordcol = "text",
                           omit_stops = TRUE, lemmatize = TRUE)
plot_dyadic_emotion_grid(words, people = c(...), sentiment = c(...))
```

Here you want both on. Stopwords ("the", "and", "i") carry no emotional signal
and just dilute the averages, and lemmatizing means "running" and "ran" both
find "run" in the norms table instead of missing it.

**Down Bad Index — `omit_stops = FALSE`, `lemmatize = FALSE`.**

```r
messages <- read_imessages(dir, speaker_name = "Your Name")
scored   <- dbi(messages, speaker_str = "Your Name")
```

`dbi()` reads the raw `text` column and doesn't need `clean_messages()` at all,
so in normal use this is handled for you — just pass it the output of
`read_imessages()` directly.

It matters if you clean first and then feed the result in. Stopword removal
blanks those words out, and a short message made entirely of them ("ok",
"yeah", "i know") cleans to nothing and drops out of the data. That doesn't
just undercount messages: losing the short replies changes where turns begin
and end, which shifts response times and the initiation and left-on-read counts
too. Every one of the five components moves, and nothing errors. If you do want
cleaned text in the DBI path for some reason, leave both arguments at their
defaults (`FALSE`), which is what they're set to for exactly this reason.

## Caveats

Reactions, edits, unsends, and
messages sent from a different number all shift the counts in ways the package
can't see.

Response time is the crudest of the five. It can't tell "ignoring you" from
"asleep," and the 48-hour cap is a blunt instrument for that. The median is
used by default because a single three-day gap otherwise dominates the mean.

Conversations where one person did all of the initiating (or all of the
ending) are dropped rather than scored, since the ratio has a zero denominator.
This means the most lopsided threads can be missing from the output entirely,
which is worth remembering when you're looking at the ranking.


## Trying it without your own data

The package ships a small synthetic corpus for testing and examples:

```r
demo <- system.file("extdata", package = "dbiR")
messages <- read_imessages(demo, speaker_name = "Shawn Wang")
dbi(messages, speaker_str = "Shawn Wang")
```

Four conversations, built so each component has a hand-checkable value —
one lopsided in each direction, one balanced, and one that exercises the
parser's handling of tapbacks, attachments, read receipts, and multi-paragraph
messages. The derivations are in `inst/extdata/README.md`.

## Acknowledgements

A big thank you to the people who listened to me yap endlessly about this project and provided valuable feedback (in no particular order):

- Kevin Trent
- Valerie Polad
- Olivia Bishop
- Jamie Reilly
- Leia Donaway
- Brooke Cullen
- Kristin Pischel
- Cody Cushing
- Isabel Leiva

## License

MIT.
