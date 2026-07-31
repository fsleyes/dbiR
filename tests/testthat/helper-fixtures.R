# Shared fixture helpers.
#
# The bundled corpus in inst/extdata is designed so that every component ratio
# has a hand-computable value; see inst/extdata/README.md for the derivation of
# each expected number. Parsing it is cheap, but several test files need the
# same two objects, so they are built through these helpers rather than being
# copy-pasted.

fixture_dir <- function() {
  system.file("extdata", package = "dbiR")
}

# Message-level data frame for the whole bundled corpus.
fixture_messages <- function() {
  read_imessages(fixture_dir(), speaker_name = "Shawn Wang")
}

# Scored conversations, with every optional filter disabled so the tests see
# the full corpus regardless of the defaults dbi() happens to carry.
fixture_dbi <- function(...) {
  dbi(fixture_messages(),
      speaker_str = "Shawn Wang",
      message_min = NA,
      date_min    = NA,
      date_max    = NA,
      ...)
}

# Pull the single scored row for one partner.
dbi_row <- function(scored, partner) {
  scored[scored$other_recipient == partner, ]
}
