#' Compute the Down Bad Index (DBI) for every conversation
#'
#' Main analysis entry point. Takes a message-level data frame (e.g. the output
#' of \code{\link{read_imessages}}) and scores each dyadic conversation on five
#' behavioral-asymmetry ratios, then combines them into a single weighted index
#' (the DBI). A higher DBI means the focal speaker invests disproportionately
#' more than the other person (texts first more, replies faster, gets left on
#' read more, sends more words/messages).
#'
#' The five component ratios, each computed per conversation:
#' \itemize{
#'   \item \code{init_ratio} — who initiates new exchanges (\code{\link{calc_init_ratio}})
#'   \item \code{rt_ratio} — relative response times (\code{\link{calc_rt_ratio}})
#'   \item \code{message_ratio} / \code{word_ratio} — volume asymmetry (\code{\link{word_message_ratio}})
#'   \item \code{end_ratio} — who is left on read (\code{\link{calc_end_ratio}})
#' }
#' Each ratio is log-transformed (so 2x and 1/2x are symmetric around 0), scaled
#' by its standard deviation WITHOUT centering (preserving 0 = "perfectly
#' balanced" as a meaningful anchor), and combined as a weighted sum.
#'
#' @param df Message-level data frame with columns \code{speaker},
#'   \code{recipient}, \code{text}, \code{datetime}, \code{convo_num}.
#' @param speaker_str Display name of the focal speaker (the person whose
#'   "down-badness" is being scored).
#' @param sec_threshold Gap in seconds that separates two exchanges; defaults to
#'   172800 (48 hours). Also caps what counts as a valid response time.
#' @param message_min Optional minimum message count; conversations with fewer
#'   messages are dropped. NA (default) keeps all.
#' @param date_min,date_max Optional date bounds (flexible formats, e.g.
#'   "08-01-2021"); messages outside the window are dropped. NA keeps all.
#' @param weights Named list of 5 weights (word_ratio, message_ratio, end_ratio,
#'   init_ratio, rt_ratio) that must sum to 1. NA (default) uses the
#'   CFA-derived empirical weights.
#' @param rt Which response-time summary feeds the index: "median" (default,
#'   robust to outliers) or "mean".
#'
#' A conversation can fail to produce one of the five components, in which case
#' its \code{dbi} is NA and a warning names it. This happens when only one
#' person ever initiated (or ended) an exchange, so there is no ratio to take.
#' The usual cause is \code{sec_threshold} being long relative to how that pair
#' texts: if no silence in the thread ever exceeds it, the entire conversation
#' collapses into one exchange with a single initiator and a single ender.
#' Shortening \code{sec_threshold} generally resolves it. Such rows are kept
#' rather than dropped — their other components are still valid.
#'
#' @return A data frame with one row per conversation, ranked by descending
#'   \code{dbi}, containing the raw ratios, their log/z transforms, and the
#'   final \code{dbi} score. Conversations missing a component keep their row
#'   with \code{dbi = NA}; see the note above.
#' @seealso \code{\link{read_imessages}} to build the input data frame,
#'   \code{\link{plot_dbi_components}} to see why each partner scored as it did.
#' @examples
#' messages <- read_imessages(system.file("extdata", package = "dbiR"),
#'                            speaker_name = "Shawn Wang")
#'
#' # Default weights, median response times.
#' scored <- dbi(messages, speaker_str = "Shawn Wang")
#' scored[, c("other_recipient", "dbi", "init_ratio", "end_ratio")]
#'
#' # Restrict to a date window and a minimum conversation length.
#' dbi(messages,
#'     speaker_str = "Shawn Wang",
#'     date_min    = "01-01-2024",
#'     date_max    = "06-01-2024",
#'     message_min = 10)
#'
#' # Score on initiation alone by passing custom weights (they must sum to 1).
#' dbi(messages,
#'     speaker_str = "Shawn Wang",
#'     weights = list(word_ratio    = 0,
#'                    message_ratio = 0,
#'                    end_ratio     = 0,
#'                    init_ratio    = 1,
#'                    rt_ratio      = 0))
#' @export
dbi <- function(df,
                speaker_str,
                sec_threshold = 172800,
                message_min = NA,
                date_min = NA,
                date_max = NA,
                weights = NA,
                rt = "median") {


  # cheap checks first, before any of the expensive work below
  if (!rt %in% c("median", "mean")) {

    stop("reaction time must be specified as median (default) or mean")
  }



  # anything that isn't a full set of 5 (including the NA default, length 1)
  # falls back to the CFA weights
  if(length(weights) != 5) { #set to default weights

    weights <- list(
      init_ratio    = 0.30,
      rt_ratio = 0.25,
      message_ratio = 0.25,
      end_ratio     = 0.15,
      word_ratio    = 0.05
    )
  } else if (length(weights) == 5) {

    # custom weights - check the type, names and sum so a typo can't quietly
    # skew every score
    if (!is.list(weights)) {
      stop("weights must be a list")
    }

    expected <- c("word_ratio", "message_ratio", "end_ratio",
                  "init_ratio", "rt_ratio")

    # setequal, not %in% - plain membership would accept a list that repeats
    # one name and leaves another out, and that gets renamed positionally
    # below without complaint
    if (!setequal(names(weights), expected)) {
      stop("weights names must match: ", paste(expected, collapse = ", "))
    }
    if (round(sum(unlist(weights)), 10) != 1) {
      stop("weights must sum to 1")
    }

  }




  # callers use short names (init_ratio, rt_ratio, ...) but the weighted sum
  # at the end looks up columns by their transformed names (<ratio>_log_z).
  # rt decides whether rt_ratio points at the median or mean column.
  #
  # the setNames calls below are POSITIONAL, so reorder into canonical order
  # first. skip this and a caller who lists their weights in a different order
  # gets them silently attached to the wrong components - names and sum both
  # validate fine, so nothing catches it. keep this line right where it is.
  weights <- weights[c("init_ratio", "rt_ratio", "message_ratio",
                       "end_ratio", "word_ratio")]

  if (rt == "median") {


    weights <- setNames(weights, c("init_ratio_log_z",
                                   "rt_ratio_median_log_z",
                                   "message_ratio_log_z",
                                   "end_ratio_log_z",
                                   "word_ratio_log_z"))
  } else if (rt == "mean") {

    weights <- setNames(weights, c("init_ratio_log_z",
                                   "rt_ratio_mean_log_z",
                                   "message_ratio_log_z",
                                   "end_ratio_log_z",
                                   "word_ratio_log_z"))


  }








  # date window and message cutoff get applied once, here, so all four
  # component functions see exactly the same messages
  sentences_cleaned <- prepare_sentences(data = df,
                                         date_min = date_min,
                                         date_max = date_max,
                                         message_min = message_min)


  # each of these comes back one row per conversation, keyed on
  # (convo_num, other_recipient)
  end_ratio <- calc_end_ratio(sentences = sentences_cleaned,
                              sec_threshold = sec_threshold,
                              speaker_str = speaker_str)
  init_ratio <- calc_init_ratio(sentences = sentences_cleaned,
                                sec_threshold = sec_threshold,
                                speaker_str = speaker_str)
  rt_ratio <- calc_rt_ratio(sentences = sentences_cleaned,
                            sec_threshold = sec_threshold,
                            speaker_str = speaker_str,
                            arrange_by = rt)
  word_message <- word_message_ratio(sentences = sentences_cleaned,
                                     speaker_str = speaker_str)



  # full_join rather than inner, so a conversation missing one metric still
  # shows up with NA there instead of disappearing from the ranking
  join_list <- list(end_ratio, init_ratio, rt_ratio, word_message)

  joined_df <- reduce(join_list, full_join, by = c("other_recipient", "convo_num")) %>%
    select(convo_num, other_recipient, word_ratio, message_ratio, end_ratio,
           init_ratio, rt_ratio_mean, rt_ratio_median, speaker_messages, other_messages)


  # a component comes back NA when only one person ever did the thing it
  # measures. usually that's sec_threshold being longer than any silence in
  # the thread, which collapses the whole conversation into one exchange with
  # a single initiator and a single ender - it shows up on people you text
  # constantly. it can also be genuine, if one person really did start (or
  # end) every exchange over many of them.
  #
  # keep the rows either way, the other components are still good. but say
  # something, because otherwise the conversation just sinks to the bottom of
  # the ranking with no explanation.
  rt_component <- if (rt == "median") "rt_ratio_median" else "rt_ratio_mean"
  component_cols <- c("init_ratio", rt_component, "message_ratio",
                      "end_ratio", "word_ratio")

  incomplete <- joined_df %>%
    filter(if_any(all_of(component_cols), is.na))

  if (nrow(incomplete) > 0) {
    detail <- vapply(seq_len(nrow(incomplete)), function(i) {
      missing <- component_cols[is.na(unlist(incomplete[i, component_cols]))]
      paste0("  - ", incomplete$other_recipient[i],
             " (missing: ", paste(missing, collapse = ", "), ")")
    }, character(1))

    shown <- utils::head(detail, 5)
    more <- if (length(detail) > 5) {
      paste0("\n  ... and ", length(detail) - 5, " more")
    } else ""

    warning(nrow(incomplete), " conversation(s) could not be scored on every ",
            "component, so their `dbi` is NA:\n",
            paste(shown, collapse = "\n"), more,
            "\nThis happens when only one person initiated or ended every ",
            "exchange, which is common when sec_threshold (currently ",
            sec_threshold, "s / ", round(sec_threshold / 3600, 1), "h) is ",
            "longer than any silence in the thread, which makes the whole ",
            "conversation count as a single exchange. Try a shorter ",
            "sec_threshold to score these. The rows are kept, and their other ",
            "components are still valid.",
            call. = FALSE)
  }


  # log makes the ratios symmetric - texting twice as much and half as much
  # end up the same distance from 0, instead of 2 vs 0.5
  joined_df_log <- joined_df %>%
    mutate(across(c(word_ratio, message_ratio, end_ratio,
                    init_ratio, rt_ratio_mean, rt_ratio_median),
                  ~ log(.x),
                  .names = "{.col}_log"))

  # divide by sd but do NOT center. dividing puts the components on a
  # comparable spread; centering would move 0 from "actually balanced" to
  # "average for whoever happens to be in this corpus", which isn't what we
  # want it to mean
  joined_df_z <- joined_df_log %>%
    mutate(across(c(word_ratio_log, message_ratio_log, end_ratio_log,
                    init_ratio_log, rt_ratio_mean_log, rt_ratio_median_log),
                  ~ .x / sd(.x, na.rm = TRUE), #z score WITHOUT centering
                  .names = "{.col}_z"))



  # multiply each *_log_z column by its weight and add them up. sorted
  # descending, so the most lopsided conversation is at the top.
  final_df <- joined_df_z %>%
    mutate(dbi = rowSums(across(names(weights),
                                ~ .x * weights[[cur_column()]]))) %>%
    arrange(desc(dbi)) %>%
    relocate(other_recipient, dbi)


  return(final_df)









}
