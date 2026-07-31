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
#' @return A data frame with one row per conversation, ranked by descending
#'   \code{dbi}, containing the raw ratios, their log/z transforms, and the
#'   final \code{dbi} score.
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

  # ---- 1. Argument validation -------------------------------------------
  # Fail fast with readable messages before doing any expensive work.
  if (!rt %in% c("median", "mean")) {

    stop("reaction time must be specified as median (default) or mean")
  }
  
  
  # ---- 2. Resolve weights ------------------------------------------------
  # If the caller didn't supply a full set of 5 weights (including the
  # weights = NA default, which has length 1), fall back to the empirical
  # weights derived from the confirmatory factor analysis.
  if(length(weights) != 5) { #set to default weights

    weights <- list(
      init_ratio    = 0.30,
      rt_ratio = 0.25,
      message_ratio = 0.25,
      end_ratio     = 0.15,
      word_ratio    = 0.05
    )
  } else if (length(weights) == 5) {

    # Caller supplied custom weights: enforce the contract (list type,
    # recognized names, sums to 1) so a typo can't silently skew the index.
    if (!is.list(weights)) {
      stop("weights must be a list")
    }
    
    expected <- c("word_ratio", "message_ratio", "end_ratio",
                  "init_ratio", "rt_ratio")
    
    # setequal, not %in%: membership alone also accepts a list that repeats one
    # name and omits another, which would slip through and then be renamed
    # positionally below.
    if (!setequal(names(weights), expected)) {
      stop("weights names must match: ", paste(expected, collapse = ", "))
    }
    if (round(sum(unlist(weights)), 10) != 1) {
      stop("weights must sum to 1")
    }

  }
  
  
  
  # ---- 3. Map user-facing weight names onto internal column names --------
  # Users specify weights with short names (init_ratio, rt_ratio, ...); the
  # weighted sum below indexes columns by their transformed names
  # (<ratio>_log_z). The rt choice decides whether rt_ratio points at the
  # median- or mean-based column.
  #
  # The rename below is POSITIONAL, so the caller's list is reordered into the
  # canonical order first. Without this, weights supplied in a different order
  # -- e.g. list(word_ratio = , message_ratio = , end_ratio = , init_ratio = ,
  # rt_ratio = ) -- are silently reassigned to the wrong components, and no
  # validation can catch it because the names and the sum are all valid.
  # Reordering by name here is what lets the two setNames blocks stay
  # positional; keep this line directly above them.
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
  
  
  
  
  
  
  
  # ---- 4. Filter the corpus ----------------------------------------------
  # Apply the date window and minimum-message cutoff once, up front, so all
  # four component metrics are computed over the identical set of messages.
  sentences_cleaned <- prepare_sentences(data = df,
                                         date_min = date_min,
                                         date_max = date_max,
                                         message_min = message_min)

  # ---- 5. Compute the five component ratios ------------------------------
  # Each helper returns one row per conversation, keyed by
  # (convo_num, other_recipient).
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
  
  
  # ---- 6. Join components into one row per conversation -------------------
  # full_join (not inner) so a conversation missing one metric still appears,
  # with NA for that component rather than vanishing from the ranking.
  join_list <- list(end_ratio, init_ratio, rt_ratio, word_message)

  joined_df <- reduce(join_list, full_join, by = c("other_recipient", "convo_num")) %>%
    select(convo_num, other_recipient, word_ratio, message_ratio, end_ratio, 
           init_ratio, rt_ratio_mean, rt_ratio_median, speaker_messages, other_messages)
  
  # ---- 7. Transform: log, then scale -------------------------------------
  # Log makes the ratios symmetric: texting 2x as much (+log 2) and half as
  # much (-log 2) sit equidistant from 0 instead of 2 vs 0.5.
  joined_df_log <- joined_df %>%
    mutate(across(c(word_ratio, message_ratio, end_ratio,
                    init_ratio, rt_ratio_mean, rt_ratio_median),
                  ~ log(.x),
                  .names = "{.col}_log"))

  # Scale by SD but do NOT center: dividing by sd puts all components on a
  # comparable spread, while keeping 0 anchored at "perfectly balanced"
  # (centering would redefine 0 as "average for THIS corpus").
  joined_df_z <- joined_df_log %>%
    mutate(across(c(word_ratio_log, message_ratio_log, end_ratio_log, 
                    init_ratio_log, rt_ratio_mean_log, rt_ratio_median_log),
                  ~ .x / sd(.x, na.rm = TRUE), #z score WITHOUT centering
                  .names = "{.col}_z"))
  
  
  # ---- 8. Weighted sum -> final index -------------------------------------
  # For each row, multiply each *_log_z column by its weight and sum. Sorted
  # descending so the top of the table is the most asymmetric conversation.
  final_df <- joined_df_z %>%
    mutate(dbi = rowSums(across(names(weights),
                                ~ .x * weights[[cur_column()]]))) %>%
    arrange(desc(dbi)) %>%
    relocate(other_recipient, dbi)
  
  
  return(final_df)
  
  
  
  

  
  
  
  
}