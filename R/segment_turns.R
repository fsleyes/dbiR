#' Collapse messages into turns and group those turns into exchanges
#'
#' Shared segmentation step behind every behavioral ratio. Rewrites a
#' message-level data frame as a turn-level one and annotates each turn with the
#' response it received and the exchange it belongs to.
#'
#' Two levels of structure are imposed:
#' \itemize{
#'   \item A \strong{turn} is an unbroken run of messages from one sender. A new
#'     turn starts when the sender changes, or when the gap since the previous
#'     message exceeds \code{sec_threshold} (so a person texting again days
#'     later starts a new turn rather than extending the old one).
#'   \item An \strong{exchange} is a burst of conversation. Turns are chained by
#'     response time; when a response takes longer than \code{sec_threshold} the
#'     conversation is treated as having lapsed and the next turn opens a new
#'     exchange.
#' }
#'
#' The \code{lag()} on \code{exchange_id} is deliberate: it keeps the turn that
#' went unanswered attached to the exchange it closed, rather than moving it
#' into the exchange that follows. \code{\link{calc_end_ratio}} depends on this
#' to identify who ended each exchange.
#'
#' @param sentences Message-level data frame with \code{speaker},
#'   \code{recipient}, \code{datetime}, and \code{convo_num}.
#' @param sec_threshold Silence gap in seconds that separates exchanges, and the
#'   longest gap still counted as a response. Default 172800 (48h).
#'
#' @return An ungrouped turn-level data frame, one row per turn, with
#'   \code{convo_num}, \code{turn_id}, \code{speaker}, \code{recipient},
#'   \code{turn_start}, \code{turn_end}, \code{message_count},
#'   \code{response_time} (seconds until the next turn),
#'   \code{valid_response_time} (the same, but NA where the gap exceeded
#'   \code{sec_threshold} and so does not count as a reply), and
#'   \code{exchange_id}.
#' @seealso \code{\link{calc_rt_ratio}}, \code{\link{calc_init_ratio}},
#'   \code{\link{calc_end_ratio}}, which all build on this.
#' @keywords internal
segment_turns <- function(sentences, sec_threshold = 172800) {

  sentences %>%

    # a turn breaks when the sender changes, or after a silence longer than
    # the threshold. cumsum over the breakpoints numbers them.
    group_by(convo_num) %>%
    mutate(prev_gap = as.numeric(difftime(datetime,
                                          lag(datetime, default = first(datetime)),
                                          units = "secs")),
           new_turn = speaker != lag(speaker, default = first(speaker)) |
             prev_gap > sec_threshold,
           turn_id = cumsum(new_turn)) %>%


    # every message in a turn has the same sender, so first() is fine for the
    # participant columns. the two timestamps bracket the turn.
    group_by(convo_num, turn_id) %>%
    summarize(speaker = first(speaker, na_rm = TRUE),
              recipient = first(recipient, na_rm = TRUE),
              turn_start = min(datetime),
              turn_end = max(datetime),
              message_count = n(),
              .groups = "drop_last") %>%
    filter(!is.na(turn_start)) %>%


    # response_time is how long the OTHER person took to answer this turn.
    # anything over the threshold isn't a reply, it's the conversation dying:
    # it starts a new exchange and gets blanked out of valid_response_time so
    # it can't drag the averages up.
    group_by(convo_num) %>%
    mutate(response_timestamp = lead(turn_start),
           response_time = difftime(response_timestamp,
                                    turn_end,
                                    units = "secs"),
           exchange_id = cumsum(ifelse(response_time > sec_threshold, 1, 0)),
           exchange_id = lag(exchange_id,
                             default = first(exchange_id)),
           valid_response_time = ifelse(response_time > sec_threshold,
                                        NA,
                                        response_time)) %>%

    # every caller regroups anyway, so don't leak grouping state out of here
    ungroup()
}
