#' Filter messages by date window and conversation size
#'
#' Pre-filter applied once by \code{\link{dbi}} before the component metrics
#' are computed, so every metric sees the identical message set. Restricts the
#' data to an optional date window and drops conversations with too few
#' messages.
#'
#' (Naming note: despite the name, this function no longer reconstructs
#' sentences — it only filters. A rename to something like
#' \code{filter_messages()} would better reflect current behavior.)
#'
#' @param data Message-level data frame with \code{datetime} and
#'   \code{convo_num} columns.
#' @param date_min,date_max Optional date bounds; strings parsed flexibly
#'   (e.g. "08-01-2021" or "2021-08-01"). NA disables that bound.
#' @param message_min Optional minimum number of messages a conversation must
#'   have to be kept. NA keeps all.
#'
#' @return The filtered data frame, grouped by \code{convo_num}, with an added
#'   \code{num_messages} column (per-conversation message count).
#' @keywords internal
prepare_sentences <- function(data, date_min, date_max, message_min) {


  # parse_date_time takes several order patterns so callers can write the date
  # however they like. comparisons are strict, so a message landing exactly on
  # a bound is excluded.
  if (!is.na(date_min) | !is.na(date_max)) {
    orders <- c("Ymd HMS", "mdY HMS", "dmY HMS", "Ymd", "mdY")

    if (!is.na(date_min)) {
      date_min <- parse_date_time(date_min, orders = orders, tz = Sys.timezone())
      data <- data %>% filter(datetime > date_min)
    }

    if (!is.na(date_max)) {
      date_max <- parse_date_time(date_max, orders = orders, tz = Sys.timezone())
      data <- data %>% filter(datetime < date_max)
    }
  }



  # counted after the date filter on purpose, so message_min applies to the
  # window being analysed rather than the all-time size of the thread
  data_clean <- data %>%
    group_by(convo_num) %>%
    mutate(num_messages = n())


  if (!is.na(message_min)) {

    data_clean <- data_clean %>%
      filter(num_messages > message_min)


  }


  return(data_clean)




}
