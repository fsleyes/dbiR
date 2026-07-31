#' Lexical and emotion norms lookup table
#'
#' Word-level psycholinguistic norms used to attach emotion and lexical
#' characteristics to cleaned message text. Joined against the
#' \code{word_clean} column produced by \code{\link{clean_messages}}.
#'
#' Columns fall into three families:
#' \itemize{
#'   \item \code{word} — the lookup key, lowercase.
#'   \item \code{emo_*} — emotion dimensions (e.g. \code{emo_anger},
#'     \code{emo_anxiety}, \code{emo_happiness}, \code{emo_intensity}). Each
#'     also has an \code{_rescale} variant on a normalized scale.
#'   \item \code{lex_*} — lexical characteristics (e.g. \code{lex_AoA} age of
#'     acquisition, concreteness, frequency).
#' }
#'
#' Use \code{names(lookup_Jul25)} to see the full list of available dimensions;
#' those names are what the \code{sentiment} arguments of
#' \code{\link{plot_convo}} and \code{\link{plot_dyadic_emotion_grid}} expect.
#'
#' @format A data frame with 156,203 rows and 46 columns.
#' @seealso \code{\link{clean_messages}} to produce joinable word-level data.
"lookup_Jul25"
