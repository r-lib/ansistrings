
#' Safe sequence operator
#'
#' This is a safer version of [base::seq()]. In particular, it avoids
#' creating backwards sequences, unless the `by` parameter is
#' explicitly negative.
#'
#' \code{\%:\%} is the operator version of `myseq`.
#'
#' @param from Start of sequence.
#' @param to End of sequence.
#' @param by Step size. Can be negative, and then `to` must not be more
#'   than `from`.
#' @return An integer vector.
#'
#' @keywords internal

myseq <- function(from, to, by = 1) {
  stopifnot(by != 0)
  if (by > 0) {
    if (to < from) {
      integer()
    } else {
      seq(from, to, by = by)
    }
  } else {
    if (to > from) {
      integer()
    } else {
      seq(from, to, by = by)
    }
  }
}

#' @rdname myseq

`%:%` <- function(from, to) myseq(from, to)
