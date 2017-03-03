
`%+%` <- function (lhs, rhs) {
  stopifnot(is.character(lhs), is.character(rhs))
  stopifnot(length(lhs) == length(rhs) || length(lhs) == 1 ||
              length(rhs) == 1)
  if (length(lhs) == 0 && length(rhs) == 0) {
    paste0(lhs, rhs)

  } else if (length(lhs) == 0) {
    lhs

  } else if (length(rhs) == 0) {
    rhs

  } else {
    paste0(lhs, rhs)
  }
}
