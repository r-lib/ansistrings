
#' Number of characters or bytes in an ANSI substring
#'
#' This is the ANSI-aware version of the [base::nchar()] function.
#' It simply drops all ANSI styles and calls [base::nchar()].
#'
#' @param x character vector, or a vector to be coerced to a character
#'   vector. Giving a factor is an error.
#' @param ... Extra arguments are passed to [base::nchar()].
#'
#' @importFrom crayon strip_style
#' @export
#' @examples
#' str <- crayon::red("I am red")
#'
#' # Has ANSI markup if your terminal supports it
#' str
#'
#' # It prints in red if your terminal supports it
#' cat(str)
#'
#' # But base::nchar() also counts the ANSI control characters,
#' # which is not what we want
#' nchar(str)
#'
#' # ansi_nchar is better:
#' ansi_nchar(str)
#'
#' # This is the same
#' nchar(crayon::strip_style(str))

ansi_nchar <- function(x, ...) {
  nchar(strip_style(x), ...)
}
