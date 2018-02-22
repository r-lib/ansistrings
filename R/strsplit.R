
#' Split an ANSI colored string
#'
#' This is the color-aware counterpart of \code{base::strsplit}.
#' It works almost exactly like the original, but keeps the colors in the
#' substrings.
#'
#' @param x Character vector, potentially ANSI styled, or a vector to
#'   coarced to character.
#' @param split Character vector of length 1 (or object which can be coerced to
#'   such) containing regular expression(s) (unless \code{fixed = TRUE}) to use
#'   for splitting.  If empty matches occur, in particular if \code{split} has
#'   zero characters, \code{x} is split into single characters.
#' @param ... Extra arguments are passed to \code{base::strsplit}.
#' @return A list of the same length as \code{x}, the \eqn{i}-th element of
#'   which contains the vector of splits of \code{x[i]}. ANSI styles are
#'   retained.
#'
#' @family ANSI string operations
#' @export
#' @importFrom utils head tail
#' @examples
#' str <- paste0(
#'   crayon::red("I am red---"),
#'   crayon::green("and I am green-"),
#'   crayon::underline("I underlined")
#' )
#'
#' cat(str, "\n")
#'
#' # split at dashes, keep color
#' cat(ansi_strsplit(str, "[-]+")[[1]], sep = "\n")
#' strsplit(crayon::strip_style(str), "[-]+")
#'
#' # split to characters, keep color
#' cat(ansi_strsplit(str, "")[[1]], "\n", sep = " ")
#' strsplit(crayon::strip_style(str), "")

ansi_strsplit <- function(x, split, ...) {
  ## This works around some strsplit oddities
  if (is.null(split) || length(split) == 0 || split == "") {
    return(strsplit(x, split, ...))
  }

  plain <- strip_style(x)
  splits <- re_exec_all(plain, split)$.match
  lapply(seq_along(splits), function(i) {
    beg <- c(1L, splits[[i]]$end + 1L)
    end <- c(splits[[i]]$start - 1L, ansi_nchar(x[[i]]))
    res <- ansi_substring(x[[i]], beg, end)

    ## Work around more strsplit oddities
    if (tail(res, 1) == "") res <- head(res, -1)

    res
  })
}
