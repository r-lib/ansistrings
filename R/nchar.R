

#' @importFrom crayon strip_style
#' @export

ansi_nchar <- function(x, ...) {
  nchar(strip_style(x), ...)
}
