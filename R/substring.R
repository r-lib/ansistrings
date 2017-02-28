
#' Substring of an ANSI-styled string, keeping correct colors
#'
#' These functions are the ANSI-aware counterparts of [base::substr()] and
#' [base::substring()]. They extract substrings from a character vector,
#' while keeping the colors and style of the characters in the strings
#' the same.
#'
#' @param x,text A character vector.
#' @param start,first Integer, the first element to be extracted.
#' @param stop,last Integer, the last element to extracted.
#' @return For `substr`, a character vector of the same length and with
#'   the same attributes as `x` (after possible coercion).
#'
#'   For `substring`, a character vector of length the longest of the
#'   arguments.  This will have names taken from `x` (if it has any
#'   after coercion, repeated as needed), and other attributes copied
#'   from `x` if it is the longest of the arguments).
#'
#' @export
#' @examples
#' str <- crayon::bold(
#'   "This text is bold, and", crayon::red("parts of it are red,"),
#'   "parts of it not."
#' )
#' cat(str, "\n")
#'
#' cat(ansi_substr(str, 1, 17), "\n")
#' cat(ansi_substr(str, 1, 42), "\n")
#' cat(ansi_substr(str, 24, 42), "\n")
#'
#' ## Vector arguments, just like in base::substring and base::substr
#' cat(ansi_substr(c(str, str), c(1, 20), c(22, 42)), sep = "\n")
#' cat(ansi_substring(str, 1:30, 1:30), "\n")

ansi_substr <- function(x, start, stop) {
  if (!is.character(x)) x <- as.character(x)
  start <- suppressWarnings(as.numeric(start))
  stop <- suppressWarnings(as.numeric(stop))
  if (any(is.na(start)) || any(is.na(stop))) {
    stop("non-numeric or NA substring positions are not supported")
  }

  if (length(start) > 1) start <- rep(start, length.out = length(x))
  if (length(stop ) > 1) stop  <- rep(stop , length.out = length(x))

  res <- vapply(seq_along(x), FUN.VALUE = "", function(i) {
    ansi_substr1(
      x[i],
      if (length(start) > 1) start[i] else start,
      if (length(stop) > 1) stop[i] else stop
    )
  })

  attributes(res) <- attributes(x)
  res
}

ansi_substr1 <- function(x, start, stop) {
  if (start > stop) return("")

  map <- make_ansi_map1(x)

  active_at_start <- map$map$start <= start & map$map$end >= start
  active_at_end   <- map$map$start <= stop  & map$map$end >= stop

  ansi_start <- vapply(start, map_raw_to_ansi1, 1, map = map)
  ansi_stop  <- vapply(stop,  map_raw_to_ansi1, 1, map = map)

  paste0(
    paste(map$map$open[active_at_start], collapse = ""),
    substr(x, ansi_start, ansi_stop),
    paste(map$map$close[active_at_end], collapse = "")
  )
}

#' @rdname ansi_substr
#' @export

ansi_substring <- function(text, first, last = 1000000L) {
  if (!is.character(text)) text <- as.character(text)
  n <- max(lt <- length(text), length(first), length(last))
  if (lt && lt < n) text <- rep_len(text, length.out = n)
  ansi_substr(text, as.integer(first), as.integer(last))
}
