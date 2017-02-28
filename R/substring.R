
#' @export

ansi_substring <- function(text, first, last = 1000000L) {
  if (!is.character(text)) text <- as.character(text)
  n <- max(lt <- length(text), length(first), length(last))
  if (lt && lt < n) text <- rep_len(text, length.out = n)
  ansi_substr(text, as.integer(first), as.integer(last))
}

#' @export

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
