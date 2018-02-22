
re_match <- function(text, pattern, perl = TRUE, ...) {

  stopifnot(is.character(pattern), length(pattern) == 1, !is.na(pattern))
  text <- as.character(text)

  match <- regexpr(pattern, text, perl = perl, ...)

  start  <- as.vector(match)
  length <- attr(match, "match.length")
  end    <- start + length - 1L

  matchstr <- substring(text, start, end)
  matchstr[ start == -1 ] <- NA_character_

  res <- data.frame(
    stringsAsFactors = FALSE,
    .text = text,
    .match = matchstr
  )

  if (!is.null(attr(match, "capture.start"))) {

    gstart  <- attr(match, "capture.start")
    glength <- attr(match, "capture.length")
    gend    <- gstart + glength - 1L

    groupstr <- substring(text, gstart, gend)
    groupstr[ gstart == -1 ] <- NA_character_
    dim(groupstr) <- dim(gstart)

    res <- cbind(groupstr, res, stringsAsFactors = FALSE)
  }

  names(res) <- c(attr(match, "capture.names"), ".text", ".match")
  class(res) <- c("tbl_df", "tbl", class(res))
  res
}

re_exec_all <- function(text, pattern, perl = TRUE, ...) {

  text <- as.character(text)
  stopifnot(is.character(pattern), length(pattern) == 1, !is.na(pattern))

  if (length(text) == 0) {
    res <- empty_result(text, pattern, perl = perl, ...)
    for (i in seq_along(res)) {
      if (is.list(res[[i]])) class(res[[i]]) <- "rematch_allrecords"
    }
    return(res)
  }

  match <- gregexpr(pattern, text, perl = perl, ...)

  rec_names <- c("match", "start", "end")
  colnames <- c(attr(match[[1]], "capture.names"), ".match")
  num_groups <- length(colnames) - 1L
  non_rec <- structure(
    list(character(0), integer(0), integer(0)),
    names = rec_names
  )

  ## Non-matching strings have a rather strange special form,
  ## so we just treat them differently
  non <- vapply(match, function(m) m[1] == -1, TRUE)
  yes <- !non
  res <- replicate(length(text), list(), simplify = FALSE)
  if (any(non)) {
    res[non] <- list(replicate(num_groups + 1, non_rec, simplify = FALSE))
  }
  if (any(yes)) {
    res[yes] <- mapply(exec1, text[yes], match[yes], SIMPLIFY = FALSE)
  }

  res <- lapply(seq_along(res[[1]]), function(i) {
    structure(lapply(res, "[[", i), class = "rematch_allrecords")
  })

  res <- structure(
    res,
    names = colnames,
    row.names = seq_along(text),
    class = c("tbl_df", "tbl", "data.frame")
  )

  res$.text <- text
  nc <- ncol(res)
  res[, c(seq_len(nc - 2), nc, nc - 1)]
}

exec1 <- function(text1, match1) {

  start    <- as.vector(match1)
  length   <- attr(match1, "match.length")
  end      <- start + length - 1L
  matchstr <- substring(text1, start, end)
  matchrec <- list(match = matchstr, start = start, end = end)
  colnames <- c(attr(match1, "capture.names"), ".match")

  ## substring fails if the index is length zero,
  ## need to handle special case
  res <- if (is.null(attr(match1, "capture.start"))) {
    replicate(length(colnames), matchrec, simplify = FALSE)

  } else {
    gstart  <- unname(attr(match1, "capture.start"))
    glength <- unname(attr(match1, "capture.length"))
    gend    <- gstart + glength - 1L

    groupstr <- substring(text1, gstart, gend)
    dim(groupstr) <- dim(gstart)

    c(
      lapply(
        seq_len(ncol(groupstr)),
        function(i) {
          list(match = groupstr[, i], start = gstart[, i], end = gend[, i])
        }
      ),
      list(.match = matchrec)
    )
  }

  res
}

empty_result <- function(text, pattern, perl=TRUE, ...) {
  match <- regexpr(pattern, text, perl = perl, ...)
  num_groups <- length(attr(match, "capture.names"))
  structure(
    c(
      replicate(num_groups, list(), simplify = FALSE),
      list(character()),
      list(list())
    ),
    names = c(attr(match, "capture.names"), ".text", ".match"),
    row.names = integer(0),
    class = c("tbl_df", "tbl", "data.frame")
  )
}

re_match_all <- function(text, pattern, perl=TRUE, ...) {

  text <- as.character(text)
  stopifnot(is.character(pattern), length(pattern) == 1, !is.na(pattern))

  ## Need to handle this case separately, as gregexpr effectively
  ## does not work for this.
  if (length(text) == 0) return(empty_result(text, pattern, perl=perl, ...))

  match <- gregexpr(pattern, text, perl=perl, ...)

  num_groups <- length(attr(match[[1]], "capture.names"))

  ## Non-matching strings have a rather strange special form,
  ## so we just treat them differently
  non <- vapply(match, function(m) m[1] == -1, TRUE)
  yes <- !non
  res <- replicate(length(text), list(), simplify = FALSE)
  if (any(non)) {
    res[non] <- list(replicate(num_groups + 1, character(), simplify = FALSE))
  }
  if (any(yes)) {
    res[yes] <- mapply(match1, text[yes], match[yes], SIMPLIFY = FALSE)
  }

  ## Need to assemble the final data frame "manually".
  ## There is apparently no function for this. rbind() is almost
  ## good, but simplifies to a matrix if the dimensions allow it....
  res <- lapply(seq_along(res[[1]]), function(i) {
    lapply(res, "[[", i)
  })

  res <- structure(
    res,
    names = c(attr(match[[1]], "capture.names"), ".match"),
    row.names = seq_along(text),
    class = c("tbl_df", "tbl", "data.frame")
  )

  res$.text <- text
  nc <- ncol(res)
  res[, c(seq_len(nc - 2), nc, nc - 1)]
}

match1 <- function(text1, match1) {

  matchstr <- substring(
    text1,
    match1,
    match1 + attr(match1, "match.length") - 1L
  )

  ## substring fails if the index is length zero,
  ## need to handle special case
  if (is.null(attr(match1, "capture.start"))) {
    list(.match = matchstr)

  } else {
    gstart  <- attr(match1, "capture.start")
    glength <- attr(match1, "capture.length")
    gend    <- gstart + glength - 1L

    groupstr <- substring(text1, gstart, gend)
    dim(groupstr) <- dim(gstart)

    c(lapply(seq_len(ncol(groupstr)), function(i) groupstr[, i]),
      list(.match = matchstr)
      )
  }
}
