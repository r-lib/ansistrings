
#' Create the map for raw <-> ANSI coordinate conversion
#'
#' The result of this function can be used with [map_raw_to_ansi1()] and
#' [map_ansi_to_raw1()] do the actual conversion between them.
#'
#' @param re The positions of the ANSI tags, as the output of the
#'   [rematch2::re_exec_all()] function.
#' @return A three column matrix.
#'   First column is the raw coordinates of the tags, second column is
#'   ansi coordinates, third column is the offset between them.
#'
#' @keywords internal

make_shifts1 <- function(re) {
  shifts <- cbind(re$.match$start[[1]],
                  re$.match$end[[1]] - re$.match$start[[1]] + 1)
  shifts[, 2] <- cumsum(shifts[, 2])
  shifts <- cbind(
    c(shifts[1,1], shifts[-1, 1] - shifts[-nrow(shifts), 2]),
    shifts
  )
  shifts
}

#' Map raw string positions to ANSI
#'
#' [map_ansi_to_raw1()] does the opposite convertion, from ANSI positions
#' to raw positions.
#'
#' @param map The map matrix, created with [make_ansi_map1()].
#' @param raw An integer or numeric scalar, the raw position to convert.
#' @return A numeric scalar, the ANSI position corresponding to the
#'   raw position `raw`.
#'
#' @keywords internal
#' @importFrom utils tail

map_raw_to_ansi1 <- function(map, raw) {
  sh <- map$shifts
  wh <- tail(which(raw >= sh[, 1]), 1)
  if (!length(wh)) {
    raw
  } else {
    raw + sh[wh, 3]
  }
}

#' Map ANSI string positions to raw positions
#'
#' [map_raw_to_ansi1()] does the opposite convertion, from raw positions
#' to ANSI positions.
#'
#' @param map The map matrix, created with [make_ansi_map1()].
#' @param ansi An integer or numeric scalar, the ANSI position to convert.
#' @return A numeric scalar, the raw position corresponding to the
#'   ANSI position `ansi`.
#'
#' @keywords internal
#' @importFrom utils tail

map_ansi_to_raw1 <- function(map, ansi) {
  sh <- map$shifts
  wh <- tail(which(ansi >= sh[, 2]), 1)
  if (!length(wh)) {
    ansi
  } else {
    ansi - sh[wh, 3]
  }
}

#' Create a map of the ANSI tags of a single string
#'
#' This map can be then used in various string operations, e.g.
#' [ansi_substr()], etc.
#'
#' `make_ansi_map1()` works for a string scalar, `make_ansi_map()` does
#' the same for all strings in a character vector.
#'
#' @param str A string scalar.
#' @return A list with two components. `map` is a data frame with four
#'   columns: `start`, `end`, `open`, `close`. It has one row for each
#'   single ANSI markup. In other words, it has one row for each ANSI start
#'   tag. The `start` column contains the start positions (in raw
#'   coordinates) of the markup, the `end` column has the end positions.
#'   The `open` and `close` columns contain the full ANSI open and close
#'   tags.
#'
#'   `shifts` is a three column matrix that can be used to convert between
#'   raw and ANSI coordinates. It is created by [make_shifts1()], see that
#'   for the actual format.
#'
#' @keywords internal
#' @importFrom rematch2 re_exec_all
#' @importFrom utils tail

make_ansi_map1 <- function(str) {
  re <- re_exec_all(str, re_ansi())

  ## Number of ANSI tags
  num_tags <- length(re$.match$start[[1]])
  if (num_tags == 0) {
    return(data.frame(
      stringsAsFactors = FALSE,
      start = numeric(),
      end = numeric(),
      open = character(),
      close = character()
    ))
  }

  ## Which tags are start tags / end tags
  start_tags <- re$start$start[[1]] > 0

  shifts <- make_shifts1(re)

  res <- data.frame(
    stringsAsFactors = FALSE,
    start = shifts[, 1],
    end = rep(NA, num_tags),
    open = re$.match$match[[1]],
    close = rep(NA_character_, num_tags)
  )

  ## To avoid multiple calls to this
  re_closes <- re_closes()

  ## Currently active start tags
  active <- numeric()

  for (i in seq_len(num_tags)) {
    ## If it is a start tag, then we mark it as active
    if (start_tags[i]) {
      active <- c(active, i)

    ## End tag, check which active start tags it closes
    } else {
      endcode <- re$end$match[[1]][i]
      endtag <- re$.match$match[[1]][i]
      active_start_codes <- re$start$match[[1]][active]
      closes <- grepl(re_closes[[endcode]], active_start_codes)
      res$end[active[closes]] <- shifts[i, 1] - 1
      res$close[active[closes]] <- endtag
      active <- setdiff(active, active[closes])
    }
  }

  ## We only really need one row per start tag
  res <- res[start_tags, ]

  ## Close at the end of the string, if the ANSI close tag is missing
  res$end[is.na(res$end)] <- nchar(str) - tail(shifts[,1], 1)
  res$close[is.na(res$close)] <- ""

  list(map = res, shifts = shifts)
}

#' @rdname make_ansi_map1

make_ansi_map <- function(str) {
  lapply(str, make_ansi_map)
}
