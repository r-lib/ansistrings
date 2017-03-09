
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
  .Call("ansistrings_make_shifts1", re$.match$start[[1]], re$.match$end[[1]],
        PACKAGE = "ansistrings")
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
  .Call("ansistrings_map_raw_to_ansi1", map$shifts, as.integer(raw),
        PACKAGE = "ansistrings")
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
  .Call("ansistrings_map_ansi_to_raw1", map$shifts, as.integer(ansi),
        PACKAGE = "ansistrings")
}
## QUESTION: Does "Raw Coordinates" mean original coordinates, or coordinates
## after assuming ANSI sequences are zero length?  Return value of
## `make_ansi_map` suggests the later, but "Raw" suggests the former.

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
  ## QUESTION: unused, should remove `shifts`?
  shifts <- make_shifts1(re)
  .Call("ansistrings_make_ansi_map1", str, re$.match$start[[1]],
        re$.match$end[[1]], re$.match$match[[1]], re$start$start[[1]],
        re$start$match[[1]], re$end$match[[1]], PACKAGE = "ansistrings")
}
## Applies `make_ansi_map` to Each Value In Char Vector
##
## Here for if/when we write an internally vectorized version of this function
## so we can reference it here without having to re-write other code.

make_ansi_map <- function(str) lapply(make_ansi_map1, str)

#' @rdname make_ansi_map1

make_ansi_map <- function(str) {
  lapply(str, make_ansi_map1)
}
