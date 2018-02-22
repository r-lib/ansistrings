
#' Create the map for raw <-> ANSI coordinate conversion
#'
#' The result of this function can be used with [map_raw_to_ansi1()] and
#' [map_ansi_to_raw1()] do the actual conversion between them.
#'
#' @param re The positions of the ANSI tags.
#' @return A three column matrix.
#'   First column is the start coordinates of the tags, in the raw,
#'   ANSI-less string. The second column is the coordinates of the tags in
#'   the ANSI-styled string. The third column is the shift that applies to
#'   the string after the coordinates. I.e. If a row in the matrix is
#'   (a, b, c), that means that at position a (in the ANSI-less string),
#'   which is position b in the ANSI string, the shift is c. This shift
#'   applies to all coordinates before the coordinates in the next row.
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
#' @importFrom utils tail

make_ansi_map1 <- function(str) {
  re <- re_exec_all(str, re_ansi())
  .Call("ansistrings_make_ansi_map1", str, re$.match$start[[1]],
        re$.match$end[[1]], re$.match$match[[1]], re$start$start[[1]],
        re$start$match[[1]], re$end$match[[1]], PACKAGE = "ansistrings")
}

## Applies `make_ansi_map` to Each Value In Char Vector
##
## Here for if/when we write an internally vectorized version of this function
## so we can reference it here without having to re-write other code.

#' @rdname make_ansi_map1

make_ansi_map <- function(str) {
  lapply(str, make_ansi_map1)
}
