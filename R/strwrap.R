#' Wrap Character Strings to Format Paragraphs - ANSI Aware
#'
#' This is an ANSI-aware copy of \code{base::strwrap}.
#'
#' @section Whitespace:
#'
#' Tabs, new lines, and form feeds  are treated as single spaces, space
#' sequences are treated as single spaces except that those following end of
#' sentence markers (i.e. \sQuote{.}, \sQuote{?}, or \sQuote{!} will be treated
#' as two spaces if they are two spaces or longer.  All other whitespace,
#' including but not limited to vertical tabs, carriage returns, etc., are
#' treated as if they occupy one screen character.
#'
#' @export
#' @param x: a character vector, or an object which can be converted to a
#'   character vector by \sQuote{as.character}.
#' @param width: a positive integer giving the target column for wrapping
#'   lines in the output.
#' @param indent: a non-negative integer giving the indentation of the first
#'   line in a paragraph.
#' @param exdent: a non-negative integer specifying the indentation of
#'   subsequent lines in paragraphs.
#' @param prefix: a character string to be used as prefix for each line
#'   except the first, for which \sQuote{initial} is used.
#' @param initial: a character string to be used as prefix for the first line
#' @param simplify: a logical.  If \sQuote{TRUE}, the result is a single
#'   character vector of line text; otherwise, it is a list of the same length
#'   as \code{x} the elements of which are character vectors of line text
#'   obtained from the corresponding element of \code{x}.  (Hence, the result in
#'   the former case is obtained by unlisting that of the latter.)

ansi_strwrap <- function(
  x, width = 0.9 * getOption("width"), indent = 0, exdent = 0,
  prefix = "", simplify = TRUE, initial = prefix
) {
  if(!is.character(x)) x <- as.character(x)

  # Split by paragraph, and collect info required to re-assemble, note we add an
  # empty string at end of each paragraph to mimic strsplit

  x.s <- lapply(
    ansi_strsplit(x, "\n[ \t\n]*\n", perl=TRUE),
    function(y)
     head(c(y, character(length(y)))[order(rep(seq_along(y), 2L))], -1L)
  )
  x.s.len <- vapply(x.s, length, integer(1L))
  x.s.ul <- unlist(x.s)

  # Replace all multi spaces with a single space, except if they happen to be
  # two spaces following a period, question mark, or exclamation point that
  # themselves appear to be at the end of a word.

  x.clean.tmp <- gsub("\t|\n|\f", " ", x.s.ul, perl=TRUE)
  x.clean <- gsub('(?<=\\w)([.!?] ) +| +', '\\1 ', x.clean.tmp, perl=TRUE)

  # Get location of spaces in entire vector; doing it here in the hopes that
  # with C vetorized mapping code it will be faster to do it this way instead of
  # for each individual CHRSXP

  x.spaces <- gregexpr(" +", x.clean)
  x.map <- make_ansi_map(x.clean)

  # Get coordinates for wrapping element.  We do this so we can just have one
  # `ansi_substr` call in case we internally vectorize that

  x.wrap.dat <- Map(elem_wrap, x.clean, spaces=x.spaces, map=x.map, width=width)

  # Replicate our input vector so we can substr with our coords

  wrap.lens <- vapply(x.wrap.dat, nrow, integer(1L))
  x.clean.rep <- rep(x.clean.tmp, wrap.lens)
  sub.coords <- do.call(rbind, x.wrap.dat)

  x.wrap.strings <- ansi_substr(x.clean.rep, sub.coords[, 1], sub.coords[, 2])

  # Re list if not simplifying

  x.res <- if(simplify) {
    unlist(x.wrap.strings)
  } else {
    x.s.groups <- rep(seq_along(x.s), x.s.len)
    x.wrap.strings.s <- split(x.wrap.strings, x.s.groups)
    unname(lapply(x.wrap.strings.s, unlist))
  }
  x.res
}
## Helper Function for `strwrap`
##
## Wraps a single element.  Assumes only space characters within are actual
## spaces since the calling function gets rid of tabs, newlines, etc.
##
## @param elem character scalar
## @param spaces list data from `regexpr` match for spaces in our vector
## @param map raw to ansi mapping data
## @param width wrap width

elem_wrap <- function(elem, spaces, map, width) {
  stopifnot(length(elem) == 1L)

  # If no spaces can't wrap

  elem.orig <- elem # copy (for debugging) since we're going to modify elem
  el.chars <- ansi_nchar(elem)

  if(length(spaces) && el.chars > width) {
    # allocate res to maximum possible size, will reduce later if needed

    res <- matrix(0L, nrow=ceiling(el.chars / width * 2), ncol=2L)
    res.i <- 1L
    elem.i <- 1L

    # Remap space position to what they would be without ansi tags (raw).
    # Need `space.len` for the two spaces following periods, etc.

    space.raw <- space.raw.orig <-
      vapply(spaces, map_ansi_to_raw1, integer(1L), map=map)
    space.len <- attr(spaces, 'match.length')

    repeat {
      # Try to find a space inside width, and if not, next one after that (the
      # max(..., 1L) handles this fail over case).  Note that `space.raw` is
      # always relative to `elem.i` since we adjust it in each loop iteration so
      # that we can always compare directly to width

      target <- max(c(which(space.raw <= width), 1L))

      # get the coords for the string up to just before that space, and then 
      # move our indices forward

      res[res.i, ] <- c(elem.i, space.raw[target] + elem.i - 2L)
      space.offset <- space.raw[target] + space.len[target]
      elem.i <- elem.i + space.offset - 1L

      space.raw <- tail(space.raw, -target) - space.offset + 1L
      space.len <- tail(space.len, -target)
      res.i <- res.i + 1

      # End case

      if(!length(space.raw) || (el.chars - elem.i + 1 <= width)) {
        res[res.i, ] <- c(elem.i, el.chars)
        break
      }
    }
    head(res, res.i)
  } else {
    matrix(c(1L, el.chars), ncol=2L)
  }
}
