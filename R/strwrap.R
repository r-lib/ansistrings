#' Wrap Character Strings to Format Paragraphs - ANSI Aware
#'
#' This is an ANSI-aware copy of \code{base::strwrap}.
#'
#' @section Whitespace:
#'
#' Tabs are treated as single spaces, space sequences are treated as single
#' spaces except that those following end of sentence markers (i.e. \sQuote{.},
#' \sQuote{?}, or \sQuote{!} will be treated as two spaces if they are two
#' spaces or longer.  Form feeds and new lines are treated as newlines.  All
#' other whitespace, including but not limited to vertical tabs, carriage
#' returns, etc., are treated as if they occupy one screen character.
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
  message("add encoding handling")

  # Replace all multi spaces with a single space, except if they happen to be
  # two spaces following a period, question mark, or exclamation point that
  # themselves appear to be at the end of a word.

  x <- gsub("\t", " ", x, fixed=TRUE)
  x.1 <- gsub('(?<=\\w)([.!?] ) +| +', '\\1 ', x, perl=TRUE)

  # Split by newlines, and establish mapping of each element back to the
  # original vector spot so we can use it when `simplify=FALSE`

  x.s <- ansi_strsplit(x.1, "\n\f")
  x.s.len <- vapply(x.s, length, integer(1L))
  x.s.ul <- unlist(x.s)

  # Get location of spaces in entire vector; doing it here in the hopes that
  # with C vetorized mapping code it will be faster to do it this way

  x.spaces <- gregexpr(" +", x.s.ul)

  # Word wrap each element

  x.wrap.l <- Map(x.s.ul, elem_wrap, width=width)

  # re-list if not simplified

  x.res <- if(simplify) {
    unlist(x.wrap.l)
  } else {
    x.s.groups <- rep(seq_along(x.s), x.s.len)
    x.wrap.l.s <- split(x.wrap.l, x.s.groups)
    unname(lapply(x.wrap.l.s, unlist))
  }
  x.res
}
## Helper Function for `strwrap`
##
## Wraps a single element.  Assumes only space characters within are actual
## spaces since the calling function gets rid of tabs, newlines, etc.

elem_wrap <- function(elem, width) {
  stopifnot(length(elem) == 1L)

  # allocate to maximum possible size

  res <- character(ceiling(ansi_nchar(elem) / width * 2))
  res.i <- 1L

  # Iterativevly copy chunks until elem is right size

  while(ansi_nchar(elem) > width) {
    # 

  }



}

#' @export

word_wrap <- function(
  x, width=getOption("width"), tolerance=8L, hyphens=TRUE, unlist=TRUE,
  collapse=NULL
) {
  stopifnot(
    is.character(x),
    length(width) == 1L && !is.na(width) && is.numeric(width),
    is.integer(tolerance) && length(tolerance) == 1L && !is.na(tolerance) &&
    tolerance >= 0L,
    is.null(collapse) || is.chr1(collapse)
  )
  width <- as.integer(width)

  if(!(width > 4L && width - tolerance > 2L)) {
    warning(
      "Display width too narrow to properly wrap text; setting to 80L"
    )
    width <- 80L
    tolerance <- 8L
  }
  width <- as.integer(width)

  # Define patterns, should probably be done outside of function

  break_char <- function(x) {
    # Allocate worst case vector, which is 2x as long as the input where we have
    # a one letter word we can wrap and then something we can't wrap

    res <- character(2 * nchar(x) / width * 2)
    res.idx <- 1
    spc.ptrn <- sprintf(base.ptrn, "\\s")

    if(!nchar(x)) return(x)
    while(nchar(x)) {
      pad <- 0L  # account for hyphen
      if(nchar(x) > width) {
        x.sub <- substr(x, 1L, width + 1L)
        x.trim <- sub(spc.ptrn, "\\1", x.sub, perl=TRUE)

        # Look for spaces in truncated string

        matched <- grepl(spc.ptrn, x.sub, perl=TRUE)
        if(!matched) x.trim <- substr(x, 1L, width)  # Failed, truncate

        # we allow one extra char for pattern match some cases, remove here

        x.trim <- substr(x.trim, 1L, width)

        # remove leading space if any

        x <- sub(
          "^\\s(.*)", "\\1",
          substr(x, min(nchar(x.trim), width) + 1L - pad, nchar(x)),
          perl=TRUE
        )
      } else {
        x.trim <- x
        x <- ""
      }
      res[[res.idx]] <- x.trim
      res.idx <- res.idx + 1L
    }
    res[1L:(res.idx - 1L)]
  }
  # x.lst workaround required because `strsplit` swallows zero char char items!!

  x.lst <- as.list(x)

  # replace new lines with 0 char item; note that leading NLs need special
  # treatment; used to put in two newlines here; not sure why though

  x.lst[nchar(x) > 0] <- strsplit(x[nchar(x) > 0], "\n")

  res <- lapply(x.lst, function(x) unlist(lapply(x, break_char)))
  res.fin <- if(unlist) unlist(res) else res
  if(!is.null(collapse)) {
    res.fin <- if(is.list(res.fin))
      lapply(res.fin, paste0, collapse=collapse) else
        paste0(res.fin, collapse=collapse)
  }
  res.fin
}
