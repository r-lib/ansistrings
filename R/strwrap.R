#' Wrap Character Strings to Format Paragraphs - ANSI Aware
#'
#' This is an ANSI-aware copy of \code{base::strwrap}.  The source code is a
#' copied and lightly modified version of that function.
#'
#' copy stwrap docs here
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

  ## Useful variables.
  indentString <- strrep(" ", indent)
  exdentString <- strrep(" ", exdent)
  y <- list()                         # return value

  ## We use ansi_strsplit() to tokenize input into paras and words, and
  ## hence need to tweak how it handles/transforms encodings.  To
  ## preserve encodings, it seems "best" to canonicalize to UTF-8
  ## (ensuring valid UTF-8), and at the end convert back to latin1
  ## where we originally had latin1.

  enc <- Encoding(x)
  x <- enc2utf8(x)
  if(any(ind <- !validEnc(x)))
  x[ind] <- iconv(x[ind], "UTF-8", "UTF-8", sub = "byte")

  z <- lapply(ansi_strsplit(x, "\n[ \t\n]*\n", perl = TRUE),
  ansi_strsplit, "[ \t\n]", perl = TRUE)

  ## Now z[[i]][[j]] is a character vector of all "words" in
  ## paragraph j of x[i].

  for(i in seq_along(z)) {
    yi <- character()
    for(j in seq_along(z[[i]])) {
      ## Format paragraph j in x[i].
      words <- z[[i]][[j]]
      nc <- ansi_nchar(words, type="w")
      if(anyNA(nc)) {
        ## use byte count as a reasonable substitute
        nc0 <- ansi_nchar(words, type="b")
        nc[is.na(nc)] <- nc0[is.na(nc)]
      }

      ## Remove extra white space unless after a period which
      ## hopefully ends a sentence.
      ## Add ? ! as other possible ends, and there might be
      ## quoted and parenthesised sentences.
      ## NB, input could be invalid here.
      if(any(nc == 0L)) {
        zLenInd <- which(nc == 0L)
        zLenInd <- zLenInd[!(zLenInd %in%
          (grep("[.?!][)\"']{0,1}$", words,
          perl = TRUE, useBytes = TRUE) + 1L))]
        if(length(zLenInd)) {
          words <- words[-zLenInd]
          nc <- nc[-zLenInd]
        }
      }

      if(!length(words)) {
        yi <- c(yi, "", initial)
        next
      }

      currentIndex <- 0L
      lowerBlockIndex <- 1L
      upperBlockIndex <- integer()
      lens <- cumsum(nc + 1L)

      first <- TRUE
      maxLength <- width - ansi_nchar(initial, type="w") - indent

      ## Recursively build a sequence of lower and upper indices
      ## such that the words in line k are the ones in the k-th
      ## index block.
      while(length(lens)) {
        k <- max(sum(lens <= maxLength), 1L)
        if(first) {
          first <- FALSE
          maxLength <- width - ansi_nchar(prefix, type="w") - exdent
        }
        currentIndex <- currentIndex + k
        if(nc[currentIndex] == 0L)
          ## Are we sitting on a space?
          upperBlockIndex <- c(upperBlockIndex,
            currentIndex - 1L)
        else
          upperBlockIndex <- c(upperBlockIndex, currentIndex)
        if(length(lens) > k) {
          ## Are we looking at a space?
          if(nc[currentIndex + 1L] == 0L) {
            currentIndex <- currentIndex + 1L
            k <- k + 1L
          }
          lowerBlockIndex <- c(lowerBlockIndex,
            currentIndex + 1L)
        }
        if(length(lens) > k)
          lens <- lens[-seq_len(k)] - lens[k]
        else
          lens <- NULL
      }

      nBlocks <- length(upperBlockIndex)
      s <- paste0(c(initial, rep.int(prefix, nBlocks - 1L)),
      c(indentString, rep.int(exdentString, nBlocks - 1L)))
      initial <- prefix
      for(k in seq_len(nBlocks))
      s[k] <- paste0(s[k], paste(words[lowerBlockIndex[k] :
        upperBlockIndex[k]],
        collapse = " "))

        yi <- c(yi, s, prefix)
    }
    y <- if(length(yi))
    c(y, list(yi[-length(yi)]))
    else
    c(y, "")
  }

  if(length(pos <- which(enc == "latin1"))) {
    y[pos] <- lapply(
      y[pos],
      function(s) {
        e <- Encoding(s)
        if(length(p <- which(e == "UTF-8")))
        s[p] <- iconv(s[p], "UTF-8", "latin1",
          sub = "byte")
          s
    } )
  }
  if(simplify) y <- as.character(unlist(y))
  y
}
