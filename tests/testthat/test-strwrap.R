context('strwrap')

test_that('strwrap examples', {
  ## Taken from `base::strwrap` examples
  ##
  ## Read in file 'THANKS'.
  x <- paste(readLines(file.path(R.home("doc"), "THANKS")), collapse = "\n")
  ## Split into paragraphs and remove the first three ones
  x <- unlist(strsplit(x, "\n[ \t\n]*\n"))[-(1:3)]
  ## Pull out the short paragraphs

  y <- tail(x, -1L)

  # We will test by adding coloring to the original test string, and confirming
  # that it ends up the same.  Let's color anything that starts with an upper
  # case letter

  uc.first.loc <- gregexpr("\\b[A-Z]\\w*\\b", y)
  uc.first <- regmatches(y, uc.first.loc)
  y.col <- y
  regmatches(y.col, uc.first.loc) <- lapply(uc.first, crayon::red)

  # and color some entire sentences

  y.col[c(2, 4, 6)] <- crayon::bgBlue(y.col[c(2, 4, 6)])
  y.paste <- paste(y.col, collapse = "\n\n")

  writeLines(ansi_strwrap(y.paste, width = 60))
  writeLines(ansi_strwrap(y.paste, width = 60, indent = 5))
  writeLines(ansi_strwrap(y.paste, width = 60, exdent = 5))
  writeLines(ansi_strwrap(y.paste, prefix = "THANKS> "))

})
