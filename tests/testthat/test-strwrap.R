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

  # Brainstorming test cases

  a <- "this is a \r string with \r carriage returns in the \rmiddle of it"
  strwrap(a, width=20)

  width <- 60
  width <- -1
  width <- NA
  width <- 1:10
  width <- 10.5

  a <- character()

  a <- "\n"
  a <- "Hello.   There\t. Wow.   This is a spacey  sentence."

  a <- "thisisalongishwordthatwillneedtobesplitsomewhow"
  a <- NA_character_
  a <- NULL
  a <- list("a", NULL, list())

  b <- c(
    paste0("hello ", red("roses"), "there"),
    green("this is a color", bgRed("and another"), "yow")
  )
  
  # Need UTF-8 Test cases
  # Need Double Width Char Test cases
  # Verify whether wrap occurs at or after width characters
  # Make sure BYTES are properly accounted for

})
