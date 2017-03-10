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

  y.col[c(2, 4, 6)] <- crayon::inverse(y.col[c(2, 4, 6)])
  y.paste <- paste(y.col, collapse = "\n\n")
  x.paste <- crayon::strip_style(y.paste)

  # writeLines(ansi_strwrap(y.paste, width = 60))

  expect_true(crayon::has_style(y.paste))
  expect_equal(
    crayon::strip_style(ansi_strwrap(y.paste, width=60)),
    strwrap(x.paste, width = 60)
  )
  # Carriage returns
  s01 <- "this is a \r string with \r carriage returns in the \rmiddle of it"
  expect_equal(ansi_strwrap(s01, 20), strwrap(s01, 20))

  # empty string
  s02 <- character()
  skip('empty string not working yet')
  expect_equal(ansi_strwrap(s02, 20), strwrap(s02, 20))


  if(FALSE) {
    s03 <- "\n"
    s04 <- "Hello.   There\t. Wow.   This is a spacey  sentence."

    s05 <- "thisisalongishwordthatwillneedtobesplitsomewhow"
    s06 <- NA_character_
    s07 <- NULL
    s08 <- list("a", NULL, list())

    a01 <- c(
      paste0("hello ", red("roses"), "there"),
      green("this is a color", bgRed("and another"), "yow")
    )

    writeLines(ansi_strwrap(y.paste, width = 60, indent = 5))
    writeLines(ansi_strwrap(y.paste, width = 60, exdent = 5))
    writeLines(ansi_strwrap(y.paste, prefix = "THANKS> "))

    # Brainstorming test cases

    strwrap(a, width=20)

    width <- 60
    width <- -1
    width <- NA
    width <- 1:10
    width <- 10.5
  }

  # Need UTF-8 Test cases
  # Need Double Width Char Test cases
  # Verify whether wrap occurs at or after width characters
  # Make sure BYTES are properly accounted for

})
