context('strwrap')

test_that('strwrap examples', {
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


   expect_equal(
     strwrap(mk_par(y[1:2]), width = 60),
     c(
       "Many more, too numerous to mention here, have contributed",
       "by sending bug reports and suggesting various improvements.",
       "",
       "Simon Davies whilst at the University of Auckland wrote the",
       "original version of glm()."
   ) )

   ## Join the rest
   x <- paste(x, collapse = "\n\n")
   ## Now for some fun:

   ## Note that messages are wrapped AT the target column indicated by
   ## 'width' (and not beyond it).
   ## From an R-devel posting by J. Hosking <jh910@juno.com>.
   x <- paste(sapply(sample(10, 100, replace = TRUE),
              function(x) substring("aaaaaaaaaa", 1, x)), collapse = " ")
   sapply(10:40,
          function(m)
          c(target = m, actual = max(nchar(strwrap(x, m)))))
})
