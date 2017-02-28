
context("Strsplit")

## test_that("ansi_strsplit", {
##   red <- "\033[31mred\033[39m"

##   str <- "plain-plain"
##   expect_equal(ansi_strsplit(str, "-"), strsplit(str, "-"))

##   str <- red %+% "-plain"
##   expect_equal(strip_style(ansi_strsplit(str, "-")[[1]]),
##                strsplit(strip_style(str), "-")[[1]])

##   expect_equal(ansi_strsplit(str, "e"),
##                list(c("\033[31mr\033[39m", "\033[31md\033[39m-plain")))

##   str <- red %+% "-" %+% red %+% "-" %+% red
##   expect_equal(strip_style(ansi_strsplit(str, "-")[[1]]),
##                strsplit(strip_style(str), "-")[[1]])

##   # with leading and trailing separators
##   str.2 <- "-" %+% red %+% "-" %+% red %+% "-" %+% red %+% "-"
##   expect_equal(strip_style(ansi_strsplit(str.2, "-")[[1]]),
##                strsplit(strip_style(str.2), "-")[[1]])

##   # greater than length 1
##   str.3 <- paste0("-", c(green("hello"), red("goodbye")), "-world-")
##   expect_equal(strip_style(unlist(ansi_strsplit(str.3, "-"))),
##                unlist(strsplit(strip_style(str.3), "-")))
## })

## test_that("ansi_strsplit multiple strings", {
##   red <- "\033[31mred\033[39m"
##   str <- c("plain-plain-" %+% red %+% "-plain-" %+% red,
##            red %+% "-" %+% red,
##            red)

##   r1 <- lapply(ansi_strsplit(str, "-"), strip_style)
##   r2 <- strsplit(strip_style(str), "-")

## })

## test_that("ansi_strsplit edge cases", {
##   expect_equal(ansi_strsplit("", "-"), list(character(0L)))
##   expect_equal(
##     strip_style(ansi_strsplit("\033[31m\033[39m", "-")[[1]]), character(0L)
##   )
##   # special cases
##   expect_equal(ansi_strsplit("", ""), strsplit("", ""))
##   expect_equal(ansi_strsplit("a", "a"), strsplit("a", "a"))
##   # this following test isn't working yet
##   expect_equal(ansi_strsplit("a", ""), strsplit("a", ""))
##   expect_equal(ansi_strsplit("", "a"), strsplit("", "a"))
##   # Longer strings
##   expect_identical(
##     ansi_strsplit(c("", "a", "aa"), "a"), strsplit(c("", "a", "aa"), "a")
##   )
##   expect_identical(
##     ansi_strsplit(c("abaa", "ababza"), "b."), strsplit(c("abaa", "ababza"), "b.")
##   )
## })

## test_that("Weird length 'split'", {
##   expect_error(ansi_strsplit(c("ab", "bd"), c("b", "d")), "must be character")
##   expect_identical(ansi_strsplit("ab", NULL), strsplit("ab", NULL))
##   expect_identical(
##     ansi_strsplit("ab", character(0L)), strsplit("ab", character(0L))
##   )
## })
