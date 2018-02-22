
context("ANSI regexes")

test_that("Issue #5", {
  yy <- "\033[48;5;114mhello world\033[49m"
  expect_equal(
    gsub(re_ansi(), "", yy, perl = TRUE),
    "hello world"
  )

  zz <- '\033[31;47mHello\033[0m\n'
  re <- re_match_all(zz, re_ansi())
  skip("Multiple commands in one tag are not supported yet")
  expect_equal(re$start, list("31;47"))
  expect_equal(re$end, list("0"))
})
