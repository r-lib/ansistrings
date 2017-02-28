
context("Nchar")

str <- c("",
         "plain",
         "\033[31m",
         "\033[39m",
         "\033[31mred\033[39m",
         "\033[31mred\033[39m\033[31mred\033[39m",
         "foo\033[31mred\033[39m",
         "\033[31mred\033[39mfoo")

test_that("ansi_nchar", {
  for (s in str) {
    expect_equal(ansi_nchar(s), nchar(strip_style(s)), info = s)
  }
})
