
context("Substrings")

str <- c("",
         "plain",
         "\033[31m",
         "\033[39m",
         "\033[31mred\033[39m",
         "\033[31mred\033[39m\033[31mred\033[39m",
         "foo\033[31mred\033[39m",
         "\033[31mred\033[39mfoo")

test_that("ansi_substr", {
  for (s in str) {
    for (i in 1 %:% ansi_nchar(s)) {
      for (j in i %:% ansi_nchar(s)) {
        expect_equal(strip_style(ansi_substr(s, i, j)),
                     substr(strip_style(s), i, j), info = paste(s, i, j))
      }
    }
  }
})


test_that("ansi_substr keeps color", {
  expect_equal(ansi_substr("\033[31mred\033[39m", 1, 1),
               "\033[31mr\033[39m")
  expect_equal(ansi_substr("foo\033[31mred\033[39m", 4, 4),
               "\033[31mr\033[39m")
  expect_equal(ansi_substr("foo\033[31mred\033[39mbar", 4, 4),
               "\033[31mr\033[39m")
  expect_equal(ansi_substr("\033[31mred\033[39mfoo\033[31mred\033[39mbar", 7, 7),
               "\033[31mr\033[39m")
})

test_that("ansi_substr, start after string end", {
  expect_equal(ansi_substr("red", 4, 4), "")
  expect_equal(ansi_substr("red", 4, 5), "")
  expect_equal(strip_style(ansi_substr("\033[31mred\033[39m", 4, 4)), "")
  expect_equal(strip_style(ansi_substr("\033[31mred\033[39m", 4, 5)), "")

  expect_equal(ansi_substr("red", 3, 4), "d")
  expect_equal(ansi_substr("red", 3, 5), "d")
  expect_equal(strip_style(ansi_substr("\033[31mred\033[39m", 3, 4)), "d")
  expect_equal(strip_style(ansi_substr("\033[31mred\033[39m", 3, 5)), "d")
})

test_that("ansi_substr, multiple strings", {
  set.seed(42)
  for (i in 1:100) {
    strs <- sample(str, 4)
    num_starts <- sample(1:5, 1)
    num_stops <- sample(1:5, 1)
    starts <- sample(1:5, num_starts, replace = TRUE)
    stops <- sample(1:5, num_stops, replace = TRUE)
    r1 <- strip_style(ansi_substr(strs, starts, stops))
    r2 <- substr(strip_style(strs), starts, stops)
    expect_equal(r1, r2)
  }
})

test_that("ansi_substr corner cases", {
  # Zero length input

  c0 <- character(0L)
  o0 <- structure(list(), class="abc")
  co0 <- structure(character(0L), class="abc")
  expect_identical(ansi_substr(c0, 1, 1), substr(c0, 1, 1))
  expect_identical(ansi_substr(o0, 1, 1), substr(o0, 1, 1))
  expect_identical(ansi_substr(co0, 1, 1), substr(co0, 1, 1))

  expect_identical(ansi_substring(c0, 1, 1), substring(c0, 1, 1))
  expect_identical(ansi_substring(o0, 1, 1), substring(o0, 1, 1))
  expect_identical(ansi_substring(co0, 1, 1), substring(co0, 1, 1))

  # Character start/stop
  expect_identical(ansi_substr("abc", "1", 1), substr("abc", "1", 1))
  expect_identical(ansi_substr("abc", 1, "1"), substr("abc", 1, "1"))

  # non-numeric arguments cause errors; NOTE: this actually "works"
  # with 'substr' but not implemented in 'ansi_substr'
  expect_error(ansi_substr("abc", "hello", 1), "non-numeric")

})

test_that("ansi_substring", {
  for (s in str) {
    for (i in 1 %:% ansi_nchar(s)) {
      for (j in i %:% ansi_nchar(s)) {
        expect_equal(strip_style(ansi_substring(s, i, j)),
                     substring(strip_style(s), i, j), info = paste(s, i, j))
      }
    }
  }
})

test_that("ansi_substring, multiple strings", {
  set.seed(42)
  for (i in 1:100) {
    strs <- sample(str, 4)
    num_starts <- sample(1:5, 1)
    num_stops <- sample(1:5, 1)
    starts <- sample(1:5, num_starts, replace = TRUE)
    stops <- sample(1:5, num_stops, replace = TRUE)
    r1 <- strip_style(ansi_substring(strs, starts, stops))
    r2 <- substring(strip_style(strs), starts, stops)
    expect_equal(r1, r2)
  }
})

test_that("ansi_substring corner cases", {
  # Zero length input

  c0 <- character(0L)
  o0 <- structure(list(), class="abc")
  co0 <- structure(character(0L), class="abc")
  expect_identical(ansi_substring(c0, 1, 1), substring(c0, 1, 1))
  expect_identical(ansi_substring(o0, 1, 1), substring(o0, 1, 1))
  expect_identical(ansi_substring(co0, 1, 1), substring(co0, 1, 1))
})
