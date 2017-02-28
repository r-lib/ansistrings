
context("ANSI mappings")

test_that("make_shifts1", {
  str <- paste("pre", crayon::red("red"), crayon::bold("bold"), "post")
  obj <- make_shifts1(rematch2::re_exec_all(str, re_ansi()))
  exp <- cbind( c(5,8,9,13), c(5,13,19,27), c(5,10,14,19))
  expect_equal(obj, exp)
})

test_that("map_raw_to_ansi1, map_ansi1_to_raw", {

  str <- paste("pre", crayon::red("red"), crayon::bold("bold"), "post")
  map <- make_ansi_map1(str)

  cases <- list(
    list(1, 1),  list(2, 2), list(3, 3), list(4, 4),
    list(5, 10), list(6, 11), list(7, 12),
    list(8, 18),
    list(9, 23), list(10, 24), list(11, 25), list(12, 26),
    list(13, 32), list(14, 33), list(15, 34), list(16, 35), list(17, 36),
    ## This is longer than the string, but it should work
    list(18, 37)
  )

  for (c in cases) {
    expect_equal(map_raw_to_ansi1(map, c[[1]]), c[[2]], info = paste(c[[1]]))
  }

  for (c in cases) {
    expect_equal(map_ansi_to_raw1(map, c[[2]]), c[[1]], info = paste(c[[2]]))
  }
})

test_that("make_ansi_map1", {
  str <- paste("pre", crayon::red("red"), crayon::bold("bold"), "post")
  map <- make_ansi_map1(str)
  exp <- list(
    map = data.frame(
      stringsAsFactors = FALSE,
      start = c(5, 9),
      end = c(7, 12),
      open = c("\033[31m", "\033[1m"),
      close = c("\033[39m", "\033[22m")
    ),
    shifts = cbind(c(5, 8, 9, 13), c(5, 13, 19, 27), c(5, 10, 14, 19))
  )
  expect_equal(map, exp)
})

test_that("make_ansi_map with unclosed tags", {
  str <- "pre \033[31mred \033[1mbold\033[22m post"
  map <- make_ansi_map1(str)
  exp <- data.frame(
    stringsAsFactors = FALSE,
    start = c(5, 9),
    end = c(18, 12),
    open = c("\033[31m", "\033[1m"),
    close = c("", "\033[22m")
  )
  expect_equal(map$map, exp)
})

test_that("make_ansi_map1 corner cases", {
  empty_map <- data.frame(
    stringsAsFactors = FALSE,
    start = numeric(),
    end = numeric(),
    open = character(),
    close = character()
  )
  expect_equal(make_ansi_map1(""), empty_map)
  expect_equal(make_ansi_map1("x"), empty_map)
  expect_equal(make_ansi_map1("foobar"), empty_map)
})

test_that("make_ansi_map", {
  str1 <- paste("pre", crayon::red("red"), crayon::bold("bold"), "post")
  str2 <- paste("another", crayon::italic("one"))

  expect_equal(
    make_ansi_map(c(str1, str2)),
    list(make_ansi_map1(str1), make_ansi_map1(str2))
  )
})
