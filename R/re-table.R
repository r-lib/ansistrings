
re_table <- function (..., perl = TRUE) {
  lapply(gregexpr(..., perl = perl), function(x) {
    res <- data.frame(
      start = x - 1L,
      end = x + attr(x, "match.length") - 2L
      )
    res <- res[res[, "start"] != -2, , drop = FALSE]
  })
}
