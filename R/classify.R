
classify_ansi_tags <- function(tags) {
  m <- re_match(tags, re_ansi(groups = TRUE))
  m <- m[, setdiff(colnames(m), c("start", "end", ".text", ".match"))]
  col <- apply(m, 1, function(x) which(x != ""))
  colnames(m)[col]
}
