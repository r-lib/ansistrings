
re_color         <- "3[0-7]|38;[0-9]+[0-9]+"
re_bgcolor       <- "4[0-7]|48;[0-9]+[0-9]+"
re_bold          <- "1"
re_blurred       <- "2"
re_italic        <- "3"
re_underline     <- "4"
re_inverse       <- "7"
re_hidden        <- "8"
re_strikethrough <- "9"

re_endcolor         <- "39"
re_endbgcolor       <- "49"
re_endboldblurred   <- "22"
re_enditalic        <- "23"
re_endunderline     <- "24"
re_endinverse       <- "27"
re_endhidden        <- "28"
re_endstrikethrough <- "29"
re_endreset         <- "0"

re_ansi <- function() {
  re_start <- paste(
    sep = "|",
    re_color, re_bgcolor, re_bold, re_blurred, re_italic, re_underline,
    re_inverse, re_hidden, re_strikethrough
  )

  re_end <- paste(
    sep = "|",
    re_endcolor, re_endbgcolor, re_endboldblurred, re_enditalic,
    re_endunderline, re_endinverse, re_endhidden, re_endstrikethrough,
    re_endreset
  )

  paste0(
    "\\x{001b}\\[",
    "(?:(?<start>", re_start, ")|(?<end>", re_end, "))",
    "m"
  )

}

re_closes <- function() {

  res <- list(
    list(re_endcolor, re_color),
    list(re_endbgcolor, re_bgcolor),
    list(re_endboldblurred, c(re_bold, re_blurred)),
    list(re_enditalic, re_italic),
    list(re_endunderline, re_underline),
    list(re_endinverse, re_inverse),
    list(re_endhidden, re_hidden),
    list(re_endstrikethrough, re_strikethrough),
    list(re_endreset, c(re_color, re_bgcolor, re_bold, re_blurred,
                        re_italic, re_underline, re_inverse, re_hidden,
                        re_strikethrough))
  )

  structure(
    unlist(lapply(
      res,
      function(x) paste0("^", paste(x[[2]], collapse = "|"), "$")
    )),
    names = unlist(lapply(res, "[[", 1))
  )
}
