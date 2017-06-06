
re_255           <- "(?:[01]?[0-9]?[0-9]|2[0-4][0-9]|25[0-5])"

re_color_8       <- "3[0-7]"
re_g_color_8     <- sprintf("(?<color_8>%s)", re_color_8)

re_bgcolor_8     <- "4[0-7]"
re_g_bgcolor_8   <- sprintf("(?<bgcolor_8>%s)", re_bgcolor_8)

re_color_256     <- sprintf("38;5;%s", re_255)
re_g_color_256   <- sprintf("(?<color_256>%s)", re_color_256)

re_bgcolor_256   <- sprintf("48;5;%s", re_255)
re_g_bgcolor_256 <- sprintf("(?<bgcolor_256>%s)", re_bgcolor_256)

re_color_rgb     <- sprintf("38;2;%s;%s;%s", re_255, re_255, re_255)
re_g_color_rgb   <- sprintf("(?<color_rgb>%s)", re_color_rgb)

re_bgcolor_rgb   <- sprintf("48;2;%s;%s;%s", re_255, re_255, re_255)
re_g_bgcolor_rgb <- sprintf("(?<bgcolor_rgb>%s)", re_bgcolor_rgb)

re_color         <- sprintf("(?:%s|%s|%s)", re_color_8, re_color_256,
                            re_color_rgb)
re_g_color       <- sprintf("(?:%s|%s|%s)", re_g_color_8, re_g_color_256,
                            re_g_color_rgb)

re_bgcolor       <- sprintf("(?:%s|%s|%s)", re_bgcolor_8, re_bgcolor_256,
                            re_bgcolor_rgb)
re_g_bgcolor     <- sprintf("(?:%s|%s|%s)", re_g_bgcolor_8,
                            re_g_bgcolor_256, re_g_bgcolor_rgb)

re_bold          <- "1"
re_g_bold        <- "(?<bold>1)"

re_blurred       <- "2"
re_g_blurred     <- "(?<blurred>2)"

re_italic        <- "3"
re_g_italic      <- "(?<italic>3)"

re_underline     <- "4"
re_g_underline   <- "(?<underline>4)"

re_inverse       <- "7"
re_g_inverse     <- "(?<inverse>7)"

re_hidden        <- "8"
re_g_hidden      <- "(?<hidden>8)"

re_strikethrough   <- "9"
re_g_strikethrough <- "(?<strikethrough>9)"

re_endcolor         <- "39"
re_endbgcolor       <- "49"
re_endboldblurred   <- "22"
re_enditalic        <- "23"
re_endunderline     <- "24"
re_endinverse       <- "27"
re_endhidden        <- "28"
re_endstrikethrough <- "29"
re_endreset         <- "0"

#' Create a regular expression that matches ANSI tags
#'
#' It uses groups to distinguish between start and end tags, and
#' it also keeps the actual command string within the ANSI tag. E.g.
#' `"1"` means a *bold* start tag.
#'
#' @return A character scalar, the regular expression.
#' @keywords internal

re_ansi <- function(groups = FALSE) {

  if (groups) {
    re_start <- paste(
      sep = "|",
      re_g_color, re_g_bgcolor, re_g_bold, re_g_blurred, re_g_italic,
      re_g_underline, re_g_inverse, re_g_hidden, re_g_strikethrough
    )

  } else {
    re_start <- paste(
      sep = "|",
      re_color, re_bgcolor, re_bold, re_blurred, re_italic, re_underline,
      re_inverse, re_hidden, re_strikethrough
    )
  }

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

#' Which ANSI end tag closes which ANSI start tag?
#'
#' Create a map that answers this question.
#'
#' @return A named list, the names are the ANSI end tag command strings.
#'   The list contains character vectors, specifying the start tags that
#'   are closed by each end tag. Most end tags close a single start tag,
#'   but some close multiple ones, and the *reset* end tag closes all
#'   start tags.
#'
#' @keywords internal

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
