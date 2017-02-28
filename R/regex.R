
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

#' Create a regular expression that matches ANSI tags
#'
#' It uses groups to distinguish between start and end tags, and
#' it also keeps the actual command string within the ANSI tag. E.g.
#' `"1"` means a *bold* start tag.
#'
#' @return A character scalar, the regular expression.
#' @keywords internal

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
