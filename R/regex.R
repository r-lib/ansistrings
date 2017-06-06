
re_255           <- "(?:[01]?[0-9]?[0-9]|2[0-4][0-9]|25[0-5])"

re_color_8       <- "(?<color_8>3[0-7])"
re_bgcolor_8     <- "(?<bgcolor_8>4[0-7])"

re_color_256     <- sprintf("(?<color_256>38;5;%s)", re_255)
re_bgcolor_256   <- sprintf("(?<bgcolor_256>48;5;%s)", re_255)

re_color_rgb     <- sprintf("(?<color_rgb>38;2;%s;%s;%s)", re_255,
                            re_255, re_255)
re_bgcolor_rgb   <- sprintf("(?<bgcolor_rgb>48;2;%s;%s;%s)", re_255,
                            re_255, re_255)

re_color         <- sprintf("(?:%s|%s|%s)", re_color_8, re_color_256,
                            re_color_rgb)
re_bgcolor       <- sprintf("(?:%s|%s|%s)", re_bgcolor_8, re_bgcolor_256,
                            re_bgcolor_rgb)

re_bold          <- "(?<bold>1)"
re_blurred       <- "(?<blurred>2)"
re_italic        <- "(?<italic>3)"
re_underline     <- "(?<underline>4)"
re_inverse       <- "(?<inverse>7)"
re_hidden        <- "(?<hidden>8)"
re_strikethrough <- "(?<strikethrough>9)"

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

  ## Removed the named capture groups from a regular expression,
  ## to speed up matching and constructing the result object
  no_groups <- function(x) gsub("\\?<[a-zA-Z_]+>", "", x)

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

  if (!groups) {
    re_start <- no_groups(re_start)
    re_end   <- no_groups(re_end)
  }

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
