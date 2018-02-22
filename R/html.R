
#' Convert ANSI highlighted strings to HTML
#'
#' @param text Character vector of ANSI highlighted strings.
#' @param fullpage Whether to include a minimal HTML header and footer
#'   in the output.
#' @param collapse Whether to collapse the character vector to a single
#'   string before the conversion.
#' @return Character vector of HTML text.
#'
#' @export

ansi_to_html <- function(text, fullpage = TRUE, collapse = TRUE) {
  if (collapse) text <- paste(text, collapse = "\n")
  html <- vapply(text, ansi_to_html1, character(1), USE.NAMES = FALSE)
  if (fullpage) html <- paste0(
    '<html>
       <head>
         <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
         <style type="text/css">
          @font-face {{
             font-family: "Menlo";
             src: url("Menlo-Regular.ttf") format("truetype");
          }}
          pre {{
            font-family: Menlo
          }}
         </style>
       </head>
       <body>
         <pre id="content">\n',
    html, '
         </pre>
       </body>
       </html>'
  )

  structure(html, class = "html")
}

#' @export

knit_print.html <- function(x, zoom = 2, ...) {
  html <- ansi_to_html(x, fullpage = TRUE, collapse = TRUE)
  html_file <- tempfile(fileext = ".html")
  font_file <- file.path(dirname(html_file), "Menlo-Regular.ttf")
  file.copy(
    system.file(package = "ansistrings", "Menlo-Regular.ttf"),
    font_file
  )
  on.exit(unlink(c(html_file, font_file)), add = TRUE)
  image_file <- tempfile(fileext = ".png")
  on.exit(unlink(image_file), add = TRUE)
  cat(html, file = html_file)
  webshot::webshot(html_file, image_file, selector = "#content",
                   zoom = zoom)
  img <- readBin(image_file, "raw", file.info(image_file)[, "size"])
  structure(
    list(image = img, extension = ".png", url = NULL),
    class = "html_screenshot"
  )
}

#' @importFrom crayon strip_style

ansi_to_html1 <- function(str) {
  map <- make_ansi_map1(str)$map
  if (!length(map$start)) return(str)

  map <- map[map$start <= map$end, , drop = FALSE]

  tags <- map$open
  tag_types <- classify_ansi_tags(tags)
  html_start_tags <- vapply(
    seq_along(tags),
    function(i) ansi_tag_to_html(get_tag_core(tags[i]), tag_types[i]),
    character(1)
  )
  html_end_tags <- rep("</span>", length(html_start_tags))

  pos <- c(rbind(map$start, map$end + 1))
  ins <- c(rbind(html_start_tags, html_end_tags))

  ## Order is stable, that is important, because we want the closing
  ## tags to precede the opening tags
  ord <- order(pos)
  pos <- pos[ord]
  ins <- ins[ord]

  str_insert_parallel(strip_style(str), pos, ins)
}

get_tag_core <- function(tag) {
  gsub("^\033\\[(.*)m$", "\\1", tag, perl = TRUE)
}

ansi_tag_to_html <- function(tag, type) {
  switch(
    type,
    color_8       = color_8_to_html(tag),
    bgcolor_8     = bgcolor_8_to_html(tag),
    color_256     = color_256_to_html(tag),
    bgcolor_256   = bgcolor_256_to_html(tag),
    color_rgb     = color_rgb_to_html(tag),
    bgcolor_rgb   = bgcolor_rgb_to_html(tag),
    bold          = bold_to_html(tag),
    blurred       = blurred_to_html(tag),
    italic        = italic_to_html(tag),
    underline     = underline_to_html(tag),
    inverse       = inverse_to_html(tag),
    hidden        = hidden_to_html(tag),
    strikethrough = strikethrough_to_html(tag),
    stop("Unknown ANSI tag found, cannot convert to HTML")
  )
}

html_color_8 <- function(x) {
  c("30" = "#2e3436",
    "31" = "#cc0000",
    "32" = "#4e9a06",
    "33" = "#c4a000",
    "34" = "#3465a4",
    "35" = "#75507b",
    "36" = "#06989a",
    "37" = "#d3d7cf",
    "40" = "#2e3436",
    "41" = "#cc0000",
    "42" = "#4e9a06",
    "43" = "#c4a000",
    "44" = "#3465a4",
    "45" = "#75507b",
    "46" = "#06989a",
    "47" = "#d3d7cf",
    "90" = "#555753"
    )[[x]]
}

html_color_256 <- function(x) {
  c("#000000", "#800000", "#008000", "#808000", "#000080", "#800080",
    "#008080", "#c0c0c0", "#808080", "#ff0000", "#00ff00", "#ffff00",
    "#0000ff", "#ff00ff", "#00ffff", "#ffffff", "#000000", "#00005f",
    "#000087", "#0000af", "#0000d7", "#0000ff", "#005f00", "#005f5f",
    "#005f87", "#005faf", "#005fd7", "#005fff", "#008700", "#00875f",
    "#008787", "#0087af", "#0087d7", "#0087ff", "#00af00", "#00af5f",
    "#00af87", "#00afaf", "#00afd7", "#00afff", "#00d700", "#00d75f",
    "#00d787", "#00d7af", "#00d7d7", "#00d7ff", "#00ff00", "#00ff5f",
    "#00ff87", "#00ffaf", "#00ffd7", "#00ffff", "#5f0000", "#5f005f",
    "#5f0087", "#5f00af", "#5f00d7", "#5f00ff", "#5f5f00", "#5f5f5f",
    "#5f5f87", "#5f5faf", "#5f5fd7", "#5f5fff", "#5f8700", "#5f875f",
    "#5f8787", "#5f87af", "#5f87d7", "#5f87ff", "#5faf00", "#5faf5f",
    "#5faf87", "#5fafaf", "#5fafd7", "#5fafff", "#5fd700", "#5fd75f",
    "#5fd787", "#5fd7af", "#5fd7d7", "#5fd7ff", "#5fff00", "#5fff5f",
    "#5fff87", "#5fffaf", "#5fffd7", "#5fffff", "#870000", "#87005f",
    "#870087", "#8700af", "#8700d7", "#8700ff", "#875f00", "#875f5f",
    "#875f87", "#875faf", "#875fd7", "#875fff", "#878700", "#87875f",
    "#878787", "#8787af", "#8787d7", "#8787ff", "#87af00", "#87af5f",
    "#87af87", "#87afaf", "#87afd7", "#87afff", "#87d700", "#87d75f",
    "#87d787", "#87d7af", "#87d7d7", "#87d7ff", "#87ff00", "#87ff5f",
    "#87ff87", "#87ffaf", "#87ffd7", "#87ffff", "#af0000", "#af005f",
    "#af0087", "#af00af", "#af00d7", "#af00ff", "#af5f00", "#af5f5f",
    "#af5f87", "#af5faf", "#af5fd7", "#af5fff", "#af8700", "#af875f",
    "#af8787", "#af87af", "#af87d7", "#af87ff", "#afaf00", "#afaf5f",
    "#afaf87", "#afafaf", "#afafd7", "#afafff", "#afd700", "#afd75f",
    "#afd787", "#afd7af", "#afd7d7", "#afd7ff", "#afff00", "#afff5f",
    "#afff87", "#afffaf", "#afffd7", "#afffff", "#d70000", "#d7005f",
    "#d70087", "#d700af", "#d700d7", "#d700ff", "#d75f00", "#d75f5f",
    "#d75f87", "#d75faf", "#d75fd7", "#d75fff", "#d78700", "#d7875f",
    "#d78787", "#d787af", "#d787d7", "#d787ff", "#d7af00", "#d7af5f",
    "#d7af87", "#d7afaf", "#d7afd7", "#d7afff", "#d7d700", "#d7d75f",
    "#d7d787", "#d7d7af", "#d7d7d7", "#d7d7ff", "#d7ff00", "#d7ff5f",
    "#d7ff87", "#d7ffaf", "#d7ffd7", "#d7ffff", "#ff0000", "#ff005f",
    "#ff0087", "#ff00af", "#ff00d7", "#ff00ff", "#ff5f00", "#ff5f5f",
    "#ff5f87", "#ff5faf", "#ff5fd7", "#ff5fff", "#ff8700", "#ff875f",
    "#ff8787", "#ff87af", "#ff87d7", "#ff87ff", "#ffaf00", "#ffaf5f",
    "#ffaf87", "#ffafaf", "#ffafd7", "#ffafff", "#ffd700", "#ffd75f",
    "#ffd787", "#ffd7af", "#ffd7d7", "#ffd7ff", "#ffff00", "#ffff5f",
    "#ffff87", "#ffffaf", "#ffffd7", "#ffffff", "#080808", "#121212",
    "#1c1c1c", "#262626", "#303030", "#3a3a3a", "#444444", "#4e4e4e",
    "#585858", "#626262", "#6c6c6c", "#767676", "#808080", "#8a8a8a",
    "#949494", "#9e9e9e", "#a8a8a8", "#b2b2b2", "#bcbcbc", "#c6c6c6",
    "#d0d0d0", "#dadada", "#e4e4e4", "#eeeeee")[[x]]
}

color_8_to_html <- function(tag) {
  sprintf('<span style="color:%s">', html_color_8(tag))
}

bgcolor_8_to_html <- function(tag) {
  sprintf('<span style="background-color:%s">', html_color_8(tag))
}

color_256_to_html <- function(tag) {
  num <- as.numeric(gsub("38;5;", "", tag))
  sprintf('<span style="color:%s">', html_color_256(num))
}

bgcolor_256_to_html <- function(tag) {
  num <- as.numeric(gsub("48;5;", "", tag))
  sprintf('<span style="background-color:%s">', html_color_256(num))
}

color_rgb_to_html <- function(tag) {
  colstr <- gsub("38;2;", "", tag)
  sprintf('<span style="color:%s">', html_color_rgb(colstr))
}

bgcolor_rgb_to_html <- function(tag) {
  colstr <- gsub("48;2;", "", tag)
  sprintf('<span style="background-color:%s">', html_color_rgb(colstr))
}

bold_to_html <- function(tag) {
  '<span style="font-weight:bold">'
}

## This is not supported. I am not sure how we could easily support it
blurred_to_html <- function(tag) {
  '<span>'
}

italic_to_html <- function(tag) {
  '<span style="font-style:italic">'
}

underline_to_html <- function(tag) {
  '<span style="text-decoration:underline">'
}

## This is difficult to do, so we do not do it
inverse_to_html <- function(tag) {
  '<span>'
}

hidden_to_html <- function(tag) {
  '<span style="visibility:hidden">'
}

strikethrough_to_html <- function(tag) {
  '<span style="text-decoration:line-through">'
}
