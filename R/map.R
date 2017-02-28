
## To calculate coordinates, raw -> ansi or back
## First columns is raw coordinates, second column is
## ansi coordinates, third column is the offset between them

make_shifts1 <- function(re) {
  shifts <- cbind(re$.match$start[[1]],
                  re$.match$end[[1]] - re$.match$start[[1]] + 1)
  shifts[, 2] <- cumsum(shifts[, 2])
  shifts <- cbind(
    c(shifts[1,1], shifts[-1, 1] - shifts[-nrow(shifts), 2]),
    shifts
  )
  shifts
}

#' @importFrom utils tail

map_raw_to_ansi1 <- function(map, raw) {
  sh <- map$shifts
  wh <- tail(which(raw >= sh[, 1]), 1)
  if (!length(wh)) {
    raw
  } else {
    raw + sh[wh, 3]
  }
}

#' @importFrom utils tail

map_ansi_to_raw1 <- function(map, ansi) {
  sh <- map$shifts
  wh <- tail(which(ansi >= sh[, 2])[1], 1)
  if (!length(wh)) {
    ansi
  } else {
    ansi - sh[wh, 3]
  }
}

#' @importFrom rematch2 re_exec_all
#' @importFrom utils tail

make_ansi_map1 <- function(str) {
  re <- re_exec_all(str, re_ansi())

  ## Number of ANSI tags
  num_tags <- length(re$.match$start[[1]])
  if (num_tags == 0) {
    return(data.frame(
      stringsAsFactors = FALSE,
      start = numeric(),
      end = numeric(),
      open = character(),
      close = character()
    ))
  }

  ## Which tags are start tags / end tags
  start_tags <- re$start$start[[1]] > 0

  shifts <- make_shifts1(re)

  res <- data.frame(
    stringsAsFactors = FALSE,
    start = shifts[, 1],
    end = rep(NA, num_tags),
    open = re$.match$match[[1]],
    close = rep(NA_character_, num_tags)
  )

  ## To avoid multiple calls to this
  re_closes <- re_closes()

  ## Currently active start tags
  active <- numeric()

  for (i in seq_len(num_tags)) {
    ## If it is a start tag, then we mark it as active
    if (start_tags[i]) {
      active <- c(active, i)

    ## End tag, check which active start tags it closes
    } else {
      endcode <- re$end$match[[1]][i]
      endtag <- re$.match$match[[1]][i]
      active_start_codes <- re$start$match[[1]][active]
      closes <- grepl(re_closes[[endcode]], active_start_codes)
      res$end[active[closes]] <- shifts[i, 1] - 1
      res$close[active[closes]] <- endtag
      active <- setdiff(active, active[closes])
    }
  }

  ## We only really need one row per start tag
  res <- res[start_tags, ]

  ## Close at the end of the string, if the ANSI close tag is missing
  res$end[is.na(res$end)] <- nchar(str) - tail(shifts[,1], 1)
  res$close[is.na(res$close)] <- ""

  list(map = res, shifts = shifts)
}

make_ansi_map <- function(str) {
  lapply(str, make_ansi_map)
}
