custom_melt <- function(dataset, times) {
  repeats <- as.integer(times)
  if (is.integer(repeats)) {
    rows <- nrow(dataset)
    cols <- ncol(dataset)
    new_cols <- cols / repeats
    result <- data.frame(matrix(nrow = rows * repeats, ncol = new_cols + 1, dimnames = list(NULL, c("Measure", colnames(dataset[1:new_cols])))))
    n <- 1
    for (x in 1:rows)
    {
      for (y in 0:(repeats - 1))
      {
        starting <- 1 + new_cols * y
        result[n, 1] <- y + 1
        result[n, 2:(new_cols + 1)] <- dataset[x, starting:(starting + new_cols - 1)]
        n <- n + 1
      }
    }
    result[[1]] <- factor(result[[1]])
    return(result)
  }
  else {
    warning("Argument 'times' is not integer")
  }
}

extract <- function(dataset, column_n, measures) {
  result <- matrix(nrow = nrow(dataset), ncol = measures)
  columns <- ncol(dataset) / measures

  for (index in 1:measures)
  {
    result[, index] <- dataset[[column_n + (index - 1) * columns]]
  }

  return(result)
}

last <- function(x) {
  return(x[length(x)])
}

strong_p <- function(data, level) {
  sapply(data, function(x) {
    if (!is.na(x) && is.numeric(x)) {
      if (x <= level) {
        return(paste0("<strong>", sprintf(round(x, 5), fmt = "%#.5f"), "</strong>"))
      } else {
        return(sprintf(round(x, 5), fmt = "%#.5f"))
      }
    }
    else {
      return(x)
    }
  })
}

`%isnameof%` <- function(name, object) {
  setNames(object, name)
}

update_translation <- function() {
  base = xlsx::read.xlsx("./translations/source.xlsx", sheetIndex = 1, encoding = "UTF-8")
  write.csv(base, "./translations/base.csv", row.names = FALSE, fileEncoding = "UTF-8")
}
