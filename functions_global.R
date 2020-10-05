custom_melt_slow <- function(dataset, times) {
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
  } else {
    stop("Argument 'times' is not integer")
  }
}

custom_melt <- function(dataset, times) {
  if (is.integer(times)) {
    rows <- nrow(dataset)
    cols <- ncol(dataset)
    new_cols <- cols / times

    result <- dataset[1:new_cols]
    for (x in 1:(times - 1))
    {
      start <- 1 + new_cols * x
      end <- new_cols * (x + 1)
      result <- rbind(result, setNames(dataset[start:end], colnames(result)))
    }

    result <- cbind(
      "Measure" = factor(rep(1:times, each = rows)),
      result
    )

    return(result)
  } else {
    stop("Argument 'times' is not integer")
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

last <- function(object) {
  tail(object, 1)
}

format_if <- function(in_data, tags_format = "strong", condition = "{x}<=0.05", f_digits = 5) {
  sapply(in_data, function(x) {
    if (!is.na(x)) {
      if (is.numeric(x)) {
        out <- ifelse(is.integer(x),
          x,
          formatC(x, digits = f_digits, format = "f")
        )

        x <- ifelse(evaluate(glue(condition)),
          as.character(tags[[tags_format]](out)),
          out
        )
      }
    } else {
      x <- NA
    }
    return(x)
  })
}

`%isnameof%` <- function(name, object) {
  setNames(object, name)
}

list_to_query <- function(in_list) {
  if (length(in_list) < 1) {
    stop("list was empty")
  }
  paste0(
    "?",
    paste0(
      lapply(1:length(in_list), function(i) {
        paste0(names(in_list[i]), "=", in_list[i])
      }),
      collapse = "&"
    )
  )
}

update_translation <- function() {
  base <- xlsx::read.xlsx("./translations/source.xlsx", sheetIndex = 1, encoding = "UTF-8")
  write.csv(base, "./translations/base.csv", row.names = FALSE, fileEncoding = "UTF-8")
}

update_list <- function(in_list, named_values) {
  for (i in 1:length(named_values)) {
    name <- names(named_values)[i]
    if (!is.null(in_list[[name]])) {
      in_list[[name]] <- named_values[[i]]
    }
  }
  return(in_list)
}

evaluate <- function(string) {
  eval(parse(text = string))
}

named_diff <- function(list_base, list_compare) {
  num <- length(list_compare)
  diff <- list()
  for (index in 1:num) {
    if (list_base[[names(list_compare[index])]] != list_compare[[index]]) {
      diff[names(list_compare[index])] <- list_compare[index]
    }
  }
  return(diff)
}

is_constant <- function(x) {
  if (is.factor(x)) {
    length(levels(x)) == 1
  } else {
    var(x, na.rm = TRUE) == 0
  }
}


mode <- function(x, na.rm = FALSE) {
  # https://stackoverflow.com/a/45216553/12449965
  if (na.rm) {
    x <- na.omit(x)
  }
  if (is.factor(x)) {
    x <- as.character(x)
  }
  ux <- unique(x)
  freq <- tabulate(match(x, ux))
  mode_loc <- which(freq == max(freq))

  return(list("mode" = ifelse(length(mode_loc) > 1, "multiple", ux[mode_loc]), "frequency" = which.max(freq)))
}

se <- function(x, na.rm = FALSE) {
  # https://stackoverflow.com/a/7220087/12449965
  if (na.rm) {
    x <- na.omit(x)
  }

  sqrt(var(x) / length(x))
}

grouped_freq <- function(variable, group_var) {
  group_var <- as.factor(group_var)

  result <- purrr::reduce(
    lapply(levels(group_var), function(x) {
      setNames(
        data.frame(
          table(variable[group_var == x])
        ),
        c("value", x)
      )
    }),
    dplyr::full_join,
    by = "value"
  )
  rownames(result) <- result$value
  result$value <- NULL
  result[is.na(result)] <- 0L

  return(result)
}
