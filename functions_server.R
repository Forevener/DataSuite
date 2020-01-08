fill_inputs <- function() {
  in_data <- get_data()
  data_names <- get_names()
  num_vars <- ncol(in_data)

  selections_tis <- list()
  selections_mis <- list()
  selections_cor <- list()

  for (index in 1:num_vars)
  {
    names(index) <- data_names[index]
    if (length(levels(factor(in_data[[index]]))) == 2) {
      selections_tis <- append(selections_tis, index)
    }
    selections_mis <- append(selections_mis, index)
    if (is.numeric(in_data[[index]])) {
      selections_cor <- append(selections_cor, index)
    }
  }

  updatePickerInput(session, "si_var_ctis", choices = selections_tis, selected = selections_tis[1])
  updatePickerInput(session, "si_var_cmis", choices = selections_mis, selected = "")
  updatePickerInput(session, "si_vars_manova", choices = selections_mis)
  updatePickerInput(session, "si_vars_regression", choices = selections_mis)
  updatePickerInput(session, "si_var1_corr", choices = selections_cor)
  updatePickerInput(session, "si_var2_corr", choices = selections_cor)
  updateNumericInput(session, "factors_number", max = ncol(Filter(is.numeric, get_data())))
  updateNumericInput(session, "clusters_number", max = nrow(na.omit(get_data())))
}

clear_ui <- function() {
  hide(selector = "div[class*=_box")
}

disable_all <- function(elements) {
  lapply(elements, function(x) {
    disable(x)
  })
}

enable_all <- function(elements) {
  lapply(elements, function(x) {
    enable(x)
  })
}

ds_execute <- function(func, hideSelector = NULL, showSelector = NULL) {
  if (!is.null(hideSelector)) {
    hide(selector = hideSelector)
  }
  if (!is.null(showSelector)) {
    show(selector = showSelector)
  }
  inputs <- names(input)
  disable_all(inputs)
  if (!isLocal || input$ps_handle_errors) {
    result <- try(func)

    if (any(class(result) == "try-error")) {
      showModal(
        modalDialog(
          title = i18n$t("Ошибка"),
          footer = modalButton(i18n$t("ОК")),
          result
        )
      )
    }
  }
  else
    func

  enable_all(inputs)
}

check_data <- function(in_data, data_names = NULL, num = TRUE, nas = FALSE) {
  result <- ""
  valid_cols <- c(1:ncol(in_data))
  valid_rows <- c(1:nrow(in_data))
  if (is.null(data_names)) {
    data_names <- get_names()
  }

  if (num) {
    valid_cols <- sapply(valid_cols, function(v) {
      if (is.numeric(in_data[[v]])) {
        return(v)
      } else {
        return(NA)
      }
    })
    valid_cols <- valid_cols[!is.na(valid_cols)]

    if (length(valid_cols) == 0) {
      stop(i18n$t("В таблице данных нет числовых столбцов"))
    }

    non_valid_cols <- (1:ncol(in_data))[-valid_cols]

    if (length(non_valid_cols) > 0) {
      result <- paste0(
        tags$strong(i18n$t("Следующие столбцы не являются числовыми и были устранены из анализа:")), "<br/>",
        paste0(data_names[non_valid_cols], collapse = "<br/>")
      )
      if (nas) {
        result <- paste0(result, "<br/><br/>")
      }
    }
  }

  if (nas) {
    valid_rows <- sapply(valid_rows, function(r) {
      if (anyNA(in_data[r, ])) {
        return(NA)
      } else {
        return(r)
      }
    })
    valid_rows <- valid_rows[!is.na(valid_rows)]

    if (length(valid_rows) == 0) {
      stop(i18n$t("В таблице данных нет строк без пропусков"))
    }

    non_valid_rows <- (1:nrow(in_data))[-valid_rows]

    if (length(non_valid_rows) > 0) {
      result <- paste0(
        result,
        tags$strong(i18n$t("Следующие строки имели пропущенные значения и были устранены из анализа:")), "<br/>",
        paste0(rownames(in_data)[non_valid_rows], collapse = "<br/>")
      )
    }
  }

  if (nchar(result) > 1) {
    showModal(
      modalDialog(
        title = i18n$t("Предупреждение"),
        footer = modalButton(i18n$t("ОК")),
        HTML(result)
      )
    )
  }

  return(list("data" = in_data[valid_rows, valid_cols], "names" = data_names[valid_cols], "cols" = valid_cols, "rows" = valid_rows))
}

compose_fa_plot_data <- function(real, simulated, resampled) {
  factors <- length(real)
  series <- 1:factors

  if (length(resampled) > 0) {
    plot_data <- data.frame(
      rep(series, 3),
      c(real, simulated, resampled),
      factor(c(rep(i18n$t("Реальные данные"), factors), rep(i18n$t("Данные симуляции"), factors), rep(i18n$t("Данные ресэмплинга"), factors)), levels = c(i18n$t("Реальные данные"), i18n$t("Данные симуляции"), i18n$t("Данные ресэмплинга")), ordered = TRUE)
    )
  }
  else {
    plot_data <- data.frame(
      rep(series, 2),
      c(real, simulated),
      factor(c(rep(i18n$t("Реальные данные"), factors), rep(i18n$t("Данные симуляции"), factors)), levels = c(i18n$t("Реальные данные"), i18n$t("Данные симуляции")), ordered = TRUE)
    )
  }
  colnames(plot_data) <- c(i18n$t("Фактор"), i18n$t("СобственноеЗначение"), i18n$t("Категория"))

  return(data.frame(plot_data))
}

build_scree_ggplot <- function(plot_data, axis.title = i18n$t("Факторы")) {
  ggplot(data = plot_data, aes(!!sym(i18n$t("Фактор")), !!sym(i18n$t("СобственноеЗначение")), colour = !!sym(i18n$t("Категория")), linetype = !!sym(i18n$t("Категория")))) +
    geom_line() +
    geom_hline(yintercept = 1) +
    geom_point(aes(alpha = !!sym(i18n$t("Категория"))), shape = 0, size = 2) +
    scale_alpha_manual(values = c(1, 0, 0)) +
    scale_color_manual(values = c("blue", "red", "orange")) +
    labs(x = axis.title, y = i18n$t("Собственное значение")) +
    theme(
      legend.title = element_blank(),
      legend.justification = c(1, 1),
      legend.position = c(1, 1),
      legend.margin = margin(6, 6, 6, 6)
    )
}
