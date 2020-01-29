# TODO: proper clearUI()
fill_inputs <- function() {
  in_data <- get_data()
  data_names <- get_names()
  num_vars <- ncol(in_data)

  selections_all <- data_names %isnameof% 1:num_vars
  selections_any <- selections_all[!sapply(in_data, is_constant)]
  selections_num <- selections_any[sapply(in_data[selections_any], is.numeric)]
  selections_two <- selections_any[sapply(in_data[selections_any], function(x) {
    length(levels(as.factor(x))) == 2
  })]

  updatePickerInput(session, "si_var_ctis", choices = selections_two)
  updatePickerInput(session, "si_var_cmis", choices = selections_any, selected = "")
  updatePickerInput(session, "si_vars_manova", choices = selections_any)
  updatePickerInput(session, "si_dep_vars_regression", choices = selections_num)
  updatePickerInput(session, "si_ind_vars_regression", choices = selections_any)
  updatePickerInput(session, "si_var1_corr", choices = selections_num)
  updatePickerInput(session, "si_var2_corr", choices = selections_num)
  updatePickerInput(session, "si_reli_vars", choices = selections_num)
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
  else {
    func
  }

  enable_all(inputs)
}

check_data <- function(columns = NULL, num = TRUE, nas = FALSE, zeroVar = FALSE, ignoreCols = NULL) {
  if (is.null(columns)) {
    in_data <- get_data()
    data_names <- get_names()
  } else {
    in_data <- get_data()[columns]
    data_names <- get_names()[columns]
  }
  result <- ""
  valid_cols <- c(1:ncol(in_data))
  valid_rows <- c(1:nrow(in_data))

  if (num) {
    logi_num <- sapply(valid_cols, function(v) {
      is.numeric(in_data[[v]])
    })
    logi_num[ignoreCols] <- TRUE
    non_valid_cols <- valid_cols[!logi_num]
    valid_cols <- valid_cols[logi_num]

    if (length(valid_cols) == 0) {
      stop(i18n$t("В таблице данных нет числовых столбцов"))
    }

    if (length(non_valid_cols) > 0) {
      result <- paste0(
        tags$strong(i18n$t("Следующие столбцы не являются числовыми и были устранены из анализа:")), "<br/>",
        paste0(data_names[non_valid_cols], collapse = "<br/>")
      )
      if (nas || zeroVar) {
        result <- paste0(result, "<br/><br/>")
      }
    }
  }

  if (zeroVar) {
    logi_var <- sapply(valid_cols, function(v) {
      if (is.factor(in_data[[v]])) {
        length(levels(in_data[[v]])) > 1
      } else {
        var(in_data[[v]], na.rm = TRUE) > 0
      }
    })
    zero_var_cols <- valid_cols[!logi_var]
    valid_cols <- valid_cols[logi_var]

    if (length(valid_cols) == 0) {
      stop(i18n$t("Все столбцы таблицы данных имеют только по одному значению"))
    }

    if (length(zero_var_cols) > 0) {
      result <- paste0(
        result,
        tags$strong(i18n$t("Следующие столбцы имели единственное значение и были устранены из анализа:")), "<br/>",
        paste0(data_names[zero_var_cols], collapse = "<br/>")
      )
      if (nas) {
        result <- paste0(result, "<br/><br/>")
      }
    }
  }

  if (nas) {
    logi_rows <- sapply(valid_rows, function(r) {
      !anyNA(in_data[r, ])
    })
    non_valid_rows <- valid_rows[!logi_rows]
    valid_rows <- valid_rows[logi_rows]

    if (length(valid_rows) == 0) {
      stop(i18n$t("В таблице данных нет строк без пропусков"))
    }

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

upload_file <- function(in_file) {
  if (is.null(in_file)) {
    stop(i18n$t("Файл отсутствует"))
  }

  cat(file = stderr(), paste0("File uploaded: ", in_file$name, "\r\n")) # USER TESTING TRACING

  file_ext <- tail(strsplit(in_file$name, split = "\\.")[[1]], 1)
  if (input$file_encoding == "NULL") {
    enc <- NULL
    loc <- readr::locale()
  } else {
    enc <- input$file_encoding
    loc <- readr::locale(encoding = enc)
  }

  temp_data <- switch(file_ext,
    "xlsx" = readxl::read_xlsx(in_file$datapath),
    "xls" = readxl::read_xls(in_file$datapath),
    "csv" = readr::read_csv(in_file$datapath, locale = loc),
    "tsv" = readr::read_tsv(in_file$datapath, locale = loc),
    "ods" = readODS::read_ods(in_file$datapath),
    "dta" = haven::read_dta(in_file$datapath, encoding = enc),
    "por" = haven::read_por(in_file$datapath),
    "sas" = haven::read_sas(in_file$datapath, encoding = enc),
    "sav" = haven::read_sav(in_file$datapath, encoding = enc),
    "zsav" = haven::read_sav(in_file$datapath, encoding = enc),
    "xpt" = haven::read_xpt(in_file$datapath)
  )
  temp_data <- dplyr::mutate_if(temp_data, is.character, as.factor) # We need factors for DT filters to work properly
  temp_data <- janitor::remove_empty(temp_data, c("rows", "cols"))
  temp_names <- colnames(temp_data)
  colnames(temp_data) <- janitor::make_clean_names(temp_names, case = "none")
  cat(file = stderr(), paste0("File structure: ", capture.output(str(temp_data)), "\r\n")) # USER TESTING TRACING

  selections <- temp_names %isnameof% 1:length(temp_names)
  updatePickerInput(session, "si_include_vars", choices = selections, selected = selections)
  updateCheckboxGroupInput(session, "cbg_by_group", choices = selections, selected = NULL)

  base_data(temp_data)
  base_names(temp_names)

  clear_ui()

  output$in_table <- renderDT(base_data(), filter = list(position = "top"), options = list(language = list(url = glue("//cdn.datatables.net/plug-ins/1.10.11/i18n/{i18n$translation_language}.json"))))
  show("databox")
  showNotification(i18n$t("Файл успешно загружен!"), type = "message")
}

set_language <- function(lang) {
  if (!any(lang == i18n$languages)) {
    lang <- "English"
  }

  i18n$set_translation_language(lang)
  source("ui_dynamic.R", encoding = "utf-8", local = TRUE)
  updateTabItems(session, "sidebar_tabs", "data_upload")
}

remove_selected <- function(input_1, input_2) {
  conflict <- intersect(input[[input_1]], input[[input_2]])
  if (length(conflict) > 0) {
    updatePickerInput(session, input_1, selected = setdiff(input[[input_1]], conflict))
  }
}
