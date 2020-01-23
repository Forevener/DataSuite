# TODO: Confirmatory factor analysis, additional options
ds.screeplot <- function() {
  # Prepare UI
  removeUI(
    selector = "div[id^='fa_results']",
    multiple = TRUE
  )
  insertUI(
    selector = "#key_div_fa_table",
    ui = tags$div(
      id = "fa_results_a",
      tags$p(i18n$t("Метод каменистой осыпи Кеттела и параллельный анализ")),
      plotOutput("fa_plot"),
      tags$p(i18n$t("Вывод:")),
      textOutput("fa_text_1"),
      textOutput("fa_text_2"),
      tags$p(i18n$t("Выводы psycho::n_factors:")),
      textOutput("fa_text_3")
    )
  )

  # Retrieve data
  in_data <- check_data(zeroVar = TRUE)$data

  # Perform parallel analysis, consider method, extract output
  pa_recommendation <- gsub("[^0-9]", "", capture.output({
    if (input$si_factoring_method == "pc") {
      model <- fa.parallel(in_data, plot = FALSE, fa = "pc")
      plot_data <- compose_fa_plot_data(model$pc.values, model$pc.sim, model$pc.simr)
      axis_label <- i18n$t("Компоненты")
      type <- i18n$t("компонентов")
      kt_recommendation <- length(Filter(function(x) {
        x > 1.0
      }, model$pc.values))
    } else {
      model <- fa.parallel(in_data, plot = FALSE, fm = input$si_factoring_method, fa = "fa")
      plot_data <- compose_fa_plot_data(model$fa.values, model$fa.sim, model$fa.simr)
      axis_label <- i18n$t("Факторы")
      type <- i18n$t("факторов")
      kt_recommendation <- length(Filter(function(x) {
        x > 1.0
      }, model$fa.values))
    }
  }))

  # Render UI
  output[["fa_plot"]] <- renderCachedPlot(
    {
      build_scree_ggplot(plot_data, axis_label)
    },
    cacheKeyExpr = plot_data
  )
  output[["fa_text_1"]] <- renderText(glue(i18n$t("По критерию параллельного анализа рекомендуется выбрать {type}: {pa_recommendation}")))
  output[["fa_text_2"]] <- renderText(glue(i18n$t("По критерию Кайзера рекомендуется выбрать {type}: {kt_recommendation}")))
  # No reactivity for the following output
  rot <- input$si_factor_rotation
  met <- input$si_factoring_method
  output[["fa_text_3"]] <- renderPrint(psycho::n_factors(in_data, rotate = rot, fm = met))
}

ds.factoranalysis <- function() {
  # Prepare UI
  removeUI(
    selector = "div[id^='fa_results']",
    multiple = TRUE
  )
  insertUI(
    selector = "#key_div_fa_table",
    ui = tags$div(
      id = "fa_results_a",
      tableOutput("fa_table_main")
    )
  )
  insertUI(
    selector = "#key_div_fa_details",
    ui = tags$div(
      id = "fa_results_b",
      tags$p(i18n$t("Полная таблица нагрузок")),
      tableOutput("fa_table_1"),
      tags$p(i18n$t("Сведения о факторах")),
      tableOutput("fa_table_2"),
      tags$p(i18n$t("Показатели качества модели")),
      tableOutput("fa_table_3")
    )
  )
  insertUI(
    selector = "#key_div_fa_plots",
    ui = tags$div(
      id = "fa_results_c",
      tags$p(i18n$t("График факторного анализа")),
      plotOutput("fa_plot_1"),
      tags$p(i18n$t("График нагрузок")),
      plotOutput("fa_plot_2")
    )
  )

  # Retrieve data and set original names
  valid_data <- check_data(zeroVar = TRUE)
  in_data <- valid_data$data
  colnames(in_data) <- valid_data$names

  # Build model, considering method
  if (input$si_factor_rotation == "equamax") {
    if (input$si_factoring_method == "pc") {
      model <- principal(in_data, nfactors = input$factors_number, rotate = input$si_factor_rotation)
    } else {
      model <- fa(in_data, nfactors = input$factors_number, rotate = input$si_factor_rotation, SMC = FALSE, fm = input$si_factoring_method)
    }
  } else {
    if (input$si_factoring_method == "pc") {
      model <- principal(in_data, nfactors = input$factors_number, rotate = input$si_factor_rotation, normalize = input$cb_normalize)
    } else {
      model <- fa(in_data, nfactors = input$factors_number, rotate = input$si_factor_rotation, SMC = FALSE, fm = input$si_factoring_method, normalize = input$cb_normalize)
    }
  }

  # Pretty names for factors
  factor_names <- lapply(1:length(model$R2), function(n) {
    if (input$si_factoring_method == "pc") {
      glue(i18n$t("Компонент {n}"))
    } else {
      glue(i18n$t("Фактор {n}"))
    }
  })

  # Prepare main loadings table
  result <- data.frame(unclass(fa.sort(model$loadings)))
  colnames(result) <- factor_names
  if (settings()$fa_cut > 0) {
    result[abs(result) < settings()$fa_cut] <- NaN
  }
  table1 <- result
  if (settings()$fa_load > 0) {
    table1[] <- format_if(as.matrix(result), condition = paste0("{abs(x)}>=", settings()$fa_load))
  }

  # Prepare extended loadings table
  tableA <- data.frame(unclass(model$loadings))
  colnames(tableA) <- factor_names
  if (settings()$fa_load > 0) {
    tableA[] <- format_if(as.matrix(tableA), condition = paste0("{abs(x)}>=", settings()$fa_load))
  }
  tableA[[i18n$t("Общность")]] <- model$communalities
  tableA[[i18n$t("Уникальность")]] <- model$uniquenesses
  tableA[[i18n$t("Сложность")]] <- model$complexity

  # Prepare eigenvalues and factors quality table
  tableB <- data.frame(rbind(model$values[1:length(model$R2)], model$Vaccounted))
  rownames(tableB)[1] <- i18n$t("Собственное значение")
  colnames(tableB) <- factor_names
  if (length(model$R2) > 1) {
    tableB <- tableB[!(row.names(tableB) %in% c("Cumulative Var", "Cumulative Proportion")), ]
    x <- rownames(tableB)
    x <- replace(x, x == "SS loadings", i18n$t("Объясняемая дисперсия"))
    x <- replace(x, x == "Proportion Var", i18n$t("Доля общей дисперсии"))
    x <- replace(x, x == "Proportion Explained", i18n$t("Доля объясняемой дисперсии"))
    rownames(tableB) <- x
  }
  else {
    rownames(tableB) <- c(i18n$t("Собственное значение"), i18n$t("Объясняемая дисперсия"), i18n$t("Доля общей дисперсии"))
  }

  # Calculate model quality
  s <- fa.stats(in_data, model)
  tableC <- i18n$t("Показатель") %isnameof% data.frame(c(
    i18n$t("Корень квадратов остатков") %isnameof% ifelse(is.null(s$rms), 0, round(s$rms, 4)),
    i18n$t("Корень среднего квадрата ошибки аппроксимации") %isnameof% ifelse(is.null(s$RMSEA[[1]]), 0, round(s$RMSEA[[1]], 4)),
    i18n$t("Индекс Такера-Льюиса") %isnameof% ifelse(is.null(s$TLI), 0, round(s$TLI, 4))
  ))

  # Prepare data for plots
  plot_data <- custom_melt(result, length(model$R2))
  colnames(plot_data) <- c(i18n$t("Фактор"), i18n$t("Нагрузка"))
  plot_data[is.na(plot_data)] <- 0
  plot_data[[i18n$t("Переменная")]] <- factor(sapply(rownames(result), function(x) {
    rep(x, length(model$R2))
  }), levels = rownames(result), ordered = TRUE)

  # Render UI
  output[["fa_table_main"]] <- renderTable(table1, rownames = TRUE, digits = 3, na = "", sanitize.text.function = identity)
  output[["fa_table_1"]] <- renderTable(tableA, rownames = TRUE, digits = 3, sanitize.text.function = identity)
  output[["fa_table_2"]] <- renderTable(tableB, rownames = TRUE, digits = 4)
  output[["fa_table_3"]] <- renderTable(tableC, rownames = TRUE, digits = 4)
  output[["fa_plot_1"]] <- renderCachedPlot(
    {
      fa.diagram(model, main = NULL)
    },
    cacheKeyExpr = model
  )
  output[["fa_plot_2"]] <- renderCachedPlot(
    {
      ggplot(data = plot_data, aes(!!sym(i18n$t("Переменная")), !!sym(i18n$t("Нагрузка")), color = !!sym(i18n$t("Фактор")), group = !!sym(i18n$t("Фактор")))) +
        geom_line() +
        ylim(-1, 1) +
        geom_hline(yintercept = 0) +
        coord_polar()
    },
    cacheKeyExpr = plot_data
  )
}
