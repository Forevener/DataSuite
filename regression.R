# TODO: Rework, separate optimal model browser, additional modelling options
ds.glm <- function() {
  # Prepare UI
  removeUI(
    selector = "div[id^=regression_]",
    multiple = TRUE
  )
  insertUI(
    selector = "#key_div_regression_tables",
    ui = tags$div(id = "regression_tables")
  )
  insertUI(
    selector = "#key_div_regression_plots",
    ui = tags$div(id = "regression_plots")
  )

  # Retrieve the data
  in_data <- get_data()
  data_names <- get_names()

  # Prepare variables, factorize independent ones
  num_vars <- ncol(in_data)
  ind_vars_i <- strtoi(input$si_ind_vars_regression)
  # Could be useful for special categorical variables selector later
  # for (i in 1:length(ind_vars_i))
  # {
  # 	x = ind_vars_i[i]
  # 	if (is.numeric(in_data[[x]]))
  # 		in_data[[x]] = as.factor(in_data[[x]])
  # }
  ind_vars_n <- colnames(in_data)[ind_vars_i]

  # Prepare the formula
  form_right <- paste0(ind_vars_n, collapse = " + ")

  # Perform GLM
  series <- strtoi(input$si_dep_vars_regression)
  lapply(1:length(series), function(i) {
    index <- series[i]

    # Building the model
    form <- as.formula(paste0(colnames(in_data)[index], " ~ ", form_right))
    model <- glm(form, data = in_data)

    # Coefficients table
    tableA <- data.frame(summary(model)$coefficients)
    tableA[[4]] <- format_if(tableA[[4]], condition = paste0("{x}<=", settings()$p))
    colnames(tableA) <- c(i18n$t("Коэффициент"), i18n$t("Стандартная ошибка"), "t", "p")
    rownames(tableA)[-1] <- data_names[ind_vars_i]

    # Quality indicators
    quality <- paste0(
      i18n$t("Информационный критерий Акаике:"), " ", round(AIC(model), 2), "\r\n",
      i18n$t("Информационный критерий Байеса:"), " ", round(BIC(model), 2), "\r\n",
      i18n$t("Логарифмическая функция правдоподобия:"), " ", round(logLik(model)[1], 2)
    )

    # Adding the UI elements
    n <- paste0("regression_table_", index)
    nt <- paste0("regression_text_", index)
    np <- paste0("regression_plot_", index)
    insertUI(
      selector = "#regression_tables",
      ui = tagList(
        tags$p(data_names[index]),
        tags$p(i18n$t("Общая таблица")),
        tableOutput(n),
        tags$p(i18n$t("Показатели качества модели")),
        verbatimTextOutput(nt),
        tags$br()
      )
    )
    insertUI(
      selector = "#regression_plots",
      ui = tagList(
        tags$p(data_names[index]),
        plotOutput(np),
        tags$br()
      )
    )

    # Rendering UI
    output[[n]] <- renderTable(tableA, rownames = TRUE, sanitize.text.function = identity)
    output[[nt]] <- renderText(quality)
    output[[np]] <- renderCachedPlot(
      {
        sjPlot::plot_model(model)
      },
      cacheKeyExpr = model
    )
  })
}

ds.optimalglms <- function() {
  # Prepare UI
  removeUI(
    selector = "div[id^=regression_]",
    multiple = TRUE
  )
  insertUI(
    selector = "#key_div_regression_tables",
    ui = tags$div(id = "regression_plots")
  )

  # Retrieve the data
  in_data <- get_data()
  data_names <- get_names()

  # Prepare variables, factorize independent ones
  num_vars <- ncol(in_data)
  ind_vars_i <- strtoi(input$si_ind_vars_regression)
  ind_vars_n <- colnames(in_data)[ind_vars_i]

  # Prepare the formula
  form_right <- paste0(ind_vars_n, collapse = " + ")

  # Perform optimal LM selection
  series <- strtoi(input$si_dep_vars_regression)
  lapply(1:length(series), function(i) {
    index <- series[i]

    # Building the models
    # new_data <- data.frame(in_data[ind_vars_i], "y" = in_data[[index]])
    # best_models <- bestglm::bestglm(new_data)
    form <- as.formula(paste0(colnames(in_data)[index], " ~ ", form_right))
    models <- leaps::regsubsets(form, in_data, nvmax = NULL)

    # Adding the UI elements
    n <- paste0("regression_plot_", index)
    insertUI(
      selector = "#regression_plots",
      ui = tagList(
        tags$p(data_names[index]),
        plotOutput(n),
        tags$br()
      )
    )

    # Rendering UI
    output[[n]] <- renderCachedPlot(
      {
        plot(models, scale = "adjr2")
      },
      cacheKeyExpr = models
    )
  })
}
