ds.manova <- function() {
  # Prepare UI
  removeUI(
    selector = "div[id^=manova_]",
    multiple = TRUE
  )
  insertUI(
    selector = "#key_div_manova_tables",
    ui = tags$div(id = "manova_tables")
  )
  insertUI(
    selector = "#key_div_manova_plots",
    ui = tags$div(id = "manova_plots")
  )

  # Retrieve the data
  in_data <- get_data()
  data_names <- get_names()

  # Prepare variables, factorize independent ones
  num_vars <- ncol(in_data)
  ind_vars_i <- strtoi(input$si_vars_manova)
  for (i in 1:length(ind_vars_i))
  {
    x <- ind_vars_i[i]
    if (is.numeric(in_data[[x]])) {
      in_data[[x]] <- as.factor(in_data[[x]])
    }
  }
  ind_vars_n <- colnames(in_data)[ind_vars_i]
  check_data(-ind_vars_i)

  # Prepare the formula
  comm <- paste0(ind_vars_n, collapse = " * ")

  # Perform MANOVA
  series <- (1:num_vars)[-ind_vars_i]
  lapply(1:length(series), function(i) {
    index <- series[i]
    if (is.numeric(in_data[[index]])) {
      # Building the model
      form <- as.formula(paste0(colnames(in_data)[index], " ~ ", comm))
      model <- aov(form, data = in_data)

      # Effects table
      tableA <- summary(model)[[1]]
      tableA[[5]] <- format_if(tableA[[5]], condition = paste0("{x}<=", settings()$p))
      colnames(tableA) <- c(i18n$t("Степени свободы"), i18n$t("Сумма квадратов"), i18n$t("Среднее квадратов"), "F", "p")
      rownames(tableA) <- sub("Residuals", i18n$t("Остаток"), rownames(tableA), fixed = TRUE)

      # Pairwise comparisons table
      tableB <- as.data.frame(last(TukeyHSD(model))[[1]])
      tableB[[4]] <- format_if(tableB[[4]], condition = paste0("{x}<=", settings()$p))
      colnames(tableB) <- c(i18n$t("Разница средних"), i18n$t("Нижняя граница интервала"), i18n$t("Верхняя граница интервала"), i18n$t("Корректированный p"))

      # Adding the UI elements
      n1 <- paste0("manova_table_", index, "A")
      n2 <- paste0("manova_table_", index, "B")
      np <- paste0("manova_plot_", index)
      insertUI(
        selector = "#manova_tables",
        ui = tagList(
          tags$p(data_names[index]),
          tags$p(i18n$t("Общая таблица")),
          tableOutput(n1),
          tags$p(i18n$t("Попарные сравнения")),
          tableOutput(n2),
          tags$br()
        )
      )
      insertUI(
        selector = "#manova_plots",
        ui = tagList(
          tags$p(data_names[index]),
          plotOutput(np),
          tags$br()
        )
      )

      # Render UI
      output[[n1]] <- renderTable(tableA, rownames = TRUE, sanitize.text.function = identity)
      output[[n2]] <- renderTable(tableB, rownames = TRUE, sanitize.text.function = identity)
      output[[np]] <- renderCachedPlot(
        {
          plot.design(form, data = in_data, xlab = i18n$t("Факторы"), ylab = data_names[index])
        },
        cacheKeyExpr = list(in_data, ind_vars_i)
      )
    }
  })
}
