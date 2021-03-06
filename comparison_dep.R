# TODO: More flexibility in variable selection and naming
ds.cds <- function(method = "t") {
  # Strings for output and local variables determining analysis type
  method_title <- switch(method,
    "t" = i18n$t("t-критерия Стьюдента"),
    "Z" = i18n$t("критерия знаков"),
    "W" = i18n$t("W-критерия Уилкоксона"),
    "Q" = i18n$t("Q-критерия Фридмана"),
    "F" = i18n$t("анализа повторяющихся наблюдений")
  )
  parametric <- method == "t" || method == "F"
  multiple <- method == "Q" || method == "F"
  method_agg <- ifelse(parametric, i18n$t("Среднее"), i18n$t("Медиана"))

  # Prepare UI
  removeUI(
    selector = "div[id^=cds_]",
    multiple = TRUE
  )
  insertUI(
    selector = "#key_div_cds_table",
    ui = tags$div(
      id = "cds_table",
      tags$p(glue(i18n$t("Сравнение зависимых выборок с помощью {method_title}"))),
      tableOutput("cds_result_table")
    )
  )
  insertUI(
    selector = "#key_div_cds_plots",
    ui = tags$div(id = "cds_plots")
  )
  if (multiple) {
    pairwise_method_title = ifelse(parametric, i18n$t("t-критерия Стьюдента"), i18n$t("W-критерия Уилкоксона"))
    insertUI(
      selector = "#key_div_cds_details",
      ui = tags$div(
        id = "cds_details",
        tags$p(glue(i18n$t("Попарные сравнения зависимых выборок с помощью {pairwise_method_title}")))
      )
    )
  }

  # Excluding non-numeric variables could lead to unpredictable results - so it's better to just throw an error and let user check the data manually
  if (length(check_data()$cols) < ncol(get_data())) {
    stop(i18n$t("Были обнаружены нечисловые переменные. Сравнение выборок отменено, рекомендуется внимательно проверить и убрать лишние столбцы из анализа."))
  }

  # Retrieve data, set variables
  n_measures <- ifelse(multiple, strtoi(input$measures_number), 2L)
  in_data <- get_data()
  num_vars <- ncol(in_data) / n_measures
  data_names <- get_names()[1:num_vars]

  new_data <- custom_melt(in_data, n_measures)
  measure <- new_data[[1]]
  new_data <- new_data[-1]
  colname <- lapply(1:n_measures, function(n) glue(i18n$t("{method_agg} по замеру #{n}")))
  table_names <- list(data_names, c(colname, c(method, "p", i18n$t("Различия"))))
  out_data <- matrix(nrow = num_vars, ncol = length(table_names[[2]]), dimnames = table_names)

  lapply(1:num_vars, function(index) {
    # Calculate means/medians and analysis results
    func <- ifelse(parametric, "mean", "median")
    aggs <- aggregate(new_data[[index]], by = list(measure), FUN = func, na.rm = TRUE)
    result <- switch(method,
      "t" = t.test(in_data[[index]], in_data[[index + num_vars]], paired = TRUE),
      "Z" = DescTools::SignTest(in_data[[index]], in_data[[index + num_vars]], na.rm = F),
      "W" = wilcox.test(in_data[[index]], in_data[[index + num_vars]], paired = TRUE, na.rm = F),
      "F" = oneway.test(new_data[[index]] ~ measure),
      "Q" = friedman.test(extract(in_data, index, n_measures))
    )

    # Dirtiest hack for proper W value (is it really proper?)
    if (method == "W") {
      result2 = wilcox.test(in_data[[index + num_vars]], in_data[[index]], paired = TRUE, na.rm = F)
      if (result$statistic > result2$statistic) {
        result = result2
      }
    }

    # Fill the resulting table
    for (y in 1:n_measures) {
      out_data[index, y] <<- sprintf(round(aggs[y, 2], 2), fmt = "%#.2f")
    }
    out_data[index, y + 1] <<- sprintf(round(result$statistic[[1]], 3), fmt = "%#.3f")
    out_data[index, y + 2] <<- format_if(result$p.value, condition = paste0("{x}<=", settings()$p))
    out_data[index, y + 3] <<- ifelse(result$p.value > 0.05, i18n$t("Отсутствуют"), i18n$t("Присутствуют"))

    if (multiple) {
      # Calculate pairwise comparisons
      pwc <- switch(method,
        "F" = pairwise.t.test(new_data[[index]], measure, p.adjust.method = "BH", paired = TRUE, na.rm = TRUE)$p.value,
        "Q" = pairwise.wilcox.test(new_data[[index]], measure, p.adjust.method = "BH", paired = TRUE, na.rm = TRUE)$p.value
      )
      pwc[] <- format_if(pwc, condition = paste0("{x}<=", settings()$p))

      # Prepare and render detailed tables
      nt <- paste0("cds_table_", index)
      insertUI(
        selector = "#cds_details",
        ui = tagList(
          tags$p(data_names[index]),
          tableOutput(nt),
          tags$br()
        )
      )
      output[[nt]] <- renderTable(pwc, rownames = TRUE, sanitize.text.function = identity)
    }

    # Generate the name for the plot and insert its UI
    n <- paste0("cds_plot_", index)
    insertUI(
      selector = "#cds_plots",
      ui = tagList(
        tags$p(data_names[index]),
        plotOutput(n),
        tags$br()
      )
    )

    if (parametric) {
      # Violin plots + mean points
      g <- ggplot(new_data, aes(measure, new_data[[index]])) +
        geom_violin() +
        stat_summary(fun.y = mean, geom = "point", size = 2)
    }
    else {
      # Violin plots + quantiles
      g <- ggplot(new_data, aes(measure, new_data[[index]])) +
        geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))
    }

    output[[n]] <- renderCachedPlot(
      {
        g + labs(x = i18n$t("Замер"), y = i18n$t("Значение"))
      },
      cacheKeyExpr = list(measure, new_data[[index]], parametric)
    )
  })

  output$cds_result_table <- renderTable(out_data, rownames = TRUE, sanitize.text.function = identity, na = "")
}
