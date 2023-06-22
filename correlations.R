ds.correlations <- function(method) {
  # Prepare UI
  removeUI(
    selector = "div[id^=corr_]",
    multiple = TRUE
  )
  method_title <- switch(method,
    "spearman" = i18n$t("критерию Спирмена"),
    "kendall" = i18n$t("критерию Кендалла"),
    "pearson" = i18n$t("критерию Пирсона")
  )
  insertUI(
    selector = "#key_div_corr_tables",
    ui = tags$div(
      id = "corr_tables",
      tags$p(glue(i18n$t("Коэффициенты корреляции по {method_title}"))),
      tableOutput("corr_table_r"),
      tags$p(i18n$t("p-значения для корреляций")),
      tableOutput("corr_table_p")
    )
  )

  # Prepare and check the data
  in_data <- get_data()
  data_names <- get_names()

  # Get variables
  vars1 <- strtoi(input$si_var1_corr)
  vars2 <- strtoi(input$si_var2_corr)
  names <- list(data_names[vars1], data_names[vars2])

  # Prepare the output
  data_r <- matrix(nrow = length(vars1), ncol = length(vars2), dimnames = names)
  data_p <- data_r
  out_data_r <- data_r
  out_data_p <- data_p

  # Calculate the correlations
  lapply(1:length(vars1), function(i) {
    x <- vars1[i]

    lapply(1:length(vars2), function(j) {
      y <- vars2[j]

      result <- cor.test(in_data[[x]], in_data[[y]], method = method)

      data_r[i, j] <<- result$estimate
      data_p[i, j] <<- result$p.value
    })
  })

  # Adjust for multiple testing
  data_p <- matrix(p.adjust(as.vector(data_p), method = input$si_adj_corr), ncol = length(vars1), nrow = length(vars2), dimnames = names)

    lapply(1:length(vars1), function(i) {
    lapply(1:length(vars2), function(j) {

      # Format output (and employ a little hack to remove negative zeroes)
      result_r <- formatC(data_r[i, j], digits = 4, format = "f")
      result_p <- formatC(data_p[i, j] + 0, digits = 5, format = "f")

      if (data_p[i, j] <= settings()$p) {
        result_r <- as.character(tags$strong(result_r))
        result_p <- as.character(tags$strong(result_p))
      }
      out_data_r[i, j] <<- result_r
      out_data_p[i, j] <<- result_p
    })
  })

  # Render the results
  output$corr_table_r <- renderTable(out_data_r, rownames = TRUE, sanitize.text.function = identity)
  output$corr_table_p <- renderTable(out_data_p, rownames = TRUE, sanitize.text.function = identity)
}

ds.scatterplots <- function(method) {
  # Prepare UI
  removeUI(
    selector = "div[id^=corr_]",
    multiple = TRUE
  )
  insertUI(
    selector = "#key_div_corr_plots",
    ui = tags$div(id = "corr_plots")
  )

  # Prepare and check the data
  in_data <- get_data()
  data_names <- get_names()

  # Get variables
  vars1 <- strtoi(input$si_var1_corr)
  vars2 <- strtoi(input$si_var2_corr)

  lapply(1:length(vars1), function(i) {
    x <- vars1[i]

    lapply(1:length(vars2), function(j) {
      y <- vars2[j]

      # Drawing scatterplots
      n <- paste0("corr_plot_", i, "x", j)
      insertUI(
        selector = "#corr_plots",
        ui = tagList(
          tags$p(paste(data_names[x], " & ", data_names[y])),
          plotly::plotlyOutput(n),
          tags$br()
        )
      )

      fit <- lm(get(colnames(in_data)[y]) ~ get(colnames(in_data)[x]), data = in_data)
      output[[n]] <- plotly::renderPlotly({
        plotly::plot_ly(data = in_data, x = ~get(colnames(in_data)[x]), y = ~get(colnames(in_data)[y]), type = 'scatter', mode = 'markers') %>%
          plotly::add_lines(x = ~get(colnames(in_data)[x]), y = fitted(fit)) %>%
          plotly::layout(xaxis = list(title = data_names[x]), yaxis = list(title = data_names[y]), showlegend = FALSE)
      })
    })
  })
}
