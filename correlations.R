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
  names <- list(data_names[vars1], data_names[vars2])

  # Prepare the output
  out_data_r <- matrix(nrow = length(vars1), ncol = length(vars2), dimnames = names)
  out_data_p <- out_data_r

  # Calculate the correlations adjusted for multiple testing
  result = psych::corr.test(in_data[vars1], in_data[vars2], method = method, adjust = input$si_adj_corr)

  lapply(1:length(vars1), function(i) {
    x <- vars1[i]

    lapply(1:length(vars2), function(j) {
      y <- vars2[j]

      # Format output (and employ a little hack to remove negative zeroes)
      result_r <- formatC(result$r[i, j], digits = 4, format = "f")
      result_p <- formatC(result$p[i, j] + 0, digits = 5, format = "f")

      if (result$p[i, j] <= settings()$p) {
        result_r <- as.character(tags$strong(result_r))
        result_p <- as.character(tags$strong(result_p))
      }
      out_data_r[i, j] <<- result_r
      out_data_p[i, j] <<- result_p

      # Drawing scatterplots
      n <- paste0("corr_plot_", i, "x", j)
      insertUI(
        selector = "#corr_plots",
        ui = tagList(
          tags$p(paste(data_names[x], " & ", data_names[y])),
          plotOutput(n),
          tags$br()
        )
      )

      output[[n]] <- renderCachedPlot(
        {
          ggplot(in_data, aes(in_data[[x]], in_data[[y]])) +
            geom_point() +
            geom_smooth() +
            labs(x = data_names[x], y = data_names[y])
        },
        cacheKeyExpr = list(in_data[[x]], in_data[[y]])
      )
    })
  })

  # Render the results
  output$corr_table_r <- renderTable(out_data_r, rownames = TRUE, sanitize.text.function = identity)
  output$corr_table_p <- renderTable(out_data_p, rownames = TRUE, sanitize.text.function = identity)
}
