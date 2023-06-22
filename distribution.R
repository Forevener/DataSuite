# TODO: Proper frequency data building, intervals for high-range data
ds.frequencytables <- function() {
  # Retrieve the data
  valid_data <- check_data(num = FALSE)
  main_data <- valid_data$data
  data_names <- valid_data$names

  # Prepare UI
  output$results_distribution <- renderUI({
    lapply(1:valid_data$series, function(x) {
      wide_box(
        title = ifelse(by_group(),
          paste0(lapply(1:ncol(valid_data$combinations), function(y) {
            paste0(colnames(valid_data$combinations)[y], " = ", as.character(valid_data$combinations[[x, y]]))
          }),
          collapse = " & "
          ),
          i18n$t("Результаты")
        ),
        tags$p(i18n$t("Частоты встречаемости")),
        tags$div(id = paste0("dist_output", x)),
        tags$br()
      )
    })
  })

  # Perform analysis
  lapply(1:valid_data$series, function(x) {
    in_data <- main_data[valid_data$group == x, , drop = FALSE]

    lapply(1:ncol(in_data), function(index) {
      # Prepare UI
      n <- paste0("dist_table_", x, "-", index)
      insertUI(
        selector = paste0("#dist_output", x),
        ui = tagList(
          tags$p(data_names[index]),
          tableOutput(n),
          tags$br()
        )
      )

      # Calculate frequencies
      tableA <- questionr::freq(in_data[[index]], digits = 2, valid = FALSE)
      tableA$n <- as.integer(tableA$n)

      # Render the results
      output[[n]] <- renderTable(tableA, rownames = TRUE)
    })
  })
}

ds.distributionplots <- function() {
  # Retrieve the data
  valid_data <- check_data(num = FALSE)
  main_data <- valid_data$data
  data_names <- valid_data$names

  # Prepare UI
  output$results_distribution <- renderUI({
    lapply(1:valid_data$series, function(x) {
      wide_box(
        title = ifelse(by_group(),
          paste0(lapply(1:ncol(valid_data$combinations), function(y) {
            paste0(colnames(valid_data$combinations)[y], " = ", as.character(valid_data$combinations[[x, y]]))
          }),
          collapse = " & "
          ),
          i18n$t("Результаты")
        ),
        tags$p(i18n$t("Графики распределения")),
        tags$div(id = paste0("dist_output", x)),
        tags$br()
      )
    })
  })

  # Perform plotting
  lapply(1:valid_data$series, function(x) {
    in_data <- main_data[valid_data$group == x, , drop = FALSE]

    lapply(1:ncol(in_data), function(index) {
      # Prepare UI
      n <- paste0("dist_plot_", x, "-", index)
      ply <- paste0(n, "_ply")
      insertUI(
        selector = paste0("#dist_output", x),
        ui = tagList(
          tags$p(data_names[index]),
          plotOutput(n),
          plotly::plotlyOutput(ply),
          tags$br()
        )
      )

      # Render plots
      output[[ply]] <- plotly::renderPlotly({
        plotly::plot_ly(data = in_data, x = ~get(colnames(in_data)[index]), type="histogram")%>%
          plotly::layout(xaxis = list(title = i18n$t("Значения")),
                 yaxis = list(title = i18n$t("Количество")))
      })
      output[[n]] <- renderCachedPlot(
        {
          n <- sym(colnames(in_data)[index])
          # Prepare plots for numeric data
          if (is.numeric(in_data[[index]])) {
            type <- isolate(input$rb_distplot_num)

            if (type == "hist") {
              ggplot(in_data, aes(!!n)) +
                geom_histogram() +
                labs(x = i18n$t("Значения"), y = i18n$t("Количество"))
            } else if (type == "bar") {
              ggplot(in_data, aes(!!n)) +
                geom_bar(na.rm = TRUE) +
                labs(x = i18n$t("Значения"), y = i18n$t("Количество"))
            } else if (type == "density") {
              ggplot(in_data, aes(!!n)) +
                geom_density() +
                labs(x = i18n$t("Значения"), y = i18n$t("Плотность"))
            } else if (type == "ecdf") {
              ggplot(in_data, aes(!!n)) +
                stat_ecdf(geom = "step", pad = FALSE) +
                labs(x = i18n$t("Значения"), y = i18n$t("Общее количество"))
            } else if (type == "poly") {
              ggplot(in_data, aes(!!n)) +
                geom_freqpoly() +
                labs(x = i18n$t("Значения"), y = i18n$t("Количество"))
            } else if (type == "bw") {
              ggplot(in_data, aes(x = 1, y = !!n)) +
                geom_boxplot() +
                labs(y = i18n$t("Значения")) +
                theme(axis.title.x = element_blank(),
                      axis.text.x = element_blank(),
                      axis.ticks.x = element_blank())
            } else if (type == "viol") {
              ggplot(in_data, aes(x = 1, y = !!n)) +
                geom_violin() +
                labs(x = i18n$t("Плотность"), y = i18n$t("Значения"))
            } else if (type == "dot") {
              ggplot(in_data, aes(!!n)) +
                geom_dotplot() +
                labs(x = i18n$t("Значения"), y = i18n$t("Плотность"))
            }
          }
          # Prepare plots for categorical data
          else {
            type <- isolate(input$rb_distplot_cat)

            if (type == "bar") {
              ggplot(in_data, aes(!!n)) +
                geom_bar() +
                scale_x_discrete(na.translate = FALSE) +
                labs(x = i18n$t("Значения"), y = i18n$t("Количество"))
            } else if (type == "pie") {
              d <- as.data.frame(table(in_data[[index]]))
              colnames(d)[1] <- "value"

              #https://www.r-graph-gallery.com/piechart-ggplot2.html
              ggplot(d, aes(x = "", y = Freq, fill = value)) +
                geom_bar(stat = "identity", width = 1, color = "white") +
                coord_polar("y", start = 0) +
                theme_void() +
                theme(legend.title=element_blank())
            }
          }
        },
        cacheKeyExpr = list(in_data[[index]], isolate(input$rb_distplot_cat), isolate(input$rb_distplot_num))
      )
    })
  })
  jqui_resizable(ui = "div[id^='dist_plot_']", options = list(alsoResize = "div[class~='ui-resizable']"))
}

ds.normalitycheck <- function(method) {
  # Retrieve the data
  valid_data <- check_data(zeroVar = TRUE)
  main_data <- valid_data$data
  data_names <- valid_data$names

  # Prepare UI
  method_title <- switch(method,
    "kolmogorov-smirnov" = i18n$t("Критерий Колмогорова-Смирнова"),
    "lilliefors" = i18n$t("Критерий Лиллиефорса"),
    "shapiro-wilk" = i18n$t("Критерий Шапиро-Уилка")
  )
  output$results_distribution <- renderUI({
    lapply(1:valid_data$series, function(x) {
      wide_box(
        title = ifelse(by_group(),
          paste0(lapply(1:ncol(valid_data$combinations), function(y) {
            paste0(colnames(valid_data$combinations)[y], " = ", as.character(valid_data$combinations[[x, y]]))
          }),
          collapse = " & "
          ),
          i18n$t("Результаты")
        ),
        tags$p(glue(i18n$t("{method_title} для проверки нормальности распределения"))),
        tableOutput(paste0("dist_norm_table_", x)),
        tags$br()
      )
    })
  })

  # Perform analysis
  lapply(1:valid_data$series, function(x) {
    in_data <- main_data[valid_data$group == x, , drop = FALSE]

    # Variables
    num_vars <- ncol(in_data)
    names <- list(data_names, c(ifelse(method == "shapiro-wilk", "W", "D"), "p", i18n$t("Распределение")))
    out_data <- matrix(nrow = num_vars, ncol = 3, dimnames = names)

    for (index in 1:num_vars)
    {
      result <- switch(method,
        "kolmogorov-smirnov" = ks.test(in_data[[index]], "dnorm"),
        "lilliefors" = DescTools::LillieTest(in_data[[index]]),
        "shapiro-wilk" = shapiro.test(in_data[[index]])
      )
      out_data[index, 1] <- sprintf(round(result$statistic, 5), fmt = "%#.5f")
      out_data[index, 2] <- sprintf(round(result$p.value, 6), fmt = "%#.6f")
      out_data[index, 3] <- ifelse(result$p.value > 0.05, i18n$t("Нормальное"), i18n$t("Отличается от нормального"))
    }

    # Render the result
    output[[paste0("dist_norm_table_", x)]] <- renderTable(out_data, rownames = TRUE)
  })
}
