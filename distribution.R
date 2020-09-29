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
        title = ifelse(by_group,
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
        title = ifelse(by_group,
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
      insertUI(
        selector = paste0("#dist_output", x),
        ui = tagList(
          tags$p(data_names[index]),
          plotOutput(n),
          tags$br()
        )
      )

      # Prepare plots for numeric data
      if (is.numeric(in_data[[index]])) {
        fact <- levels(factor(in_data[[index]]))
        dmin <- min(in_data[[index]], na.rm = TRUE)
        dmax <- max(in_data[[index]], na.rm = TRUE)
        scale_step <- min(diff(as.numeric(fact)))
        plot_breaks <- seq(dmin, dmax, by = scale_step)
        hist_call <- ggplot(in_data, aes(in_data[[index]]))
        if (length(plot_breaks) > 18) {
          new_step <- scale_step * round(length(plot_breaks) / 14)
          plot_breaks <- seq(dmin, dmax, by = new_step)
          bw <- abs(dmax - dmin) / (length(plot_breaks) - 2)
          hist_call <- hist_call + geom_histogram(fill = "white", colour = "black", binwidth = bw, center = 0.5, closed = "left", na.rm = TRUE)
        }
        else {
          bw <- abs(dmax - dmin) / (length(plot_breaks) - 1)
          hist_call <- hist_call + geom_histogram(fill = "white", colour = "black", binwidth = bw, na.rm = TRUE)
        }
        hist_call <- hist_call +
          stat_function(
            fun = function(x) {
              dnorm(x,
                mean = mean(in_data[[index]], na.rm = TRUE),
                sd = sd(in_data[[index]], na.rm = TRUE)
              ) * bw * length(na.omit(in_data[[index]]))
            },
            color = "red", size = 1, na.rm = TRUE
          ) +
          scale_x_continuous(breaks = plot_breaks)
      }
      # Prepare plots for categorical data
      else {
        hist_call <- ggplot(NULL, aes(na.omit(in_data[[index]]))) +
          geom_bar(fill = "white", colour = "black")
      }

      # Render plots
      output[[n]] <- renderCachedPlot(
        {
          hist_call + labs(x = i18n$t("Значения"), y = i18n$t("Количество"))
        },
        cacheKeyExpr = in_data[[index]]
      )
    })
  })
  jqui_resizable(ui = "div[id^='dist_plot_']")
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
        title = ifelse(by_group,
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
        "kolmogorov-smirnov" = stats::ks.test(in_data[[index]], "dnorm"),
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
