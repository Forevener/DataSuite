ds.cis <- function(method = "t") {
  # Strings for output
  method_title <- switch(method,
    "t" = i18n$t("t-критерия Стьюдента"),
    "U" = i18n$t("U-критерия Манна-Уитни"),
    "D" = i18n$t("D-критерия Колмогорова-Смирнова"),
    "Z" = i18n$t("Z-критерия Уалда-Вольфовица"),
    "H" = i18n$t("H-критерия Краскела-Уоллиса"),
    "F" = i18n$t("F-критерия Уэлча"),
    "X" = i18n$t("критерия хи-квадрат")
  )
  parametric <- method == "t" || method == "F"
  multiple <- method == "H" || method == "F" || method == "X"
  method_agg <- if (parametric) {
    i18n$t("Среднее")
  } else if (method == "X") {
    i18n$t("Мода")
  } else {
    i18n$t("Медиана")
  }

  # Prepare UI
  removeUI(
    selector = "div[id^=cis_]",
    multiple = TRUE
  )
  insertUI(
    selector = "#key_div_cis_table",
    ui = tags$div(
      id = "cis_table",
      tags$p(glue(i18n$t("Сравнение независимых выборок с помощью {method_title}"))),
      tableOutput("cis_result_table")
    )
  )
  insertUI(
    selector = "#key_div_cis_plots",
    ui = tags$div(id = "cis_plots")
  )
  if (multiple) {
    header <- if (method == "X") {
      i18n$t("Межгрупповые таблицы частот")
    } else {
      pairwise_method_title <- if (parametric) {
        i18n$t("t-критерия Стьюдента")
      } else {
        i18n$t("W-критерия Уилкоксона")
      }
      glue(i18n$t("Попарные сравнения независимых выборок с помощью {pairwise_method_title}"))
    }
    insertUI(
      selector = "#key_div_cis_details",
      ui = tags$div(
        id = "cis_details",
        tags$p(header)
      )
    )
  }

  # Retrieve data, extract independent and dependent variables
  ind_var_i <- strtoi(ifelse(multiple, input$si_var_cmis, input$si_var_ctis))
  ind_var <- as.factor(get_data()[[ind_var_i]])
  if (method == "X") {
    valid_data <- check_data(-ind_var_i, num = FALSE)
  } else {
    valid_data <- check_data(-ind_var_i)
  }
  in_data <- valid_data$data
  data_names <- valid_data$names

  # Variables
  num_vars <- ncol(in_data)
  column_names <- lapply(levels(ind_var), function(x) paste0(method_agg, " ", x))
  names <- list(data_names, c(column_names, method, "p", i18n$t("Различия")))
  out_data <- matrix(nrow = num_vars, ncol = length(levels(ind_var)) + 3, dimnames = names)

  # Perform analysis
  lapply(1:num_vars, function(index) {
    # Skip zero-variance columns for parametric criteria
    if (parametric) {
      if (sum(by(in_data[[index]], ind_var, is_constant)) > 1) {
        return(NULL)
      }
    }

    # Calculate means/medians and analysis results
    if (method != "X") {
      aggs <- aggregate(in_data[[index]], by = list(ind_var), FUN = ifelse(parametric, "mean", "median"), na.rm = TRUE)
    } else {
      aggs <- aggregate(in_data[[index]], by = list(ind_var), FUN = mode, na.rm = TRUE)
      g_freq <- grouped_freq(in_data[[index]], ind_var)
    }

    result <- switch(method,
      "t" = t.test(in_data[[index]] ~ ind_var, na.rm = TRUE),
      "U" = wilcox.test(in_data[[index]] ~ ind_var, na.rm = TRUE, exact = FALSE),
      "D" = ks.test(in_data[ind_var == levels(ind_var)[1], ][[index]], in_data[ind_var == levels(ind_var)[2], ][[index]], exact = FALSE),
      "Z" = DescTools::RunsTest(in_data[[index]] ~ ind_var, na.rm = TRUE),
      "F" = oneway.test(in_data[[index]] ~ ind_var),
      "H" = kruskal.test(in_data[[index]] ~ ind_var),
      "X" = chisq.test(g_freq)
    )

    # Dirtiest hack for proper U value
    if (method == "U") {
      result2 <- wilcox.test(in_data[[index]] ~ factor(ind_var, levels = unique(ind_var)), na.rm = TRUE, exact = FALSE)
      if (result$statistic > result2$statistic) {
        result <- result2
      }
    }

    # Fill the resulting table
    for (y in 1:length(levels(ind_var))) {
      out_data[index, y] <<- if (method == "X") {
        aggs[[y, 2]]
      } else {
        sprintf(round(aggs[y, 2], 2), fmt = "%#.2f")
      }
    }
    out_data[index, y + 1] <<- sprintf(round(result$statistic[[1]], 4), fmt = "%#.4f")
    out_data[index, y + 2] <<- format_if(result$p.value, condition = paste0("{x}<=", settings()$p))
    out_data[index, y + 3] <<- ifelse(result$p.value > settings()$p, i18n$t("Отсутствуют"), i18n$t("Присутствуют"))

    if (multiple) {
      if (method != "X") {
        # Calculate pairwise comparisons
        additional <- switch(method,
          "F" = pairwise.t.test(in_data[[index]], ind_var, p.adjust.method = "BH")$p.value,
          "H" = pairwise.wilcox.test(in_data[[index]], ind_var, p.adjust.method = "BH")$p.value
        )
        additional[] <- format_if(additional, condition = paste0("{x}<=", settings()$p))
      } else {
        # Output grouped frequencies
        additional <- g_freq
      }

      # Prepare and render detailed tables
      nt <- paste0("cis_table_", index)
      insertUI(
        selector = "#cis_details",
        ui = tagList(
          tags$p(data_names[index]),
          tableOutput(nt),
          tags$br()
        )
      )
      output[[nt]] <- renderTable(additional, rownames = TRUE, sanitize.text.function = identity)
    }

    # Generate and render the plots
    n <- paste0("cis_plot_", index)
    insertUI(
      selector = "#cis_plots",
      ui = tagList(
        tags$p(data_names[index]),
        plotOutput(n),
        tags$br()
      )
    )
    if (parametric) {
      # Violin plots + mean points
      g <- ggplot(in_data, aes(ind_var, in_data[[index]])) +
        geom_violin() +
        stat_summary(fun.y = mean, geom = "point", size = 2)
    } else {
      # Violin plots + quantiles
      g <- ggplot(in_data, aes(ind_var, in_data[[index]])) +
        geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))
    }
    output[[n]] <- renderCachedPlot(
      {
        g + labs(x = get_names()[ind_var_i], y = i18n$t("Значение"))
      },
      cacheKeyExpr = list(ind_var, in_data[[index]], method)
    )
  })

  # Render UI
  output$cis_result_table <- renderTable(out_data, rownames = TRUE, sanitize.text.function = identity, na = "")
}
