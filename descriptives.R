ds.descriptives <- function(method = "parametric") {
  grouping_vars <- strtoi(input$cbg_by_group)

  # Retrieve valid data and save original names
  valid_data <- check_data(ignoreCols = grouping_vars)
  main_data <- valid_data$data
  data_names <- valid_data$names
  grouping_names <- colnames(get_data()[grouping_vars])

  source("by_group_combinations.R", encoding = "utf-8", local = TRUE)

  # Prepare UI
  output$results_descriptive <- renderUI({
    lapply(1:series, function(x) {
      wide_box(
        title = ifelse(length(grouping_vars) > 0,
                       paste0(lapply(1:ncol(combinations), function(y) {
                         paste0(colnames(combinations)[y], " = ", as.character(combinations[[x, y]]))
                       }),
                       collapse = " & "),
                       i18n$t("Результаты")),
        tags$p(ifelse(method == "parametric",
          i18n$t("Параметрическая описательная статистика"),
          i18n$t("Непараметрическая описательная статистика")
        )),
        tableOutput(paste0("desc_main_table_", x)),
        tags$br()
      )
    })
  })

  lapply(1:series, function(index) {
    source("by_group_filter.R", encoding = "utf-8", local = TRUE)
    in_data <- data.matrix(in_data)

    # Perform analysis
    if (method == "parametric") {
      means <- colMeans2(in_data, na.rm = TRUE)
      sds <- colSds(in_data, na.rm = TRUE)
      out_data <- i18n$t("Минимум") %isnameof% data.frame(colMins(in_data, na.rm = TRUE))
      out_data[[i18n$t("Нижняя граница нормы")]] <- means - sds
      out_data[[i18n$t("Среднее")]] <- means
      out_data[[i18n$t("Верхняя граница нормы")]] <- means + sds
      out_data[[i18n$t("Максимум")]] <- colMaxs(in_data, na.rm = TRUE)
      out_data[[i18n$t("Стандартное отклонение")]] <- sds
    }
    else {
      out_data <- data.frame(colQuantiles(in_data, na.rm = TRUE))
      colnames(out_data) <- c(i18n$t("Минимум"), i18n$t("Нижний квартиль"), i18n$t("Медиана"), i18n$t("Верхний квартиль"), i18n$t("Максимум"))
    }

    # Set pretty names
    if (length(grouping_vars) > 0) {
      rownames(out_data) <- data_names[columns]
    } else {
      rownames(out_data) <- data_names
    }

    # Render the resulting table
    output[[paste0("desc_main_table_", index)]] <- renderTable(out_data, rownames = TRUE)
  })
}
