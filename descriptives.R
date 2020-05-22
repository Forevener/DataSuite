ds.descriptives <- function(method = "parametric") {

  # Retrieve valid data and save original names
  valid_data <- check_data()
  main_data <- valid_data$data
  data_names <- valid_data$names

  by_group <- !is.null(valid_data$combinations)
  series <- ifelse(by_group, nrow(valid_data$combinations), 1)

  # Prepare UI
  output$results_descriptive <- renderUI({
    lapply(1:series, function(x) {
      wide_box(
        title = ifelse(by_group,
          paste0(lapply(1:ncol(valid_data$combinations), function(y) {
            paste0(colnames(valid_data$combinations)[y], " = ", as.character(valid_data$combinations[[x, y]]))
          }),
          collapse = " & "
          ),
          i18n$t("Результаты")
        ),
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
    in_data <- data.matrix(main_data[valid_data$group == index, ])

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
    # if (by_group) {
    #   rownames(out_data) <- data_names[columns]
    # } else {
      rownames(out_data) <- data_names
    # }

    # Render the resulting table
    output[[paste0("desc_main_table_", index)]] <- renderTable(out_data, rownames = TRUE)
  })
}
