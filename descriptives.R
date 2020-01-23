ds.descriptives <- function(method = "parametric") {
  # Prepare UI
  removeUI(selector = "#desc_table")
  txt = ifelse(method == "parametric", i18n$t("Параметрическая описательная статистика"), i18n$t("Непараметрическая описательная статистика"))
  insertUI(
    selector = "#key_div_desc",
    ui = tags$div(
      id = "desc_table",
      tags$p(txt),
      tableOutput("desc_main_table")
    )
  )

  # Retrieve valid data and save original names
  valid_data <- check_data()
  in_data <- data.matrix(valid_data$data)
  data_names <- valid_data$names

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
  rownames(out_data) <- data_names

  # Render the resulting table
  output$desc_main_table <- renderTable(out_data, rownames = TRUE)
}
