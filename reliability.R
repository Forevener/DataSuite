# TODO: Reversed items selection, more descriptives for drop table
ds.reliability <- function() {
  # Prepare UI
  removeUI(selector = "#reliability_tables")
  insertUI(
    selector = "#key_div_reli_table",
    ui = tags$div(
      id = "reliability_tables",
      tags$p(i18n$t("Результаты анализа надёжности")),
      tableOutput("reliability_table_1"),
      tags$p(i18n$t("Таблица отбросов")),
      tableOutput("reliability_table_2")
    )
  )

  # Filter numeric valid data
  valid_data <- check_data(get_data()[strtoi(input$si_reli_vars)], nas = TRUE)
  in_data <- valid_data$data
  data_names <- valid_data$names

  # Calculate Alpha and Omega
  if (!is.null(input$si_reli_reversed_items))
  {
    reversed_int <- strtoi(input$si_reli_reversed_items)
    reversed_keys <- rep(1, ncol(in_data))
    reversed_keys[reversed_int] <- -1
    resultA <- psych::alpha(as.data.frame(in_data), keys = reversed_keys)
    resultO <- psych::omega(in_data, plot = FALSE, key = reversed_keys)
  }
  else
  {
    resultA <- psych::alpha(as.data.frame(in_data))
    resultO <- psych::omega(in_data, plot = FALSE)
  }

  # Calculate composite reliability from https://www.r-bloggers.com/five-ways-to-calculate-internal-consistency/
  items <- paste0(colnames(in_data), collapse = "+")
  model <- paste0("F1 =~ ", items)
  fit <- lavaan::cfa(model, data = in_data)
  sl <- lavaan::standardizedSolution(fit)
  sl <- sl$est.std[sl$op == "=~"]
  names(sl) <- colnames(in_data)
  re <- 1 - sl^2
  cr <- sum(sl)^2 / (sum(sl)^2 + sum(re))

  # Prepare main table
  tableA <- i18n$t("Показатель") %isnameof% data.frame(c(
    i18n$t("Альфа Кронбаха") %isnameof% resultA$total$raw_alpha,
    i18n$t("Лямбда-6 Гуттмана") %isnameof% resultA$total$`G6(smc)`,
    i18n$t("Омега МакДональда") %isnameof% resultO$omega.tot,
    i18n$t("Композитная надёжность") %isnameof% cr
  ))

  # Prepare drop table
  tableB <- data.frame(resultA$alpha.drop[c(1, 3)])
  colnames(tableB) <- c(i18n$t("Альфа при отбросе"), i18n$t("Лямбда-6 при отбросе"))
  rownames(tableB) <- data_names

  # Render UI
  output[["reliability_table_1"]] <- renderTable(tableA, rownames = TRUE, digits = 4)
  output[["reliability_table_2"]] <- renderTable(tableB, rownames = TRUE, digits = 4)
}
