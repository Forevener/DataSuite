power_pwr <- reactive(
  pwr::pwr.p.test(
    h = input$sl_pwr_es,
    n = input$sl_pwr_sample_size,
    sig.level = input$sl_pwr_p_value,
    power = NULL
  )$power
)

power_sig <- reactive(
  pwr::pwr.p.test(
    h = input$sl_pwr_es,
    n = input$sl_pwr_sample_size,
    sig.level = NULL,
    power = input$sl_pwr_power
  )$sig.level
)

power_n <- reactive(
  ceiling(
    pwr::pwr.p.test(
      h = input$sl_pwr_es,
      n = NULL,
      sig.level = input$sl_pwr_p_value,
      power = input$sl_pwr_power
    )$n
  )
)

power_es <- reactive(
  pwr::pwr.p.test(
    h = NULL,
    n = input$sl_pwr_sample_size,
    sig.level = input$sl_pwr_p_value,
    power = input$sl_pwr_power
  )[1]
)

output$to_pwr_output <- renderTable(
  {
    result <- data.frame(power_es(), power_pwr(), power_sig(), power_n())
    colnames(result) <- c(
      i18n$t("Величина эффекта"),
      i18n$t("Мощность"),
      i18n$t("Вероятность ошибки 1-го рода"),
      i18n$t("Размер выборки")
    )
    result
  },
  digits = 4
)
