ds.descriptives <- function(method = "parametric") {
  # Retrieve valid data and save original names
  valid_data <- check_data()
  main_data <- valid_data$data
  data_names <- valid_data$names

  # Prepare UI
  box_title <- switch(method,
    "parametric" = i18n$t("Параметрическая описательная статистика"),
    "nonparametric" = i18n$t("Непараметрическая описательная статистика"),
    "custom" = i18n$t("Описательная статистика")
  )
  output$results_descriptive <- renderUI({
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
        tags$p(box_title),
        tableOutput(paste0("desc_main_table_", x)),
        tags$br()
      )
    })
  })

  lapply(1:valid_data$series, function(x) {
    in_data <- data.matrix(main_data[valid_data$group == x, ])

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
    else if (method == "nonparametric") {
      out_data <- data.frame(colQuantiles(in_data, na.rm = TRUE))
      colnames(out_data) <- c(i18n$t("Минимум"), i18n$t("Нижний квартиль"), i18n$t("Медиана"), i18n$t("Верхний квартиль"), i18n$t("Максимум"))
    } else {
      out_data <- data.frame()[1:ncol(in_data), ]

      if (i18n$t("Крайние значения") %in% input$cbg_custom_desc) {
        out_data[[i18n$t("Минимум")]] <- colMins(in_data, na.rm = TRUE)
        out_data[[i18n$t("Максимум")]] <- colMaxs(in_data, na.rm = TRUE)
      }

      if (i18n$t("Размах") %in% input$cbg_custom_desc) {
        out_data[[i18n$t("Размах")]] <- colMaxs(in_data, na.rm = TRUE) - colMins(in_data, na.rm = TRUE)
      }
      if (i18n$t("Среднее") %in% input$cbg_custom_desc) {
        out_data[[i18n$t("Среднее")]] <- colMeans2(in_data, na.rm = TRUE)
      }
      if (i18n$t("Стандартная ошибка среднего") %in% input$cbg_custom_desc) {
        out_data[[i18n$t("Стандартная ошибка среднего")]] <- apply(in_data, 2, function(x) {
          se(x, na.rm = TRUE)
        })
      }
      if (i18n$t("Среднее абсолютное отклонение") %in% input$cbg_custom_desc) {
        out_data[[i18n$t("Среднее абсолютное отклонение")]] <- apply(in_data, 2, function(x) {
          DescTools::MeanAD(x, na.rm = TRUE)
        })
      }
      if (i18n$t("Дисперсия") %in% input$cbg_custom_desc) {
        out_data[[i18n$t("Дисперсия")]] <- colVars(in_data, na.rm = TRUE)
      }
      if (i18n$t("Стандартное отклонение") %in% input$cbg_custom_desc) {
        out_data[[i18n$t("Стандартное отклонение")]] <- colSds(in_data, na.rm = TRUE)
      }
      if (i18n$t("Коридор нормы") %in% input$cbg_custom_desc) {
        out_data[[i18n$t("Нижняя граница нормы")]] <- colMeans2(in_data, na.rm = TRUE) - colSds(in_data, na.rm = TRUE)
        out_data[[i18n$t("Верхняя граница нормы")]] <- colMeans2(in_data, na.rm = TRUE) + colSds(in_data, na.rm = TRUE)
      }
      if (i18n$t("Доверительный интервал") %in% input$cbg_custom_desc) {
        t_add <- t(
          apply(in_data, 2, function(x) {
            DescTools::MeanCI(x, na.rm = TRUE)[-1]
          })
        )
        colnames(t_add) <- c(i18n$t("Нижняя граница ДИ"), i18n$t("Верхняя граница ДИ"))
        out_data <- cbind(out_data, t_add)
      }
      if (i18n$t("Мода") %in% input$cbg_custom_desc) {
        t_add <- t(
          apply(in_data, 2, function(x) {
            m <- mode(x, na.rm = TRUE)
            c(ifelse(m$mode == "multiple", i18n$t("Несколько"), m$mode), m$frequency)
          })
        )
        colnames(t_add) <- c(i18n$t("Мода"), i18n$t("Частота моды"))
        out_data <- cbind(out_data, t_add)
      }
      if (i18n$t("Медиана") %in% input$cbg_custom_desc) {
        out_data[[i18n$t("Медиана")]] <- colMedians(in_data, na.rm = TRUE)
      }
      if (i18n$t("Квартили") %in% input$cbg_custom_desc) {
        q <- colQuantiles(in_data, na.rm = TRUE)
        out_data[[i18n$t("Нижний квартиль")]] <- q[, 2]
        out_data[[i18n$t("Верхний квартиль")]] <- q[, 4]
      }
      if (i18n$t("Квартильный размах") %in% input$cbg_custom_desc) {
        out_data[[i18n$t("Квартильный размах")]] <- colIQRs(in_data, na.rm = TRUE)
      }
      if (i18n$t("Эксцесс") %in% input$cbg_custom_desc) {
        out_data[[i18n$t("Эксцесс")]] <- apply(in_data, 2, function(x) {
          moments::kurtosis(x, na.rm = TRUE)
        })
      }
      if (i18n$t("Асимметрия") %in% input$cbg_custom_desc) {
        out_data[[i18n$t("Асимметрия")]] <- apply(in_data, 2, function(x) {
          moments::skewness(x, na.rm = TRUE)
        })
      }
    }

    # Set pretty names
    rownames(out_data) <- data_names

    # Render the resulting table
    output[[paste0("desc_main_table_", x)]] <- renderTable(out_data, rownames = TRUE)
  })
}
