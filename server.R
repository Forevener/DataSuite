library(shiny)
library(rhandsontable)
library(xlsx)

shinyServer(function(input, output, session) {
  enc = reactive({input$radio})
  indep_var_ctis = reactive({input$ctis_dropdown})
  indep_var_cmis = reactive({input$cmis_dropdown})
  in_data = NULL
  observeEvent(input$upload, {
    
    in_file <- input$upload
    
    if (is.null(in_file))
      return(NULL)
    
    in_data <<- read.xlsx(file = in_file$datapath, sheetIndex = 1, header = TRUE, encoding = enc())

    selections = list()
    for (index in 1:ncol(in_data))
    {
      if (length(levels(factor(in_data[[index]]))) == 2)
      {
        names(index) = colnames(in_data[index])
        selections = append(selections, index)
      }
    }
    if (length(selections) >= 1)
      updateSelectInput(session, "ctis_dropdown", choices = selections, selected = selections[1])
    
    selections = list()
    for (index in 1:ncol(in_data))
    {
        names(index) = colnames(in_data[index])
        selections = append(selections, index)
    }
    if (length(selections) >= 1)
      updateSelectInput(session, "cmis_dropdown", choices = selections, selected = selections[1])

    output$in_table <- renderRHandsontable({rhandsontable(in_data)})
  })
  
  observeEvent(input$dp, {
    if (is.null(in_data))
    {
      showNotification("Не загружены данные для обработки!")
      return(NULL)
    }
      
      num_vars = ncol(in_data)
      names = list(colnames(in_data), c("Минимум", "Нижняя граница нормы", "Среднее", "Верхняя граница нормы", "Максимум", "Стандартное отклонение"))
      out_data = matrix(nrow = num_vars, ncol = 6, dimnames = names)
      
      for (index in 1:num_vars)
      {
        if (is.numeric(in_data[[index]]))
        {
          rmean = round(mean(in_data[[index]], na.rm = TRUE), 2)
          rsd = round(sd(in_data[[index]], na.rm = TRUE), 2)
          out_data[index, 1] = min(in_data[[index]], na.rm = TRUE)
          out_data[index, 2] = rmean - rsd
          out_data[index, 3] = rmean
          out_data[index, 4] = rmean + rsd
          out_data[index, 5] = max(in_data[[index]], na.rm = TRUE)
          out_data[index, 6] = rsd
        }
        else
        {
          out_data[index, 1] = "Переменная не является числовой"
          out_data[index, 2] = ""
          out_data[index, 3] = ""
          out_data[index, 4] = ""
          out_data[index, 5] = ""
          out_data[index, 6] = ""
        }
      }
      updateTabsetPanel(session, "mainTabs", selected = "Tab2")
      output$out_table <- renderTable(out_data, rownames = TRUE)
    })
    
    observeEvent(input$dnp, {
      if (is.null(in_data))
      {
        showNotification("Не загружены данные для обработки!")
        return(NULL)
      }
      
      num_vars = ncol(in_data)
      names = list(colnames(in_data), c("Минимум", "Нижний квартиль", "Медиана", "Верхний квартиль", "Максимум"))
      out_data = matrix(nrow = num_vars, ncol = 5, dimnames = names)
      
      for (index in 1:num_vars)
      {
        if (is.numeric(in_data[[index]]))
        {
          result = quantile(in_data[[index]], na.rm = TRUE)
          out_data[index, 1] = result[[1]]
          out_data[index, 2] = result[[2]]
          out_data[index, 3] = result[[3]]
          out_data[index, 4] = result[[4]]
          out_data[index, 5] = result[[5]]
        }
        else
        {
          out_data[index, 1] = "Переменная не является числовой"
          out_data[index, 2] = ""
          out_data[index, 3] = ""
          out_data[index, 4] = ""
          out_data[index, 5] = ""
        }
      }
      updateTabsetPanel(session, "mainTabs", selected = "Tab2")
      output$out_table <- renderTable(out_data, rownames = TRUE)
    })
      
  observeEvent(input$sw, {
    if (is.null(in_data))
    {
      showNotification("Не загружены данные для обработки!")
      return(NULL)
    }

    num_vars = ncol(in_data)
    names = list(colnames(in_data), c("W", "p", "Распределение"))
    out_data = matrix(nrow = num_vars, ncol = 3, dimnames = names)
    
    for (index in 1:num_vars)
    {
      if (is.numeric(in_data[[index]]))
      {
        result = shapiro.test(in_data[[index]])
        out_data[index, 1] = sprintf(round(result$statistic, 5), fmt = '%#.5f')
        out_data[index, 2] = sprintf(round(result$p.value, 6), fmt = '%#.6f')
        out_data[index, 3] = ifelse(result$p.value > 0.05, "Нормальное", "Отличается от нормального")
      }
      else
      {
        out_data[index, 1] = "-"
        out_data[index, 2] = "-"
        out_data[index, 3] = "Переменная не является числовой"
      }
    }
    updateTabsetPanel(session, "mainTabs", selected = "Tab2")
    output$out_table <- renderTable(out_data, rownames = TRUE)
  })
  
  observeEvent(input$mw, {
    if (is.null(in_data))
    {
      showNotification("Не загружены данные для обработки!")
      return(NULL)
    }
    if (indep_var_ctis() == "0")
    {
      showNotification("Нет подходящих независимых переменных для данного вида анализа")
      return(NULL)
    }
    
    ind_var = strtoi(indep_var_ctis())
    num_vars = ncol(in_data)
    factors <- factor(in_data[[ind_var]])
    names = list(colnames(in_data[, ind_var * -1]), c(paste("Медиана ", levels(factors)[1]), paste("Медиана ", levels(factors)[2]), "U", "p", "Различия"))
    out_data = matrix(nrow = num_vars - 1, ncol = 5, dimnames = names)
    
    series = 1:num_vars
    series = series[-ind_var]
    for (i in 1:length(series))
    {
      index = series[i]
      if (is.numeric(in_data[[index]]))
      {
        medians <- aggregate(in_data[[index]], by = list(factors), FUN = "median", na.rm = TRUE)
        result = wilcox.test(in_data[[index]] ~ in_data[[ind_var]], data = in_data, correct = FALSE)
        out_data[i, 1] = medians[1, 2]
        out_data[i, 2] = medians[2, 2]
        out_data[i, 3] = sprintf(round(result$statistic[[1]], 3), fmt = '%#.3f')
        out_data[i, 4] = sprintf(round(result$p.value, 6), fmt = '%#.6f')
        out_data[i, 5] = ifelse(result$p.value > 0.05, "Отсутствуют", "Присутствуют")
      }
      else
      {
        out_data[i, 1] = "-"
        out_data[i, 2] = "-"
        out_data[i, 3] = "-"
        out_data[i, 4] = "-"
        out_data[i, 5] = "Переменная не является числовой"
      }
    }
    updateTabsetPanel(session, "mainTabs", selected = "Tab2")
    output$out_table <- renderTable(out_data, rownames = TRUE)
  })
  
  observeEvent(input$ti, {
    if (is.null(in_data))
    {
      showNotification("Не загружены данные для обработки!")
      return(NULL)
    }
    if (indep_var_ctis() == "0")
    {
      showNotification("Нет подходящих независимых переменных для данного вида анализа")
      return(NULL)
    }
    
    ind_var = strtoi(indep_var_ctis())
    num_vars = ncol(in_data)
    factors <- factor(in_data[[ind_var]])
    names = list(colnames(in_data[, ind_var * -1]), c(paste("Среднее ", levels(factors)[1]), paste("Среднее ", levels(factors)[2]), "T", "p", "Различия"))
    out_data = matrix(nrow = num_vars - 1, ncol = 5, dimnames = names)
    
    series = 1:num_vars
    series = series[-ind_var]
    for (i in 1:length(series))
    {
      index = series[i]
      if (is.numeric(in_data[[index]]))
      {
        means <- aggregate(in_data[[index]], by = list(factors), FUN = "mean", na.rm = TRUE)
        result = t.test(in_data[[index]] ~ in_data[[ind_var]], data = in_data)
        out_data[i, 1] = sprintf(round(means[1, 2], 2), fmt = '%#.2f')
        out_data[i, 2] = sprintf(round(means[2, 2], 2), fmt = '%#.2f')
        out_data[i, 3] = sprintf(round(result$statistic[[1]], 4), fmt = '%#.4f')
        out_data[i, 4] = sprintf(round(result$p.value, 6), fmt = '%#.6f')
        out_data[i, 5] = ifelse(result$p.value > 0.05, "Отсутствуют", "Присутствуют")
      }
      else
      {
        out_data[i, 1] = "-"
        out_data[i, 2] = "-"
        out_data[i, 3] = "-"
        out_data[i, 4] = "-"
        out_data[i, 5] = "Переменная не является числовой"
      }
    }
    updateTabsetPanel(session, "mainTabs", selected = "Tab2")
    output$out_table <- renderTable(out_data, rownames = TRUE)
  })
  
  observeEvent(input$kw, {
    if (is.null(in_data))
    {
      showNotification("Не загружены данные для обработки!")
      return(NULL)
    }
    if (indep_var_cmis() == "0")
    {
      showNotification("Не выбрана независимая переменная!")
      return(NULL)
    }
    
    ind_var = strtoi(indep_var_cmis())
    num_vars = ncol(in_data)
    factors <- factor(in_data[[ind_var]])
    colname = c()
    
    for (index in 1:length(levels(factors)))
      colname = append(colname, paste("Медиана ", levels(factors)[index]))

    names = list(colnames(in_data[, ind_var * -1]), append(colname, c("H", "p", "Различия")))
    out_data = matrix(nrow = num_vars - 1, ncol = length(levels(factors)) + 3, dimnames = names)
    
    series = 1:num_vars
    series = series[-ind_var]
    for (i in 1:length(series))
    {
      index = series[i]
      if (is.numeric(in_data[[index]]))
      {
        medians <- aggregate(in_data[[index]], by = list(factors), FUN = "median", na.rm = TRUE)
        result = kruskal.test(in_data[[index]] ~ in_data[[ind_var]], data = in_data)
        for (y in 1:length(levels(factors)))
          out_data[i, y] = medians[y, 2]

        out_data[i, y + 1] = sprintf(round(result$statistic[[1]], 3), fmt = '%#.3f')
        out_data[i, y + 2] = sprintf(round(result$p.value, 6), fmt = '%#.6f')
        out_data[i, y + 3] = ifelse(result$p.value > 0.05, "Отсутствуют", "Присутствуют")
      }
      else
      {
        for (y in 1:length(levels(factors)))
          out_data[i, y] = "-"
        
        out_data[i, y + 1] = "-"
        out_data[i, y + 2] = "-"
        out_data[i, y + 3] = "Переменная не является числовой"
      }
    }
    updateTabsetPanel(session, "mainTabs", selected = "Tab2")
    output$out_table <- renderTable(out_data, rownames = TRUE)
  })
  
  observeEvent(input$ff, {
    if (is.null(in_data))
    {
      showNotification("Не загружены данные для обработки!")
      return(NULL)
    }
    if (indep_var_cmis() == "0")
    {
      showNotification("Не выбрана независимая переменная!")
      return(NULL)
    }
    
    ind_var = strtoi(indep_var_cmis())
    num_vars = ncol(in_data)
    factors <- factor(in_data[[ind_var]])
    colname = c()
    
    for (index in 1:length(levels(factors)))
      colname = append(colname, paste("Среднее ", levels(factors)[index]))
    
    names = list(colnames(in_data[, ind_var * -1]), append(colname, c("F", "p", "Различия")))
    out_data = matrix(nrow = num_vars - 1, ncol = length(levels(factors)) + 3, dimnames = names)
    
    series = 1:num_vars
    series = series[-ind_var]
    for (i in 1:length(series))
    {
      index = series[i]
      if (is.numeric(in_data[[index]]))
      {
        means <- aggregate(in_data[[index]], by = list(factors), FUN = "mean", na.rm = TRUE)
        result = summary(aov(in_data[[index]] ~ in_data[[ind_var]], data = in_data))
        for (y in 1:length(levels(factors)))
          out_data[i, y] = sprintf(round(means[y, 2], 2), fmt = '%#.2f')

        out_data[i, y + 1] = sprintf(round(result[[1]][1, 4], 4), fmt = '%#.4f')
        out_data[i, y + 2] = sprintf(round(result[[1]][1, 5], 6), fmt = '%#.6f')
        out_data[i, y + 3] = ifelse(result[[1]][1, 5] > 0.05, "Отсутствуют", "Присутствуют")
      }
      else
      {
        for (y in 1:length(levels(factors)))
          out_data[i, y] = "-"
        
        out_data[i, y + 1] = "-"
        out_data[i, y + 2] = "-"
        out_data[i, y + 3] = "Переменная не является числовой"
      }
    }
    updateTabsetPanel(session, "mainTabs", selected = "Tab2")
    output$out_table <- renderTable(out_data, rownames = TRUE)
  })
})