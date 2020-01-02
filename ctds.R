ds.dependent_ttest = function()
{
	removeUI(selector = "div[id^=ctds_]",
			 multiple = TRUE)

	insertUI(selector = "#key_div_ctds_table",
			 ui = tags$div(id = "ctds_table",
			 			  tags$p("Сравнение двух зависимых выборок с помощью t-критерия Стьюдента"),
			 			  tableOutput("ctds_result_table")))

	insertUI(selector = "#key_div_ctds_plots",
			 ui = tags$div(id = "ctds_plots"))

	in_data = get_data()

	if (ncol(in_data) %% 2 != 0)
	{
		showNotification("Количество столбцов нечётное - проверьте наличие нужных данных и отсутствие лишних", type = "error")
		return(NULL)
	}

	num_vars = ncol(in_data) / 2
	data_names = get_names()[1:num_vars]
	names = list(data_names, c("Среднее до", "Среднее после", "t", "p", "Различия"))
	out_data = matrix(nrow = num_vars, ncol = 5, dimnames = names)

	for (index in 1:num_vars)
	{
		if (is.numeric(in_data[[index]]))
		{
			result = t.test(in_data[[index]], in_data[[index+num_vars]], paired = TRUE, na.rm = TRUE)
			out_data[index, 1] = sprintf(round(mean(in_data[[index]], na.rm = TRUE), 2), fmt = '%#.2f')
			out_data[index, 2] = sprintf(round(mean(in_data[[index+num_vars]], na.rm = TRUE), 2), fmt = '%#.2f')
			out_data[index, 3] = sprintf(round(result$statistic[[1]], 4), fmt = '%#.4f')
			out_data[index, 4] = sprintf(round(result$p.value, 6), fmt = '%#.6f')
			out_data[index, 5] = ifelse(result$p.value > 0.05, "Отсутствуют", "Присутствуют")
		}
		else
		{
			out_data[index, 5] = "Переменная не является числовой"
		}
	}

	new_data = custom.melt(in_data, 2)

	lapply(2:(num_vars + 1), function (index)
	{
		n = paste0("plot_", index - 1)

		insertUI(selector = "#ctds_plots",
				 ui = tagList(tags$p(data_names[index - 1]),
				 			 plotOutput(n)))

		output[[n]] = renderCachedPlot({
			ggplot(new_data, aes(new_data[[1]], new_data[[index]])) +
				geom_violin() +
				stat_summary(fun.y=mean, geom="point", size=2) +
				labs(x = NULL, y = "Значение")
		}, cacheKeyExpr = list(new_data[[1]], new_data[[index]]))
	})

	output$ctds_result_table <- renderTable(out_data, rownames = TRUE, na = "")
}

ds.signtest = function()
{
	removeUI(selector = "div[id^=ctds_]",
			 multiple = TRUE)

	insertUI(selector = "#key_div_ctds_table",
			 ui = tags$div(id = "ctds_table",
			 			  tags$p("Сравнение двух зависимых выборок с помощью критерия знаков"),
			 			  tableOutput("ctds_result_table")))

	insertUI(selector = "#key_div_ctds_plots",
			 ui = tags$div(id = "ctds_plots"))

	in_data = get_data()

	if (ncol(in_data) %% 2 != 0)
	{
		showNotification("Количество столбцов нечётное - проверьте наличие нужных данных и отсутствие лишних", type = "error")
		return(NULL)
	}

	num_vars = ncol(in_data) / 2
	data_names = get_names()[1:num_vars]
	names = list(data_names, c("Медиана до", "Медиана после", "Z", "p", "Различия"))
	out_data = matrix(nrow = num_vars, ncol = 5, dimnames = names)

	for (index in 1:num_vars)
	{
		if (is.numeric(in_data[[index]]))
		{
			result = DescTools::SignTest(in_data[[index]], in_data[[index+num_vars]], na.rm = TRUE)
			out_data[index, 1] = median(in_data[[index]], na.rm = TRUE)
			out_data[index, 2] = median(in_data[[index+num_vars]], na.rm = TRUE)
			out_data[index, 3] = sprintf(round(result$statistic[[1]], 4), fmt = '%#.4f')
			out_data[index, 4] = sprintf(round(result$p.value, 6), fmt = '%#.6f')
			out_data[index, 5] = ifelse(result$p.value > 0.05, "Отсутствуют", "Присутствуют")
		}
		else
		{
			out_data[index, 5] = "Переменная не является числовой"
		}
	}

	new_data = custom.melt(in_data, 2)

	lapply(2:(num_vars + 1), function (index)
	{
		n = paste0("plot_", index - 1)
		insertUI(selector = "#ctds_plots",
				 ui = tagList(tags$p(data_names[index - 1]),
				 			 plotOutput(n)))

		output[[n]] = renderCachedPlot({
			ggplot(new_data, aes(new_data[[1]], new_data[[index]])) +
				geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
				labs(x = NULL, y = "Значение")
		}, cacheKeyExpr = list(new_data[[1]], new_data[[index]]))
	})

	output$ctds_result_table <- renderTable(out_data, rownames = TRUE, na = "")
}

ds.wilcoxonmatchedpairs = function()
{
	removeUI(selector = "div[id^=ctds_]",
			 multiple = TRUE)

	insertUI(selector = "#key_div_ctds_table",
			 ui = tags$div(id = "ctds_table",
			 			  tags$p("Сравнение двух зависимых выборок с помощью W-критерия Уилкоксона"),
			 			  tableOutput("ctds_result_table")))

	insertUI(selector = "#key_div_ctds_plots",
			 ui = tags$div(id = "ctds_plots"))

	in_data = get_data()

	if (ncol(in_data) %% 2 != 0)
	{
		showNotification("Количество столбцов нечётное - проверьте наличие нужных данных и отсутствие лишних", type = "error")
		return(NULL)
	}

	num_vars = ncol(in_data) / 2
	data_names = get_names()[1:num_vars]
	names = list(data_names, c("Медиана до", "Медиана после", "W", "p", "Различия"))
	out_data = matrix(nrow = num_vars, ncol = 5, dimnames = names)

	for (index in 1:num_vars)
	{
		if (is.numeric(in_data[[index]]))
		{
			result = wilcox.test(in_data[[index]], in_data[[index+num_vars]], paired = TRUE, na.rm = TRUE)
			out_data[index, 1] = median(in_data[[index]], na.rm = TRUE)
			out_data[index, 2] = median(in_data[[index+num_vars]], na.rm = TRUE)
			out_data[index, 3] = sprintf(round(result$statistic[[1]], 4), fmt = '%#.4f')
			out_data[index, 4] = sprintf(round(result$p.value, 6), fmt = '%#.6f')
			out_data[index, 5] = ifelse(result$p.value > 0.05, "Отсутствуют", "Присутствуют")
		}
		else
		{
			out_data[index, 5] = "Переменная не является числовой"
		}
	}

	new_data = custom.melt(in_data, 2)

	lapply(2:(num_vars + 1), function (index)
	{
		n = paste0("plot_", index - 1)
		insertUI(selector = "#ctds_plots",
				 ui = tagList(tags$p(data_names[index - 1]),
				 			 plotOutput(n)))

		output[[n]] = renderCachedPlot({
			ggplot(new_data, aes(new_data[[1]], new_data[[index]])) +
				geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
				labs(x = NULL, y = "Значение")
		}, cacheKeyExpr = list(new_data[[1]], new_data[[index]]))
	})

	output$ctds_result_table <- renderTable(out_data, rownames = TRUE, na = "")
}