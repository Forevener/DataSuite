ds.independent_ttest = function()
{
	removeUI(selector = "div[id^=ctis_]",
			 multiple = TRUE)

	insertUI(selector = "#key_div_ctis_table",
			 ui = tags$div(id = "ctis_table",
			 			  tags$p("Сравнение двух независимых выборок с помощью t-критерия Стьюдента"),
			 			  tableOutput("ctis_result_table")))

	insertUI(selector = "#key_div_ctis_plots",
			 ui = tags$div(id = "ctis_plots"))

	in_data = get_data()
	data_names = get_names()

	if (indep_var_ctis() == "0")
	{
		showNotification("Нет подходящих независимых переменных для данного вида анализа", type = "error")
		return(NULL)
	}

	num_vars = ncol(in_data)
	ind_var = strtoi(indep_var_ctis())
	factors <- factor(in_data[[ind_var]])
	names = list(data_names[-ind_var], c(paste("Среднее ", levels(factors)[1]), paste("Среднее ", levels(factors)[2]), "T", "p", "Различия"))
	out_data = matrix(nrow = num_vars - 1, ncol = 5, dimnames = names)

	series = 1:num_vars
	series = series[-ind_var]
	lapply(1:length(series), function (i)
	{
		index = series[i]
		if (is.numeric(in_data[[index]]))
		{
			means = aggregate(in_data[[index]], by = list(factors), FUN = "mean", na.rm = TRUE)
			result = t.test(in_data[[index]] ~ in_data[[ind_var]], data = in_data, na.rm = TRUE)
			out_data[i, 1] <<- sprintf(round(means[1, 2], 2), fmt = '%#.2f')
			out_data[i, 2] <<- sprintf(round(means[2, 2], 2), fmt = '%#.2f')
			out_data[i, 3] <<- sprintf(round(result$statistic[[1]], 4), fmt = '%#.4f')
			out_data[i, 4] <<- sprintf(round(result$p.value, 6), fmt = '%#.6f')
			out_data[i, 5] <<- ifelse(result$p.value > 0.05, "Отсутствуют", "Присутствуют")

			n = paste0("plot_", index)
			insertUI(selector = "#ctis_plots",
					 ui = tagList(tags$p(data_names[index]),
					 			 plotOutput(n)))

			output[[n]] = renderCachedPlot({
				ggplot(in_data, aes(in_data[[ind_var]], in_data[[index]])) +
					geom_violin() +
					stat_summary(fun.y=mean, geom="point", size=2) +
					labs(x = data_names[ind_var], y = "Значение")
			}, cacheKeyExpr = list(in_data[[ind_var]], in_data[[index]]))
		}
		else
		{
			out_data[i, 5] <<- "Переменная не является числовой"
		}
	})

	output$ctis_result_table <- renderTable(out_data, rownames = TRUE, na = "")
}

ds.mannwhitney = function()
{
	removeUI(selector = "div[id^=ctis_]",
			 multiple = TRUE)

	insertUI(selector = "#key_div_ctis_table",
			 ui = tags$div(id = "ctis_table",
			 			  tags$p("Сравнение двух независимых выборок с помощью U-критерия Манна-Уитни"),
			 			  tableOutput("ctis_result_table")))

	insertUI(selector = "#key_div_ctis_plots",
			 ui = tags$div(id = "ctis_plots"))

	in_data = get_data()
	data_names = get_names()

	if (indep_var_ctis() == "0")
	{
		showNotification("Нет подходящих независимых переменных для данного вида анализа", type = "error")
		return(NULL)
	}

	num_vars = ncol(in_data)
	ind_var = strtoi(indep_var_ctis())
	factors <- factor(in_data[[ind_var]])
	names = list(data_names[-ind_var], c(paste("Медиана ", levels(factors)[1]), paste("Медиана ", levels(factors)[2]), "U", "p", "Различия"))
	out_data = matrix(nrow = num_vars - 1, ncol = 5, dimnames = names)

	series = 1:num_vars
	series = series[-ind_var]
	lapply(1:length(series), function (i)
	{
		index = series[i]
		if (is.numeric(in_data[[index]]))
		{
			medians = aggregate(in_data[[index]], by = list(factors), FUN = "median", na.rm = TRUE)
			result = wilcox.test(in_data[[index]] ~ in_data[[ind_var]], data = in_data, correct = FALSE, na.rm = TRUE, exact = FALSE)
			out_data[i, 1] <<- medians[1, 2]
			out_data[i, 2] <<- medians[2, 2]
			out_data[i, 3] <<- sprintf(round(result$statistic[[1]], 3), fmt = '%#.3f')
			out_data[i, 4] <<- sprintf(round(result$p.value, 6), fmt = '%#.6f')
			out_data[i, 5] <<- ifelse(result$p.value > 0.05, "Отсутствуют", "Присутствуют")

			n = paste0("plot_", index)
			insertUI(selector = "#ctis_plots",
					 ui = tagList(tags$p(data_names[index]),
					 			 plotOutput(n)))
			output[[n]] = renderCachedPlot({
				ggplot(in_data, aes(in_data[[ind_var]], in_data[[index]])) +
					geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
					labs(x = data_names[ind_var], y = "Значение")
			}, cacheKeyExpr = list(in_data[[ind_var]], in_data[[index]]))

		}
		else
		{
			out_data[i, 5] <<- "Переменная не является числовой"
		}
	})

	output$ctis_result_table <- renderTable(out_data, rownames = TRUE, na = "")
}