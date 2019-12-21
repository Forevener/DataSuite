ds.mannwhitney = function()
{
	in_data = get_data()
	data_names = get_names()

	if (is.null(in_data))
	{
		showNotification("Не загружены данные для обработки!", type = "warning")
		return(NULL)
	}
	if (indep_var_ctis() == "0")
	{
		showNotification("Нет подходящих независимых переменных для данного вида анализа", type = "error")
		return(NULL)
	}

	removeUI(
		selector = "div[id^='tab4_plot']",
		multiple = TRUE)

	num_vars = ncol(in_data)
	ind_var = strtoi(indep_var_ctis())
	factors <- factor(in_data[[ind_var]])
	names = list(data_names[-ind_var], c(paste("Медиана ", levels(factors)[1]), paste("Медиана ", levels(factors)[2]), "U", "p", "Различия"))
	out_data = matrix(nrow = num_vars - 1, ncol = 5, dimnames = names)

	series = 1:num_vars
	series = series[-ind_var]
	for (i in 1:length(series))
	{
		index = series[i]
		if (is.numeric(in_data[[index]]))
		{
			medians <- aggregate(in_data[[index]], by = list(factors), FUN = "median", na.rm = TRUE)
			result = wilcox.test(in_data[[index]] ~ in_data[[ind_var]], data = in_data, correct = FALSE, na.rm = TRUE, exact = FALSE)
			out_data[i, 1] = medians[1, 2]
			out_data[i, 2] = medians[2, 2]
			out_data[i, 3] = sprintf(round(result$statistic[[1]], 3), fmt = '%#.3f')
			out_data[i, 4] = sprintf(round(result$p.value, 6), fmt = '%#.6f')
			out_data[i, 5] = ifelse(result$p.value > 0.05, "Отсутствуют", "Присутствуют")

			n = paste0("plot_", index)
			insertUI(
				selector = "#tab4bottom",
				ui = tags$div(id = paste0("tab4_plot", index), tags$p(data_names[index]), plotOutput(n)))
			local({
				l = index
				output[[n]] = renderPlot({
					ggplot(in_data, aes(in_data[[ind_var]], in_data[[l]])) +
						geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
						labs(x = data_names[ind_var], y = "Значение")
				})
			})
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

	output$out_table <- renderTable(out_data, rownames = TRUE)
	updateTabsetPanel(session, "mainTabs", selected = "Tab2")
}