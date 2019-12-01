ds.independent_ttest = function()
{
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

	num_vars = ncol(in_data)
	ind_var = strtoi(indep_var_ctis())
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
			result = t.test(in_data[[index]] ~ in_data[[ind_var]], data = in_data, na.rm = TRUE)
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

	removeUI(
		selector = "div[id^='tab4_plot']",
		multiple = TRUE)

	for (index in 1:num_vars)
	{
		if (is.numeric(in_data[[index]]))
		{
			n = paste0("plot_", index)
			insertUI(
				selector = "#tab4bottom",
				ui = tags$div(id = paste0("tab4_plot", index), tags$p(colnames(in_data)[index]), plotOutput(n)))
			local({
				l = index
				output[[n]] = renderPlot({
					ggplot(in_data, aes(in_data[[ind_var]], in_data[[l]])) +
						geom_violin() +
						stat_summary(fun.y=mean, geom="point", size=2) +
						labs(x = colnames(in_data[ind_var]), y = "Значение")
				})
			})
		}
	}

	output$out_table <- renderTable(out_data, rownames = TRUE)
	updateTabsetPanel(session, "mainTabs", selected = "Tab2")
}