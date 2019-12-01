ds.onewayanova <- function()
{
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

	num_vars = ncol(in_data)
	ind_var = strtoi(indep_var_cmis())
	factors <- factor(in_data[[ind_var]])
	colname = lapply(levels(factors), function(x) paste0("Среднее ", x))
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