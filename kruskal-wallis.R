ds.kruskalwallis = function()
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

	removeUI(
		selector = "div[id^='tab3_table']",
		multiple = TRUE)
	removeUI(
		selector = "div[id^='tab4_plot']",
		multiple = TRUE)

	num_vars = ncol(in_data)
	ind_var = strtoi(indep_var_cmis())
	factors <- factor(in_data[[ind_var]])
	colname = lapply(levels(factors), function(x) paste0("Медиана ", x))
	names = list(colnames(in_data[, ind_var * -1]), append(colname, c("H", "p", "Различия")))
	out_data = matrix(nrow = num_vars - 1, ncol = length(levels(factors)) + 3, dimnames = names)

	series = 1:num_vars
	series = series[-ind_var]
	for (i in 1:length(series))
	{
		index = series[i]
		if (is.numeric(in_data[[index]]))
		{
			# Calculating K-W results and medians
			medians <- aggregate(in_data[[index]], by = list(factors), FUN = "median", na.rm = TRUE)
			result = kruskal.test(in_data[[index]] ~ in_data[[ind_var]], data = in_data)
			for (y in 1:length(levels(factors)))
				out_data[i, y] = medians[y, 2]

			out_data[i, y + 1] = sprintf(round(result$statistic[[1]], 3), fmt = '%#.3f')
			out_data[i, y + 2] = sprintf(round(result$p.value, 6), fmt = '%#.6f')
			out_data[i, y + 3] = ifelse(result$p.value > 0.05, "Отсутствуют", "Присутствуют")

			# Building detailed tables
			n = paste0("table_", index)
			insertUI(
				selector = "#tab3bottom",
				ui = tags$div(id = paste0("tab3_table", index), tags$p(colnames(in_data)[index]), tableOutput(n)))
			local({
				l = index
				output[[n]] = renderTable(pairwise.wilcox.test(in_data[[l]], in_data[[ind_var]], p.adjust.method = "BH")$p.value, rownames = TRUE)
			})

			# Drawing violin plots
			n = paste0("plot_", i)
			insertUI(
				selector = "#tab4bottom",
				ui = tags$div(id = paste0("tab4_plot", i), tags$p(colnames(in_data)[index]), plotOutput(n)))
			local({
				l = index
				output[[n]] = renderPlot({
					ggplot(in_data, aes(as.character(in_data[[ind_var]]), in_data[[l]])) +
						geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
						labs(x = colnames(in_data[ind_var]), y = "Значение")
				})
			})
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

	output$out_table <- renderTable(out_data, rownames = TRUE)
	updateTabsetPanel(session, "mainTabs", selected = "Tab2")
}