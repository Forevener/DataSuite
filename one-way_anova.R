ds.onewayanova <- function()
{
	in_data = get_data()
	data_names = get_names()

	if ((is.null(in_data)) || (ncol(in_data) < 1))
	{
		showNotification("Не загружены данные для обработки!", type = "warning")
		return(NULL)
	}
	if (indep_var_cmis() == "0")
	{
		showNotification("Не выбрана независимая переменная!", type = "error")
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
	colname = lapply(levels(factors), function(x) paste0("Среднее ", x))
	names = list(data_names[-ind_var], append(colname, c("F", "p", "Различия")))
	out_data = matrix(nrow = num_vars - 1, ncol = length(levels(factors)) + 3, dimnames = names)

	series = 1:num_vars
	series = series[-ind_var]
	for (i in 1:length(series))
	{
		index = series[i]
		if (is.numeric(in_data[[index]]))
		{
			# Calculating one-way ANOVA results and means
			means <- aggregate(in_data[[index]], by = list(factors), FUN = "mean", na.rm = TRUE)
			result = oneway.test(in_data[[index]] ~ in_data[[ind_var]], data = in_data)
			for (y in 1:length(levels(factors)))
				out_data[i, y] = sprintf(round(means[y, 2], 2), fmt = '%#.2f')

			out_data[i, y + 1] = sprintf(round(result$statistic, 4), fmt = '%#.4f')
			out_data[i, y + 2] = sprintf(round(result$p.value, 6), fmt = '%#.6f')
			out_data[i, y + 3] = ifelse(result$p.value > 0.05, "Отсутствуют", "Присутствуют")

			# Building detailed tables
			n = paste0("table_", index)
			insertUI(
				selector = "#tab3bottom",
				ui = tags$div(id = paste0("tab3_table", index), tags$p(data_names[index]), tableOutput(n)))
			local({
				l = index
				pwc = pairwise.t.test(in_data[[l]], in_data[[ind_var]], p.adjust.method = "BH")$p.value
				pwc[] = unlist(strong.p(pwc, 0.05))
				output[[n]] = renderTable(pwc, rownames = TRUE, sanitize.text.function = function (x) {x})
			})

			# Drawing violin plots
			n = paste0("plot_", index)
			insertUI(
				selector = "#tab4bottom",
				ui = tags$div(id = paste0("tab4_plot", index), tags$p(data_names[index]), plotOutput(n)))
			local({
				l = index
				output[[n]] = renderPlot({
					ggplot(in_data, aes(as.character(in_data[[ind_var]]), in_data[[l]])) +
						geom_violin() +
						stat_summary(fun.y=mean, geom="point", size=2) +
						labs(x = data_names[ind_var], y = "Значение")
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