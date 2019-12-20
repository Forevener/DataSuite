ds.repeatedanova = function()
{
	in_data = get_data()
	measures = strtoi(measures_input())

	if (is.null(in_data))
	{
		showNotification("Не загружены данные для обработки!")
		return(NULL)
	}
	if (ncol(in_data) %% measures != 0)
	{
		showNotification("Количество столбцов не делится на количество замеров - проверьте наличие нужных данных и отсутствие лишних")
		return(NULL)
	}

	removeUI(
		selector = "div[id^='tab3_table']",
		multiple = TRUE)
	removeUI(
		selector = "div[id^='tab4_plot']",
		multiple = TRUE)

	new_data = custom.melt(in_data, measures)
	num_vars = ncol(new_data) - 1
	data_names = get_names()[1:num_vars]
	factors = factor(as.character(new_data[[1]]))
	colname = lapply(1:measures, function(x) paste0("Среднее по замеру #", x))
	names = list(data_names, append(colname, c("F", "p", "Различия")))
	out_data = data.frame(matrix(nrow = num_vars, ncol = length(names[[2]]), dimnames = names))

	for (index in 2:(num_vars + 1))
	{
		i = index - 1
		if (is.numeric(new_data[[index]]))
		{
			# Calculating repeated measures ANOVA and means
			means <- aggregate(new_data[[index]], by = list(factors), FUN = "mean", na.rm = TRUE)
			result = oneway.test(new_data[[index]] ~ new_data[[1]])

			for (y in 1:measures)
				out_data[i, y] = means[y, 2]

			out_data[i, y + 1] = sprintf(round(result$statistic[[1]], 3), fmt = '%#.3f')
			out_data[i, y + 2] = result$p.value
			out_data[i, y + 3] = ifelse(result$p.value > 0.05, "Отсутствуют", "Присутствуют")

			# Building detailed tables and drawing violin plots
			n1 = paste0("table_", i)
			n2 = paste0("plot_", i)
			insertUI(
				selector = "#tab3bottom",
				ui = tags$div(id = paste0("tab3_table", i), tags$p(data_names[i]), tableOutput(n1)))
			insertUI(
				selector = "#tab4bottom",
				ui = tags$div(id = paste0("tab4_plot", i), tags$p(data_names[i]), plotOutput(n2)))
			local({
				l = index
				p_table = pairwise.t.test(new_data[[l]], new_data[[1]], p.adjust.method = "BH")$p.value
				p_table[] = unlist(strong.p(p_table, 0.05))
				output[[n1]] = renderTable(p_table, rownames = TRUE, sanitize.text.function = function (x) {x})
				output[[n2]] = renderPlot({
					ggplot(new_data, aes(as.character(new_data[[1]]), new_data[[l]])) +
						geom_violin() +
						stat_summary(fun.y=mean, geom="point", size=2) +
						labs(x = "Замер", y = "Значение")
				})
			})
		}
		else
		{
			for (y in 1:measures)
				out_data[i, y] = "-"

			out_data[i, y + 1] = "-"
			out_data[i, y + 2] = "-"
			out_data[i, y + 3] = "Переменная не является числовой"
		}
	}

	out_data[[measures + 2]] = strong.p(out_data[[measures + 2]], 0.05)

	output$out_table <- renderTable(out_data, rownames = TRUE, sanitize.text.function = function (x) {x})
	updateTabsetPanel(session, "mainTabs", selected = "Tab2")
}
