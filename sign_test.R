library(DescTools)

ds.signtest = function()
{
	in_data = get_data()

	if (is.null(in_data))
	{
		showNotification("Не загружены данные для обработки!", type = "warning")
		return(NULL)
	}
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
			result = SignTest(in_data[[index]], in_data[[index+num_vars]], na.rm = TRUE)
			out_data[index, 1] = median(in_data[[index]], na.rm = TRUE)
			out_data[index, 2] = median(in_data[[index+num_vars]], na.rm = TRUE)
			out_data[index, 3] = sprintf(round(result$statistic[[1]], 4), fmt = '%#.4f')
			out_data[index, 4] = sprintf(round(result$p.value, 6), fmt = '%#.6f')
			out_data[index, 5] = ifelse(result$p.value > 0.05, "Отсутствуют", "Присутствуют")
		}
		else
		{
			out_data[index, 1] = "-"
			out_data[index, 2] = "-"
			out_data[index, 3] = "-"
			out_data[index, 4] = "-"
			out_data[index, 5] = "Переменная не является числовой"
		}
	}

	new_data = custom.melt(in_data, 2)

	removeUI(
		selector = "div[id^='tab4_plot']",
		multiple = TRUE)

	for (index in 2:(num_vars + 1))
	{
		n = paste0("plot_", index - 1)
		insertUI(
			selector = "#tab4bottom",
			ui = tags$div(id = paste0("tab4_plot", index - 1), tags$p(data_names[index - 1]), plotOutput(n)))
		local({
			l = index
			output[[n]] = renderPlot({
				ggplot(new_data, aes(new_data[[1]], new_data[[l]])) +
					geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
					labs(x = NULL, y = "Значение")
			})
		})
	}

	output$out_table <- renderTable(out_data, rownames = TRUE)
	updateTabsetPanel(session, "mainTabs", selected = "Tab2")
}