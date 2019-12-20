ds.shapirowilk = function()
{
	in_data = get_data()
	data_names = get_names()

	if (is.null(in_data))
	{
		showNotification("Не загружены данные для обработки!")
		return(NULL)
	}

	num_vars = ncol(in_data)
	names = list(data_names, c("W", "p", "Распределение"))
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

	output$out_table <- renderTable(out_data, rownames = TRUE)
	updateTabsetPanel(session, "mainTabs", selected = "Tab2")
}