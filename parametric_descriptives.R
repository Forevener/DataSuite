ds.parametric_descriptives = function()
{
	in_data = get_data()
	data_names = get_names()

	if (is.null(in_data))
	{
		showNotification("Не загружены данные для обработки!")
		return(NULL)
	}

	num_vars = ncol(in_data)
	names = list(data_names, c("Минимум", "Нижняя граница нормы", "Среднее", "Верхняя граница нормы", "Максимум", "Стандартное отклонение"))
	out_data = matrix(nrow = num_vars, ncol = 6, dimnames = names)

	for (index in 1:num_vars)
	{
		if (is.numeric(in_data[[index]]))
		{
			rmean = round(mean(in_data[[index]], na.rm = TRUE), 2)
			rsd = round(sd(in_data[[index]], na.rm = TRUE), 2)
			out_data[index, 1] = min(in_data[[index]], na.rm = TRUE)
			out_data[index, 2] = rmean - rsd
			out_data[index, 3] = rmean
			out_data[index, 4] = rmean + rsd
			out_data[index, 5] = max(in_data[[index]], na.rm = TRUE)
			out_data[index, 6] = rsd
		}
		else
		{
			out_data[index, 1] = "Переменная не является числовой"
			out_data[index, 2] = ""
			out_data[index, 3] = ""
			out_data[index, 4] = ""
			out_data[index, 5] = ""
			out_data[index, 6] = ""
		}
	}

	output$out_table <- renderTable(out_data, rownames = TRUE)
	updateTabsetPanel(session, "mainTabs", selected = "Tab2")
}