ds.nonparametric_descriptives = function()
{
	in_data = get_data()
	data_names = get_names()

	if (is.null(in_data))
	{
		showNotification("Не загружены данные для обработки!", type = "warning")
		return(NULL)
	}

	num_vars = ncol(in_data)
	names = list(data_names, c("Минимум", "Нижний квартиль", "Медиана", "Верхний квартиль", "Максимум"))
	out_data = matrix(nrow = num_vars, ncol = 5, dimnames = names)

	for (index in 1:num_vars)
	{
		if (is.numeric(in_data[[index]]))
		{
			result = quantile(in_data[[index]], na.rm = TRUE)
			out_data[index, 1] = result[[1]]
			out_data[index, 2] = result[[2]]
			out_data[index, 3] = result[[3]]
			out_data[index, 4] = result[[4]]
			out_data[index, 5] = result[[5]]
		}
		else
		{
			out_data[index, 1] = "Переменная не является числовой"
			out_data[index, 2] = ""
			out_data[index, 3] = ""
			out_data[index, 4] = ""
			out_data[index, 5] = ""
		}
	}

	output$out_table <- renderTable(out_data, rownames = TRUE)
	updateTabsetPanel(session, "mainTabs", selected = "Tab2")
}