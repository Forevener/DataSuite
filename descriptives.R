ds.parametric_descriptives = function()
{
	removeUI(selector = "#desc_table")

	insertUI(selector = "#key_div_desc",
			 ui = tags$div(id = "desc_table",
			 			  tags$p("Параметрическая описательная статистика"),
			 			  tableOutput("param_desc_table")))

	in_data = get_data()
	data_names = get_names()

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
		# else
		# {
		# 	out_data[index, 1] = "Переменная не является числовой"
		# 	out_data[index, 2] = ""
		# 	out_data[index, 3] = ""
		# 	out_data[index, 4] = ""
		# 	out_data[index, 5] = ""
		# 	out_data[index, 6] = ""
		# }
	}



	output$param_desc_table <- renderTable(out_data, rownames = TRUE)
}

ds.nonparametric_descriptives = function()
{
	removeUI(selector = "#desc_table")

	insertUI(selector = "#key_div_desc",
			 ui = tags$div(id = "desc_table",
			 			  tags$p("Непараметрическая описательная статистика"),
			 			  tableOutput("nonparam_desc_table")))

	in_data = get_data()
	data_names = get_names()

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
		# else
		# {
		# 	out_data[index, 1] = "Переменная не является числовой"
		# 	out_data[index, 2] = ""
		# 	out_data[index, 3] = ""
		# 	out_data[index, 4] = ""
		# 	out_data[index, 5] = ""
		# }
	}

	output$nonparam_desc_table <- renderTable(out_data, rownames = TRUE)
}