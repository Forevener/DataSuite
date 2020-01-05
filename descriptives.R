ds.descriptives = function(method = "parametric")
{
	# Prepare UI
	removeUI(selector = "#desc_table")
	insertUI(selector = "#key_div_desc",
			 ui = tags$div(id = "desc_table",
			 			  tags$p("Параметрическая описательная статистика"),
			 			  tableOutput("desc_table_main")))

	# Retrieve valid data and save original names
	valid_data = check.data(get_data())
	in_data = data.matrix(valid_data$data)
	data_names = valid_data$names

	# Perform analysis
	if (method == "parametric")
	{
		means = colMeans2(in_data)
		sds = colSds(in_data)
		out_data = data.frame("Минимум" = colMins(in_data))
		out_data[["Нижняя граница нормы"]] = means - sds
		out_data[["Среднее"]] = means
		out_data[["Верхняя граница нормы"]] = means + sds
		out_data[["Максимум"]] = colMaxs(in_data)
		out_data[["Стандартное отклонение"]] = sds
	}
	else
	{
		out_data = data.frame(colQuantiles(in_data))
		colnames(out_data) = c("Минимум", "Нижний квартиль", "Медиана", "Верхний квартиль", "Максимум")
	}

	# Set pretty names
	rownames(out_data) = data_names

	# Render the resulting table
	output$desc_table_main = renderTable(out_data, rownames = TRUE)
}