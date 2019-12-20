ds.frequencytables = function()
{
	in_data = get_data()
	data_names = get_names()

	if (is.null(in_data))
	{
		showNotification("Не загружены данные для обработки!")
		return(NULL)
	}

	num_vars = ncol(in_data)

	removeUI(
		selector = "div[id^='tab3_table']",
		multiple = TRUE)

	for (index in 1:num_vars)
	{
		n = paste0("table_", index)
		insertUI(
			selector = "#tab3bottom",
			ui = tags$div(id = paste0("tab3_table", index), tags$p(data_names[index]), tableOutput(n)))
		local({
			l = index
			output[[n]] = renderTable(questionr::freq(in_data[[l]], digits = 2, valid = FALSE), rownames = TRUE)
		})
	}

	updateTabsetPanel(session, "mainTabs", selected = "Tab3")
}