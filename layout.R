fill.dropdowns = function()
{
	in_data = get_data()
	data_names = get_names()
	num_vars = ncol(in_data)

	if ((is.null(num_vars)) || (num_vars < 1))
	{
		showModal(
			modalDialog(
				title = "Ошибка",
				"Исключено слишком много переменных, анализ невозможен"
			)
		)
		return(NULL)
	}

	selections_tis = list()
	selections_mis = list()
	selections_cor = list()

	for (index in 1:num_vars)
	{
		names(index) = data_names[index]
		if (length(levels(factor(in_data[[index]]))) == 2)
			selections_tis = append(selections_tis, index)
		selections_mis = append(selections_mis, index)
		if (is.numeric(in_data[[index]]))
			selections_cor = append(selections_cor, index)
	}

	updateSelectInput(session, "ctis_dropdown", choices = selections_tis, selected = selections_tis[1])
	updateSelectInput(session, "cmis_dropdown", choices = selections_mis, selected = selections_mis[1])
	updatePickerInput(session, "vars_manova", choices = selections_mis)
	updatePickerInput(session, "cl1_dropdown", choices = selections_cor)
	updatePickerInput(session, "cl2_dropdown", choices = selections_cor)
	updateNumericInput(session, "factors_number", max = ncol(Filter(is.numeric, get_data())))
	updateNumericInput(session, "clusters_number", max = nrow(na.omit(get_data())))
}

clear.ui = function()
{
	removeUI(
		selector = "div[id^='tab3_table']",
		multiple = TRUE)
	removeUI(
		selector = "div[id^='tab4_plot']",
		multiple = TRUE)
	output$out_table <- renderTable(NULL)
}