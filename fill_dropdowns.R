fill.dropdowns = function()
{
	in_data = get_data()
	data_names = get_names()

		if ((is.null(ncol(in_data))) || (ncol(in_data) < 1))
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

	for (index in 1:ncol(in_data))
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
	updateSelectInput(session, "vars_manova", choices = selections_mis)
	updateSelectInput(session, "cl1_dropdown", choices = selections_cor)
	updateSelectInput(session, "cl2_dropdown", choices = selections_cor)
}