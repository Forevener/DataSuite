fill.dropdowns = function()
{
	in_data = get_data()
	data_names = get_names()
	num_vars = ncol(in_data)

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

	updateSelectInput(session, "si_var_ctis", choices = selections_tis, selected = selections_tis[1])
	updateSelectInput(session, "si_var_cmis", choices = selections_mis, selected = selections_mis[1])
	updatePickerInput(session, "si_vars_manova", choices = selections_mis)
	updatePickerInput(session, "si_vars_regression", choices = selections_mis)
	updatePickerInput(session, "si_var1_corr", choices = selections_cor)
	updatePickerInput(session, "si_var2_corr", choices = selections_cor)
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

disable.all = function(elements)
{
	lapply(elements, function (x) { disable(x) })
}

enable.all = function(elements)
{
	lapply(elements, function (x) { enable(x) })
}

ds.execute = function(func, hideSelector = NULL, showSelector = NULL)
{
	if (!is.null(hideSelector))
		hide(selector = hideSelector)
	if (!is.null(showSelector))
		show(selector = showSelector)
	inputs = names(input)
	disable.all(inputs)
	result = try(func)

	if (any(class(result) == "try-error"))
	{
		showModal(
			modalDialog(
				title = "Ошибка",
				footer = modalButton("ОК"),
				result
			)
		)
	}

	enable.all(inputs)
}