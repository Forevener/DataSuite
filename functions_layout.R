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

check.data = function(in_data, data_names = NULL, num = TRUE, nas = FALSE)
{
	result = ""
	valid_cols = c(1:ncol(in_data))
	valid_rows = c(1:nrow(in_data))
	if (is.null(data_names))
		data_names = get_names()

	if (num)
	{
		valid_cols = sapply(valid_cols, function (v)
		{
			if (is.numeric(in_data[[v]]))
				return(v)
			else
				return(NA)
		})
		valid_cols = valid_cols[!is.na(valid_cols)]

		if (length(valid_cols) == 0)
			stop("В таблице данных нет числовых столбцов")

		non_valid_cols = (1:ncol(in_data))[-valid_cols]

		if (length(non_valid_cols) > 0)
		{
			result = paste0("<strong>Следующие столбцы не являются числовыми и были устранены из анализа:</strong><br/>",
							paste0(data_names[non_valid_cols], collapse = "<br/>"))
			if (nas)
				result = paste0(result, "<br/><br/>")
		}
	}

	if (nas)
	{
		valid_rows = sapply(valid_rows, function (r)
		{
			if (anyNA(in_data[r, ]))
				return(NA)
			else
				return(r)
		})
		valid_rows = valid_rows[!is.na(valid_rows)]

		if (length(valid_rows) == 0)
			stop("В таблице данных нет строк без пропусков")

		non_valid_rows = (1:nrow(in_data))[-valid_rows]

		if (length(non_valid_rows) > 0)
		{
			result = paste0(result,
							"<strong>Следующие строки имели пропущенные значения и были устранены из анализа:</strong><br/>",
							paste0(rownames(in_data)[non_valid_rows], collapse = "<br/>"))
		}
	}

	if (nchar(result) > 1)
	{
		showModal(
			modalDialog(
				title = "Предупреждение",
				footer = modalButton("ОК"),
				HTML(result)
			)
		)
	}

	return(list("data" = in_data[valid_rows, valid_cols], "names" = data_names[valid_cols], "cols" = valid_cols, "rows" = valid_rows))
}
