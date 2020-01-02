ds.manova <- function()
{
	# Prepare UI
	removeUI(selector = "div[id^=manova_]",
			 multiple = TRUE)
	insertUI(selector = "#key_div_manova_tables",
			 ui = tags$div(id = "manova_tables"))
	insertUI(selector = "#key_div_manova_plots",
			 ui = tags$div(id = "manova_plots"))

	# MANOVA should really be used only for nvar > 2
	if (length(indep_vars_css()) < 2)
	{
		showNotification("Не выбрано достаточно независимых переменных!", type = "error")
		return(NULL)
	}

	# Retrieve the data
	in_data = get_data()
	data_names = get_names()

	# Prepare variables, factorize independent ones
	num_vars = ncol(in_data)
	ind_vars_i = strtoi(indep_vars_css())
	for (i in 1:length(ind_vars_i))
	{
		x = ind_vars_i[i]
		if (is.numeric(in_data[[x]]))
			in_data[[x]] = as.factor(in_data[[x]])
	}
	ind_vars_n = colnames(in_data)[ind_vars_i]

	# Prepare the formula
	comm = ind_vars_n[1]
	for (i in 2:length(ind_vars_n))
	{
		comm = paste0(comm, " * ", ind_vars_n[i])
	}

	# Perform MANOVA
	series = (1:num_vars)[-ind_vars_i]
	lapply(1:length(series), function (i)
	{
		index = series[i]
		if (is.numeric(in_data[[index]]))
		{
			# Building the model
			form = as.formula(paste0(colnames(in_data)[index], " ~ ", comm))
			model = aov(form, data = in_data)

			# Effects table
			tableA = summary(model)[[1]]
			tableA[[5]] = strong.p(tableA[[5]], 0.05)
			colnames(tableA) = c("Степени свободы", "Сумма квадратов", "Среднее квадратов", "F", "p")
			rownames(tableA) = sub("Residuals", "Остаток", rownames(tableA), fixed = TRUE)

			# Pairwise comparisons table
			tableB = as.data.frame(last(TukeyHSD(model))[[1]])
			tableB[[4]] = strong.p(tableB[[4]], 0.05)
			colnames(tableB) = c("Разница средних", "Нижняя граница интервала", "Верхняя граница интервала", "Корректированный p")

			# Adding the UI elements
			n1 = paste0("table_", index, "A")
			n2 = paste0("table_", index, "B")
			np = paste0("plot_", index)
			insertUI(selector = "#manova_tables",
					 ui = tagList(tags$p(data_names[index]),
					 			 tags$p("Общая таблица"),
					 			 tableOutput(n1),
					 			 tags$p("Попарные сравнения"),
					 			 tableOutput(n2)))
			insertUI(selector = "#manova_plots",
					 ui = tagList(tags$p(data_names[index]),
					 			 plotOutput(np)))

			# Render UI
			output[[n1]] = renderTable(tableA, rownames = TRUE, sanitize.text.function = function (x) {x})
			output[[n2]] = renderTable(tableB, rownames = TRUE, sanitize.text.function = function (x) {x})
			output[[np]] = renderCachedPlot({
				plot.design(form, data = in_data, xlab = "Факторы", ylab = data_names[index])
			}, cacheKeyExpr = list(in_data, ind_vars_i))
		}
	})
}