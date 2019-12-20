ds.manova <- function()
{
	in_data = get_data()
	data_names = get_names()

	if (is.null(in_data))
	{
		showNotification("Не загружены данные для обработки!")
		return(NULL)
	}
	if (length(indep_vars_css()) < 1)
	{
		showNotification("Не выбрана независимая переменная!")
		return(NULL)
	}

	removeUI(
		selector = "div[id^='tab3_table']",
		multiple = TRUE)
	removeUI(
		selector = "div[id^='tab4_plot']",
		multiple = TRUE)

	num_vars = ncol(in_data)
	ind_vars_i = strtoi(indep_vars_css())
	for (i in 1:length(ind_vars_i))
	{
		x = ind_vars_i[i]
		if (is.numeric(in_data[[x]]))
			in_data[[x]] = as.factor(in_data[[x]])
	}

	ind_vars_n = colnames(in_data)[ind_vars_i]

	comm = paste0(ind_vars_n[1])
	for (i in 2:length(ind_vars_n))
	{
		comm = paste0(comm, " * ", ind_vars_n[i])
	}

	series = (1:num_vars)[-ind_vars_i]
	for (i in 1:length(series))
	{
		index = series[i]
		if (is.numeric(in_data[[index]]))
		{
			# Building detailed tables
			form = as.formula(paste0(colnames(in_data)[index], " ~ ", comm))
			model = aov(form, data = in_data)
			n1 = paste0("table_", index, "A")
			n2 = paste0("table_", index, "B")
			np = paste0("plot_", index)
			insertUI(
				selector = "#tab3bottom",
				ui = tags$div(id = paste0("tab3_table", index, "A"),
							  tags$p(data_names[index]),
							  tags$p("Общая таблица"),
							  tableOutput(n1),
							  tags$p("Попарные сравнения"),
							  tableOutput(n2)))
			insertUI(
				selector = "#tab4bottom",
				ui = tags$div(id = paste0("tab4_plot", index), tags$p(data_names[index]), plotOutput(np)))
			local({
				l = index
				lform = form
				tableA = summary(model)[[1]]
				tableA[[5]] = strong.p(tableA[[5]], 0.05)
				tableB = as.data.frame(last(TukeyHSD(model))[[1]])
				tableB[[4]] = strong.p(tableB[[4]], 0.05)
				colnames(tableA) = c("Степени свободы", "Сумма квадратов", "Среднее квадратов", "F", "p")
				rownames(tableA) = sub("Residuals", "Остаток", rownames(tableA), fixed = TRUE)
				colnames(tableB) = c("Разница средних", "Нижняя граница интервала", "Верхняя граница интервала", "Корректированный p")
				output[[n1]] = renderTable(tableA, rownames = TRUE, sanitize.text.function = function (x) {x})
				output[[n2]] = renderTable(tableB, rownames = TRUE, sanitize.text.function = function (x) {x})

				# Drawing design plots
				output[[np]] = renderPlot({
					plot.design(lform, data = in_data, xlab = "Факторы", ylab = data_names[l])
				})
			})
		}
	}

	updateTabsetPanel(session, "mainTabs", selected = "Tab3")
}