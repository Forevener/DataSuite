ds.reliability = function()
{
	in_data = na.omit(Filter(is.numeric, get_data()))

	if ((is.null(in_data)) || (ncol(in_data) < 1))
	{
		showNotification("Не загружены данные для обработки!", type = "warning")
		return(NULL)
	}

	clear.ui()

	resultA = psych::alpha(as.data.frame(in_data))
	resultO = psych::omega(in_data, plot = FALSE)

	items = paste(colnames(in_data), collapse = "+")
	model = paste("F1", items, sep = "=~")

	fit = lavaan::cfa(model, data = in_data)

	sl = lavaan::standardizedSolution(fit)
	sl = sl$est.std[sl$op == "=~"]
	names(sl) = colnames(in_data)

	re = 1 - sl^2

	cr = sum(sl) ^ 2 / (sum(sl) ^ 2 + sum(re))

	tableA = data.frame("Показатель" = c(
		"Альфа Кронбаха" = resultA$total$raw_alpha,
		"Лямбда-6 Гуттмана" = resultA$total$`G6(smc)`,
		"Омега МакДональда" = resultO$omega.tot,
		"Композитная надёжность" = cr
	))

	tableB = data.frame(resultA$alpha.drop[c(1,3)])
	colnames(tableB) = c("Альфа при отбросе", "Лямбда-6 при отбросе")

	insertUI(
		selector = "#tab3bottom",
		ui = tags$div(id = "tab3_table1",
					  tags$p("Результаты анализа надёжности"),
					  tableOutput("table_1"),
					  tags$p("Таблица отбросов"),
					  tableOutput("table_2")
		)
	)

	output[["table_1"]] = renderTable(tableA, rownames = TRUE, digits = 4)

	output[["table_2"]] = renderTable(tableB, rownames = TRUE, digits = 4)

	updateTabsetPanel(session, "mainTabs", selected = "Tab3")
}