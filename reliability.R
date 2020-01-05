# TODO: Reversed items selection, more descriptives for drop table
ds.reliability = function()
{
	# Prepare UI
	removeUI(selector = "#reliability_table")
	insertUI(selector = "#key_div_reli_table",
			 ui = tags$div(id = "reliability_table",
			 			  tags$p("Результаты анализа надёжности"),
			 			  tableOutput("table_1"),
			 			  tags$p("Таблица отбросов"),
			 			  tableOutput("table_2")))

	# Filter numeric valid data
	in_data = check.data(get_data(), nas = TRUE)$data

	# Calculate Alpha and Omega
	resultA = psych::alpha(as.data.frame(in_data))
	resultO = psych::omega(in_data, plot = FALSE)

	#Calculate composite reliability from https://www.r-bloggers.com/five-ways-to-calculate-internal-consistency/
	items = paste0(colnames(in_data), collapse = "+")
	model = paste0("F1 =~ ", items)
	fit = lavaan::cfa(model, data = in_data)
	sl = lavaan::standardizedSolution(fit)
	sl = sl$est.std[sl$op == "=~"]
	names(sl) = colnames(in_data)
	re = 1 - sl^2
	cr = sum(sl) ^ 2 / (sum(sl) ^ 2 + sum(re))

	# Prepare main table
	tableA = data.frame("Показатель" = c(
		"Альфа Кронбаха" = resultA$total$raw_alpha,
		"Лямбда-6 Гуттмана" = resultA$total$`G6(smc)`,
		"Омега МакДональда" = resultO$omega.tot,
		"Композитная надёжность" = cr
	))

	# Prepare drop table
	tableB = data.frame(resultA$alpha.drop[c(1,3)])
	colnames(tableB) = c("Альфа при отбросе", "Лямбда-6 при отбросе")

	# Render UI
	output[["table_1"]] = renderTable(tableA, rownames = TRUE, digits = 4)
	output[["table_2"]] = renderTable(tableB, rownames = TRUE, digits = 4)
}