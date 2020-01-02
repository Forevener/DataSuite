ds.screeplot = function()
{
	removeUI(
		selector = "div[id^='fa_results']",
		multiple = TRUE)

	insertUI(
		selector = "#key_div_fa_table",
		ui = tags$div(id = "fa_results_a",
					  tags$p("Метод каменистой осыпи Кеттела и параллельный анализ"),
					  plotOutput("fa_plot"),
					  tags$p("Вывод:"),
					  textOutput("fa_text_1"),
					  textOutput("fa_text_2"),
					  tags$p("Выводы psycho::n_factors:"),
					  textOutput("fa_text_3")
		)
	)

	in_data = Filter(is.numeric, get_data())

	info = gsub("[^0-9]", "", capture.output({
		if (factoring_method() == "pc")
		{
			model = fa.parallel(in_data, plot = FALSE, fa = "pc")
			plot_data = fa.plot.data(model$pc.values, model$pc.sim, model$pc.simr)
			axis_label = "Компоненты"
			summary_end = "компонентов: "
			recommendation = length(Filter(function(x) {x > 1.0}, model$pc.values))
		}
		else
		{
			model = fa.parallel(in_data, plot = FALSE, fm = factoring_method(), fa = "fa")
			plot_data = fa.plot.data(model$fa.values, model$fa.sim, model$fa.simr)
			axis_label = "Факторы"
			summary_end = "факторов: "
			recommendation = length(Filter(function(x) {x > 1.0}, model$fa.values))
		}
	}))

	output[["fa_plot"]] = renderCachedPlot({
		scree.ggplot(plot_data, axis_label)
	}, cacheKeyExpr = plot_data)

	output[["fa_text_1"]] = renderText(paste0("По критерию параллельного анализа рекомендуется выбрать ", summary_end, info))

	output[["fa_text_2"]] = renderText(paste0("По критерию Кайзера рекомендуется выбрать ", summary_end, recommendation))

	# Block reactivity for now
	rot = factor_rotation()
	met = factoring_method()
	output[["fa_text_3"]] = renderPrint(psycho::n_factors(in_data, rotate = rot, fm = met))
}

ds.factoranalysis = function()
{
	removeUI(
		selector = "div[id^='fa_results']",
		multiple = TRUE)

	insertUI(
		selector = "#key_div_fa_table",
		ui = tags$div(id = "fa_results_a",
					  tableOutput("fa_table_main")
		)
	)

	insertUI(
		selector = "#key_div_fa_details",
		ui = tags$div(id = "fa_results_b",
					  tags$p("Полная таблица нагрузок"),
					  tableOutput("fa_table_1"),
					  tags$p("Сведения о факторах"),
					  tableOutput("fa_table_2"),
					  tags$p("Выводы:"),
					  verbatimTextOutput("fa_text_1")
		)
	)

	insertUI(
		selector = "#key_div_fa_plots",
		ui = tags$div(id = "fa_results_c",
					  tags$p("График факторного анализа"),
					  plotOutput("fa_plot_1"),
					  tags$p("График нагрузок"),
					  plotOutput("fa_plot_2")
		)
	)

	in_data = get_data()
	colnames(in_data) = get_names()

	in_data = Filter(is.numeric, in_data)

	if (factoring_method() == "pc")
		model = principal(in_data, nfactors = factors_limit(), rotate = factor_rotation(), normalize = factor_normalize())
	else
		model = fa(in_data, nfactors = factors_limit(), rotate = factor_rotation(), SMC = FALSE, fm = factoring_method(), normalize = factor_normalize())

	s = fa.stats(in_data, model)
	summary_text = paste0("Корень квадратов остатков: ", ifelse(is.null(s$rms), 0, round(s$rms, 4)),
						  "\r\nКорень среднего квадрата ошибки аппроксимации: ", ifelse(is.null(s$RMSEA[[1]]), 0, round(s$RMSEA[[1]], 4)),
						  "\r\nИндекс Такера-Льюиса: ", ifelse(is.null(s$TLI), 0, round(s$TLI, 4))
	)

	factor_names = lapply(1:length(model$R2), function (x) {
		paste0("Фактор ", x)
	})

	tableA = data.frame(unclass(model$loadings))
	colnames(tableA) = factor_names
	tableA$`Общность` = model$communalities
	tableA$`Уникальность` = model$uniquenesses
	tableA$`Сложность` = model$complexity

	tableB = data.frame(rbind("Собственные значения" = model$values[1:length(model$R2)], model$Vaccounted))
	colnames(tableB) = factor_names

	if (length(model$R2) > 1)
	{
		tableB = tableB[!(row.names(tableB) %in% c("Cumulative Var", "Cumulative Proportion")), ]
		x = rownames(tableB)
		x = replace(x, x == "SS loadings", "Объясняемая дисперсия")
		x = replace(x, x == "Proportion Var", "Доля общей дисперсии")
		x = replace(x, x == "Proportion Explained", "Доля объясняемой дисперсии")
		rownames(tableB) = x
	}
	else
	{
		rownames(tableB) = c("Собственное значение", "Объясняемая дисперсия", "Доля общей дисперсии")
	}

	result = data.frame(unclass(fa.sort(model$loadings)))
	colnames(result) = factor_names
	result[abs(result) < 0.3] = NaN

	output[["fa_table_main"]] = renderTable(result, rownames = TRUE, digits = 3, na = "")
	output[["fa_text_1"]] = renderText(summary_text)
	output[["fa_table_1"]] = renderTable(tableA, rownames = TRUE, digits = 3)
	output[["fa_table_2"]] = renderTable(tableB, rownames = TRUE, digits = 4)
	output[["fa_plot_1"]] = renderCachedPlot({
		fa.diagram(model, main = NULL)
	}, cacheKeyExpr = model)

	plot_data = custom.melt(result, length(model$R2))
	colnames(plot_data) = c("Фактор", "Нагрузка")
	plot_data[is.na(plot_data)] = 0
	plot_data$`Переменная` = factor(unlist(lapply(rownames(result), function (x) {rep(x, length(model$R2))})), levels = rownames(result), ordered = TRUE)

	output[["fa_plot_2"]] = renderCachedPlot({
		ggplot(data = plot_data, aes(`Переменная`, `Нагрузка`, color = `Фактор`, group = `Фактор`)) +
			geom_line() +
			ylim(-1, 1) +
			geom_hline(yintercept = 0) +
			coord_polar()
	}, cacheKeyExpr = plot_data)
}