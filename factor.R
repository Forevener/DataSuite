ds.screeplot = function()
{
	in_data = Filter(is.numeric, get_data())

	if (is.null(in_data))
	{
		showNotification("Не загружены данные для обработки!", type = "warning")
		return(NULL)
	}

	clear.ui()

	info = strsplit(capture.output({
		model = fa.parallel(in_data, plot = FALSE)
	}), "\\D+")[[1]][-1]

	plot_data_fa = fa.plot.data(model$fa.values, model$fa.sim, model$fa.simr)
	plot_data_pc = fa.plot.data(model$pc.values, model$pc.sim, model$pc.simr)

	insertUI(
		selector = "#tab4bottom",
		ui = tags$div(id = "tab4_plot1",
					  tags$p("Метод каменистой осыпи Кеттела: факторы"),
					  plotOutput("plot_1_FA"),
					  tags$p("Метод каменистой осыпи Кеттела: компоненты"),
					  plotOutput("plot_1_PC")
		)
	)

	output[["plot_1_FA"]] = renderPlot({
		scree.ggplot(plot_data_fa, "Факторы")
	})

	output[["plot_1_PC"]] = renderPlot({
		scree.ggplot(plot_data_pc, "Компоненты")
	})

	insertUI(
		selector = "#tab4bottom",
		ui = tags$div(id = "tab4_plot2",
					  tags$p("Вывод:"),
					  textOutput("text_1"),
					  textOutput("text_2")
		)
	)

	output[["text_1"]] = renderText(paste0("По критерию каменистой осыпи рекомендуется выбрать факторов: ",
										   info[1],
										   ",  компонентов: ",
										   info[2])
	)

	output[["text_2"]] = renderText(paste0("По критерию Кайзера рекомендуется выбрать факторов: ",
										   length(Filter(function(x) {x > 1.0}, model$fa.values)),
										   ",  компонентов: ",
										   length(Filter(function(x) {x > 1.0}, model$pc.values)))
	)

	updateTabsetPanel(session, "mainTabs", selected = "Tab4")
}

ds.factoranalysis = function()
{
	in_data = get_data()
	colnames(in_data) = get_names()

	if (is.null(in_data))
	{
		showNotification("Не загружены данные для обработки!", type = "warning")
		return(NULL)
	}

	in_data = Filter(is.numeric, in_data)

	clear.ui()

	#out_data = matrix(nrow = num_vars - 1, ncol = 5, dimnames = names)
	#fa(in_data, nfactors = factors_limit(), rotate = "varimax", SMC = FALSE, fm = "pa", normalize = TRUE)
	model = principal(in_data, nfactors = factors_limit(), rotate = "varimax", normalize = TRUE)
	s = fa.stats(in_data, model)
	summary_text = paste0("Корень квадратов остатков: ", round(s$rms, 4),
						  "\r\nКорень среднего квадрата ошибки аппроксимации: ", round(s$RMSEA[[1]], 4),
						  "\r\nИндекс Такера-Льюиса: ", round(s$TLI, 4)
	)

	factor_names = lapply(1:length(model$R2), function (x) {
		paste0("Фактор ", x)
	})

	tableA = data.frame(unclass(model$loadings))
	colnames(tableA) = factor_names
	tableA$Общность = model$communalities
	tableA$Уникальность = model$uniquenesses
	tableA$Сложность = model$complexity

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

	insertUI(
		selector = "#tab3bottom",
		ui = tags$div(id = "tab3_table1",
					  tags$p("Полная таблица нагрузок"),
					  tableOutput("table_1"),
					  tags$p("Сведения о факторах"),
					  tableOutput("table_2"),
					  tags$p("Выводы:"),
					  verbatimTextOutput("text_1")
		)
	)
	output[["text_1"]] = renderText(summary_text)
	output[["table_1"]] = renderTable(tableA, rownames = TRUE, digits = 3)
	output[["table_2"]] = renderTable(tableB, rownames = TRUE, digits = 4)

	insertUI(
		selector = "#tab4bottom",
		ui = tags$div(id = "tab4_plot1",
					  tags$p("График факторного анализа"),
					  plotOutput("plot_1")
		)
	)
	output[["plot_1"]] = renderPlot(fa.diagram(model))

	output$out_table <- renderTable(result, rownames = TRUE, digits = 3, na = "")
	updateTabsetPanel(session, "mainTabs", selected = "Tab2")
}