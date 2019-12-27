ds.dendro = function()
{
	in_data = na.omit(Filter(is.numeric, get_data()))

	if (is.null(in_data))
	{
		showNotification("Не загружены данные для обработки!", type = "warning")
		return(NULL)
	}

	clear.ui()

	result = dist(in_data)
	hc = hclust(result)

	insertUI(
		selector = "#tab4bottom",
		ui = tags$div(id = "tab4_plot1",
					  tags$p("Дендрограмма (по методу иерархической кластеризации)"),
					  plotOutput("plot_1_DG"),
					  tags$p("График осыпи кластеров (по методу К-средних)"),
					  plotOutput("plot_1_SS"),
					  tags$p("График силуэтов кластеров (по методу К-средних)"),
					  plotOutput("plot_1_SI"),
					  tags$p("График промежутков кластеров (по методу К-средних)"),
					  plotOutput("plot_1_GS"),
					  tags$p("Индексы качества кластеризации (по методу К-средних)"),
					  plotOutput("plot_1_QI"),
					  tags$p("Выводы NbClust::NbClust:"),
					  verbatimTextOutput("text_1")
		)
	)

	output[["plot_1_DG"]] = renderPlot({
		plot(as.dendrogram(hc), ylab = "Евклидово расстояние", leaflab = "none")
	})

	max_clusters = floor(nrow(in_data) / 10)

	s = capture.output({
		nb = data.frame("Num" = as.factor(t(NbClust(in_data, method = "kmeans", max.nc = max_clusters)$Best.nc)[, 1]))
	})

	output[["plot_1_SS"]] = renderPlot({
		fviz_nbclust(in_data, kmeans, method = "wss", k.max = max_clusters) +
			labs(x = "Количество кластеров", y = "Внутри-кластерные суммы квадратов", title = NULL)
	})

	output[["plot_1_SI"]] = renderPlot({
		fviz_nbclust(in_data, kmeans, method = "silhouette", k.max = max_clusters) +
			labs(x = "Количество кластеров", y = "Средняя ширина силуэта", title = NULL)
	})

	output[["plot_1_GS"]] = renderPlot({
		fviz_nbclust(in_data, kmeans, nstart = 25,  method = "gap_stat", nboot = 50, verbose = FALSE, k.max = max_clusters) +
			labs(x = "Количество кластеров", y = "Меж-кластерный промежуток", title = NULL)
	})

	output[["plot_1_QI"]] = renderPlot({
		ggplot(nb, aes(Num)) +
		 	geom_bar(fill="steelblue") +
			theme_classic() +
			labs(x = "Количество кластеров", y = "Количество индексов")
	})

	output[["text_1"]] = renderPrint(s)

	updateTabsetPanel(session, "mainTabs", selected = "Tab4")
}

ds.clusteranalysis = function()
{
	in_data = na.omit(Filter(is.numeric, get_data()))

	clear.ui()
	clusters = clusters_num()

	result = kmeans(in_data, center = clusters, nstart = floor(nrow(in_data) / 5))

	out_data = base_data()
	out_data$Кластер = NaN
	out_data[input$in_table_rows_all, ][!rowSums(is.na(out_data[strtoi(included_vars())])), ]$Кластер = result$cluster

	insertUI(
		selector = "#tab3bottom",
		ui = tags$div(id = "tab3_table1",
					  tags$p("Состав кластеров"),
					  tableOutput("table_1_S"),
					  tags$p("Средние кластеров"),
					  tableOutput("table_1_M"),
					  downloadLink("dlClusteringData", "Скачать таблицу данных с результатами кластеризации")
		)
	)

	tableA = data.frame("Количество" = result$size)
	output[["table_1_S"]] = renderTable(tableA, rownames = TRUE)

	tableB = data.frame(result$centers)
	output[["table_1_M"]] = renderTable(tableB, rownames = TRUE, digits = 2)

	output[["dlClusteringData"]] = downloadHandler(
		filename = function() {
			paste0("Данные кластеризации ", Sys.Date(), ".xlsx")
		},
		content = function(file) {
			write.xlsx(out_data, file)
		}
	)

	updateTabsetPanel(session, "mainTabs", selected = "Tab3")
}