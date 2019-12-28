ds.dendro = function()
{
	in_data = na.omit(Filter(is.numeric, get_data()))

	if ((is.null(in_data)) || (ncol(in_data) < 1))
	{
		showNotification("Не загружены данные для обработки!", type = "warning")
		return(NULL)
	}

	clear.ui()

	scaled = scale(in_data)
	result = dist(scaled)
	hc = hclust(result)

	insertUI(
		selector = "#tab4bottom",
		ui = tags$div(id = "tab4_plot1",
					  tags$p("Дендрограмма (по методу иерархической кластеризации)"),
					  plotOutput("plot_1_DG"),
					  tags$p("Дерево кластеризации"),
					  plotOutput("plot_1_CT"),
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
	repeats = floor(nrow(in_data) / 5)

	clustering_data = data.frame("K1" = kmeans(scaled, center = 1, nstart = repeats)$cluster)
	for (index in 2:max_clusters)
	{
		clustering_data[[paste0("K", index)]] = kmeans(scaled, center = index, nstart = repeats)$cluster
	}

	s = capture.output({
		nb = data.frame("Num" = as.factor(t(NbClust::NbClust(scaled, method = "kmeans", max.nc = max_clusters)$Best.nc)[, 1]))
	})

	output[["plot_1_CT"]] = renderPlot({
		clustree::clustree(clustering_data, prefix = "K")
	})

	output[["plot_1_SS"]] = renderPlot({
		factoextra::fviz_nbclust(scaled, kmeans, method = "wss", k.max = max_clusters) +
			labs(x = "Количество кластеров", y = "Внутри-кластерные суммы квадратов", title = NULL)
	})

	output[["plot_1_SI"]] = renderPlot({
		factoextra::fviz_nbclust(scaled, kmeans, method = "silhouette", k.max = max_clusters) +
			labs(x = "Количество кластеров", y = "Средняя ширина силуэта", title = NULL)
	})

	output[["plot_1_GS"]] = renderPlot({
		factoextra::fviz_nbclust(scaled, kmeans, nstart = 25,  method = "gap_stat", nboot = 50, verbose = FALSE, k.max = max_clusters) +
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

	if ((is.null(in_data)) || (ncol(in_data) < 1))
	{
		showNotification("Не загружены данные для обработки!", type = "warning")
		return(NULL)
	}

	scaled = scale(in_data)
	k_dist = dist(scaled)

	clear.ui()
	clusters = clusters_num()

	result = kmeans(scaled, center = clusters, nstart = floor(nrow(in_data) / 5))

	s = fpc::cluster.stats(k_dist, result$cluster)
	sil = cluster::silhouette(result$cluster, k_dist)

	out_data = base_data()
	out_data$`Кластер` = NaN
	out_data[input$in_table_rows_all, ][!rowSums(is.na(out_data[strtoi(included_vars())])), ]$`Кластер` = result$cluster

	insertUI(
		selector = "#tab3bottom",
		ui = tags$div(id = "tab3_table1",
					  tags$p("Состав кластеров"),
					  tableOutput("table_1_S"),
					  tags$p("Средние кластеров"),
					  tableOutput("table_1_M"),
					  tags$p("Индексы качества кластеризации"),
					  tableOutput("table_1_Q"),
					  downloadLink("dlClusteringData", "Скачать таблицу данных с результатами кластеризации")
		)
	)

	insertUI(
		selector = "#tab4bottom",
		ui = tags$div(id = "tab4_plot1",
					  tags$p("График кластеризации"),
					  plotOutput("plot_1")
		)
	)

	tableA = data.frame("Количество" = result$size)
	output[["table_1_S"]] = renderTable(tableA, rownames = TRUE)

	tableB = data.frame(result$centers)
	output[["table_1_M"]] = renderTable(tableB, rownames = TRUE, digits = 2)

	tableC = data.frame("Показатель" = c(
		"Меж-кластерное расстояние" = s$average.between,
		"Внутри-кластерное расстояние" = s$average.within,
		"Средняя ширина силуэта" = s$avg.silwidth,
		"Индекс Данна" = s$dunn,
		"Вторичный индекс Данна" = s$dunn2
	))
	output[["table_1_Q"]] = renderTable(tableC, rownames = TRUE, digits = 4)

	output[["dlClusteringData"]] = downloadHandler(
		filename = function() {
			paste0("Данные кластеризации ", Sys.Date(), ".xlsx")
		},
		content = function(file) {
			xlsx::writwrite.xlsx(out_data, file)
		}
	)

	output[["plot_1"]] = renderPlot({
		factoextra::fviz_cluster(result, in_data, repel = TRUE, main = NULL)
	})

	output[["plot_1"]] = renderPlot({
		factoextra::fviz_silhouette(sil, print.summary = FALSE) +
			labs(title = NULL, y = "Ширина силуэта")
	})

	updateTabsetPanel(session, "mainTabs", selected = "Tab3")
}