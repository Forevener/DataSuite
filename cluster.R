# TODO: Rework, separate hierarchical and kmeans clustering, and possibly more methods
ds.dendro = function()
{
	# Prepare UI
	removeUI(selector = "div[id^='clust_']",
			 multiple = TRUE)
	insertUI(selector = "#key_div_clust_main",
			 ui = tags$div(id = "clust_selection",
			 			  tags$p("Выводы NbClust::NbClust:"),
			 			  verbatimTextOutput("text")))
	insertUI(selector = "#key_div_clust_plots",
			 ui = tags$div(id = "clust_plots",
			 			  tags$p("Дендрограмма (по методу иерархической кластеризации)"),
			 			  plotOutput("plot_DG"),
			 			  tags$p("Дерево кластеризации"),
			 			  plotOutput("plot_CT"),
			 			  tags$p("График осыпи кластеров (по методу К-средних)"),
			 			  plotOutput("plot_SS"),
			 			  tags$p("График силуэтов кластеров (по методу К-средних)"),
			 			  plotOutput("plot_SI"),
			 			  # tags$p("График промежутков кластеров (по методу К-средних)"),
			 			  # plotOutput("plot_GS"),
			 			  tags$p("Индексы качества кластеризации (по методу К-средних)"),
			 			  plotOutput("plot_QI")))

	# Retrieve and prepare the data
	in_data = check.data(get_data(), nas = TRUE)$data
	scaled = scale(in_data)
	result = dist(scaled)
	hc = hclust(result)

	# Variables
	max_clusters = floor(nrow(in_data) / 10)
	repeats = floor(nrow(in_data) / 5)

	# Prepare the data for clustree
	clustering_data = data.frame("K1" = kmeans(scaled, center = 1, nstart = repeats)$cluster)
	for (index in 2:max_clusters)
	{
		clustering_data[[paste0("K", index)]] = kmeans(scaled, center = index, nstart = repeats)$cluster
	}

	# Perform cluster number selection
	nb_results = capture.output({
		nb = data.frame("Num" = as.factor(t(NbClust::NbClust(scaled, method = "kmeans", max.nc = max_clusters)$Best.nc)[, 1]))
	})

	# Render UI
	output[["text"]] = renderPrint(
		nb_results
	)
	output[["plot_DG"]] = renderCachedPlot({
		plot(as.dendrogram(hc), ylab = "Евклидово расстояние", leaflab = "none")
	}, cacheKeyExpr = hc)
	output[["plot_CT"]] = renderCachedPlot({
		clustree::clustree(clustering_data, prefix = "K")
	}, cacheKeyExpr = clustering_data)
	output[["plot_SS"]] = renderCachedPlot({
		factoextra::fviz_nbclust(scaled, kmeans, method = "wss", k.max = max_clusters) +
			labs(x = "Количество кластеров", y = "Внутри-кластерные суммы квадратов", title = NULL)
	}, cacheKeyExpr = list(scaled, kmeans, max_clusters))
	output[["plot_SI"]] = renderCachedPlot({
		factoextra::fviz_nbclust(scaled, kmeans, method = "silhouette", k.max = max_clusters) +
			labs(x = "Количество кластеров", y = "Средняя ширина силуэта", title = NULL)
	}, cacheKeyExpr = list(scaled, kmeans, max_clusters))
	# Too much calculations for too negligible result
	# output[["plot_GS"]] = renderCachedPlot({
	# 	factoextra::fviz_nbclust(scaled, kmeans, nstart = 25,  method = "gap_stat", nboot = 50, verbose = FALSE, k.max = max_clusters) +
	# 		labs(x = "Количество кластеров", y = "Меж-кластерный промежуток", title = NULL)
	# }, cacheKeyExpr = )
	output[["plot_QI"]] = renderCachedPlot({
		ggplot(nb, aes(Num)) +
		 	geom_bar(fill="steelblue") +
			theme_classic() +
			labs(x = "Количество кластеров", y = "Количество индексов")
	}, cacheKeyExpr = nb)
}

ds.clusteranalysis = function()
{
	# Prepare UI
	removeUI(selector = "div[id^=clust_]",
			 multiple = TRUE)
	insertUI(selector = "#key_div_clust_main",
			 ui = tags$div(id = "clust_tables",
			 			  tags$p("Состав кластеров"),
			 			  tableOutput("table_1_S"),
			 			  tags$p("Средние кластеров"),
			 			  tableOutput("table_1_M"),
			 			  tags$p("Индексы качества кластеризации"),
			 			  tableOutput("table_1_Q"),
			 			  downloadLink("dlClusteringData", "Скачать таблицу данных с результатами кластеризации")))
	insertUI(selector = "#key_div_clust_plots",
			 ui = tags$div(id = "clust_plots",
			 			  tags$p("График силуэтов"),
			 			  plotOutput("plot_1")))

	# Retrieve and prepare the valid data
	in_data = check.data(get_data(), nas = TRUE)$data
	scaled = scale(in_data)
	k_dist = dist(scaled)
	clusters = clusters_num()

	# Perform clustering
	result = kmeans(scaled, center = clusters, nstart = floor(nrow(in_data) / 5))

	# Clustering stats
	s = fpc::cluster.stats(k_dist, result$cluster)
	sil = cluster::silhouette(result$cluster, k_dist)

	# Prepare data for export
	out_data = base_data()
	out_data[["Кластер"]] = NaN
	out_data[input$in_table_rows_all, ][!rowSums(is.na(out_data[strtoi(included_vars())])), ][["Кластер"]] = result$cluster

	# Generate detail tables
	tableA = data.frame("Количество" = result$size)
	tableB = data.frame(t(result$centers))
	tableC = data.frame("Показатель" = c(
		"Меж-кластерное расстояние" = s$average.between,
		"Внутри-кластерное расстояние" = s$average.within,
		"Средняя ширина силуэта" = s$avg.silwidth,
		"Индекс Данна" = s$dunn,
		"Вторичный индекс Данна" = s$dunn2
	))

	# Render UI
	output[["table_1_S"]] = renderTable(tableA, rownames = TRUE)
	output[["table_1_M"]] = renderTable(tableB, rownames = TRUE, digits = 2)
	output[["table_1_Q"]] = renderTable(tableC, rownames = TRUE, digits = 4)
	output[["dlClusteringData"]] = downloadHandler(
		filename = function() {
			paste0("Данные кластеризации ", Sys.Date(), ".xlsx") # TODO: Various export formats
		},
		content = function(file) {
			xlsx::write.xlsx(out_data, file)
		}
	)
	output[["plot_1"]] = renderCachedPlot({
		factoextra::fviz_cluster(result, in_data, repel = TRUE, main = NULL)
	}, cacheKeyExpr = list(result, in_data))
	output[["plot_1"]] = renderCachedPlot({
		factoextra::fviz_silhouette(sil, print.summary = FALSE) +
			labs(title = NULL, y = "Ширина силуэта")
	}, cacheKeyExpr = sil)
}