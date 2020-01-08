# TODO: Rework, separate hierarchical and kmeans clustering, and possibly more methods; Various export formats
ds.dendro <- function() {
  # Prepare UI
  removeUI(
    selector = "div[id^='clust_']",
    multiple = TRUE
  )
  insertUI(
    selector = "#key_div_clust_main",
    ui = tags$div(
      id = "clust_selection",
      tags$p(i18n$t("Выводы NbClust::NbClust:")),
      verbatimTextOutput("clust_text")
    )
  )
  insertUI(
    selector = "#key_div_clust_plots",
    ui = tags$div(
      id = "clust_plots",
      tags$p(i18n$t("Дендрограмма (по методу иерархической кластеризации)")),
      plotOutput("clust_plot_DG"),
      tags$p(i18n$t("Дерево кластеризации")),
      plotOutput("clust_plot_CT"),
      tags$p(i18n$t("График осыпи кластеров (по методу К-средних)")),
      plotOutput("clust_plot_SS"),
      tags$p(i18n$t("График силуэтов кластеров (по методу К-средних)")),
      plotOutput("clust_plot_SI"),
      # tags$p(i18n$t("График промежутков кластеров (по методу К-средних)")),
      # plotOutput("clust_plot_GS"),
      tags$p(i18n$t("Индексы качества кластеризации (по методу К-средних)")),
      plotOutput("clust_plot_QI")
    )
  )

  # Retrieve and prepare the data
  in_data <- check_data(get_data(), nas = TRUE)$data
  scaled <- scale(in_data)
  result <- dist(scaled)
  hc <- hclust(result)

  # Variables
  max_clusters <- floor(nrow(in_data) / 10)
  repeats <- floor(nrow(in_data) / 5)

  # Prepare the data for clustree
  clustering_data <- data.frame("K1" = kmeans(scaled, center = 1, nstart = repeats)$cluster)
  for (index in 2:max_clusters)
  {
    clustering_data[[paste0("K", index)]] <- kmeans(scaled, center = index, nstart = repeats)$cluster
  }

  # Perform cluster number selection
  nb_results <- capture.output({
    nb <- data.frame("Num" = as.factor(t(NbClust::NbClust(scaled, method = "kmeans", max.nc = max_clusters)$Best.nc)[, 1]))
  })

  # Render UI
  output[["clust_text"]] <- renderPrint(
    nb_results
  )
  output[["clust_plot_DG"]] <- renderCachedPlot(
    {
      plot(as.dendrogram(hc), ylab = i18n$t("Евклидово расстояние"), leaflab = "none")
    },
    cacheKeyExpr = hc
  )
  output[["clust_plot_CT"]] <- renderCachedPlot(
    {
      clustree::clustree(clustering_data, prefix = "K")
    },
    cacheKeyExpr = clustering_data
  )
  output[["clust_plot_SS"]] <- renderCachedPlot(
    {
      factoextra::fviz_nbclust(scaled, kmeans, method = "wss", k.max = max_clusters) +
        labs(x = i18n$t("Количество кластеров"), y = i18n$t("Внутри-кластерные суммы квадратов"), title = NULL)
    },
    cacheKeyExpr = list(scaled, kmeans, max_clusters)
  )
  output[["clust_plot_SI"]] <- renderCachedPlot(
    {
      factoextra::fviz_nbclust(scaled, kmeans, method = "silhouette", k.max = max_clusters) +
        labs(x = i18n$t("Количество кластеров"), y = i18n$t("Средняя ширина силуэта"), title = NULL)
    },
    cacheKeyExpr = list(scaled, kmeans, max_clusters)
  )
  # Too much calculations for too negligible result
  # output[["clust_plot_GS"]] = renderCachedPlot({
  # 	factoextra::fviz_nbclust(scaled, kmeans, nstart = 25,  method = "gap_stat", nboot = 50, verbose = FALSE, k.max = max_clusters) +
  # 		labs(x = i18n$t("Количество кластеров"), y = i18n$t("Меж-кластерный промежуток"), title = NULL)
  # }, cacheKeyExpr = )
  output[["clust_plot_QI"]] <- renderCachedPlot(
    {
      ggplot(nb, aes(Num)) +
        geom_bar(fill = "steelblue") +
        theme_classic() +
        labs(x = i18n$t("Количество кластеров"), y = i18n$t("Количество индексов"))
    },
    cacheKeyExpr = nb
  )
}

ds.clusteranalysis <- function() {
  # Prepare UI
  removeUI(
    selector = "div[id^=clust_]",
    multiple = TRUE
  )
  insertUI(
    selector = "#key_div_clust_main",
    ui = tags$div(
      id = "clust_tables",
      tags$p(i18n$t("Состав кластеров")),
      tableOutput("clust_table_1_S"),
      tags$p(i18n$t("Средние кластеров")),
      tableOutput("clust_table_1_M"),
      tags$p(i18n$t("Индексы качества кластеризации")),
      tableOutput("clust_table_1_Q"),
      downloadLink("dlClusteringData", i18n$t("Скачать таблицу данных с результатами кластеризации"))
    )
  )
  insertUI(
    selector = "#key_div_clust_plots",
    ui = tags$div(
      id = "clust_plots",
      tags$p(i18n$t("График кластеризации")),
      plotOutput("clust_plot_1"),
      tags$p(i18n$t("График силуэтов")),
      plotOutput("clust_plot_2")
    )
  )

  # Retrieve and prepare the valid data
  in_data <- check_data(get_data(), nas = TRUE)$data
  scaled <- scale(in_data)
  k_dist <- dist(scaled)
  clusters <- clusters_num()

  # Perform clustering
  result <- kmeans(scaled, center = clusters, nstart = floor(nrow(in_data) / 5))

  # Clustering stats
  s <- fpc::cluster.stats(k_dist, result$cluster)
  sil <- cluster::silhouette(result$cluster, k_dist)

  # Prepare data for export
  out_data <- base_data()
  out_data[[i18n$t("Кластер")]] <- NaN
  out_data[input$in_table_rows_all, ][!rowSums(is.na(out_data[strtoi(included_vars())])), ][[i18n$t("Кластер")]] <- result$cluster

  # Generate detail tables
  tableA <- i18n$t("Количество") %isnameof% data.frame(result$size)
  tableB <- data.frame(t(result$centers))
  tableC <- i18n$t("Показатель") %isnameof% data.frame(c(
    i18n$t("Меж-кластерное расстояние") %isnameof% s$average.between,
    i18n$t("Внутри-кластерное расстояние") %isnameof% s$average.within,
    i18n$t("Средняя ширина силуэта") %isnameof% s$avg.silwidth,
    i18n$t("Индекс Данна") %isnameof% s$dunn,
    i18n$t("Вторичный индекс Данна") %isnameof% s$dunn2
  ))

  # Render UI
  output[["clust_table_1_S"]] <- renderTable(tableA, rownames = TRUE)
  output[["clust_table_1_M"]] <- renderTable(tableB, rownames = TRUE, digits = 2)
  output[["clust_table_1_Q"]] <- renderTable(tableC, rownames = TRUE, digits = 4)
  output[["dlClusteringData"]] <- downloadHandler(
    filename = function() {
      paste0(i18n$t("Данные кластеризации "), format(Sys.time(), "%Y-%m-%d %H-%M-%S"), ".xlsx")
    },
    content = function(file) {
      xlsx::write.xlsx(out_data, file)
    }
  )
  output[["clust_plot_1"]] <- renderCachedPlot(
    {
      factoextra::fviz_cluster(result, in_data, repel = TRUE, main = NULL)
    },
    cacheKeyExpr = list(result, in_data)
  )
  output[["clust_plot_2"]] <- renderCachedPlot(
    {
      factoextra::fviz_silhouette(sil, print.summary = FALSE) +
        labs(title = NULL, y = i18n$t("Ширина силуэта"))
    },
    cacheKeyExpr = sil
  )
}
