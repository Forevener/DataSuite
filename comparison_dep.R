ds.cds = function(method = "t")
{
	# Strings for output and local variables determining analysis type
	method_title = switch(method,
						  "t" = "t-критерия Стьюдента",
						  "Z" = "критерия знаков",
						  "W" = "W-критерия Уилкоксона",
						  "Q" = "Q-критерия Фридмана",
						  "F" = "анализа повторяющихся наблюдений")
	parametric = ifelse(method == "t" || method == "F", TRUE, FALSE)
	multiple = ifelse(method == "Q" || method == "F", TRUE, FALSE)
	method_agg = ifelse(parametric, "Среднее ", "Медиана ")

	# Prepare UI
	removeUI(selector = "div[id^=cds_]",
			 multiple = TRUE)
	insertUI(selector = "#key_div_cds_table",
			 ui = tags$div(id = "cds_table",
			 			  tags$p(paste0("Сравнение зависимых выборок с помощью ", method_title)),
			 			  tableOutput("cds_result_table")))
	insertUI(selector = "#key_div_cds_plots",
			 ui = tags$div(id = "cds_plots"))
	if (multiple)
		insertUI(selector = "#key_div_cds_details",
				 ui = tags$div(id = "cds_details"))

	# Excluding non-numeric variables could lead to unpredictable results - so it's better to just throw an error and let user check the data manually
	if (length(check.data(get_data(), get_names())$cols) < ncol(get_data()))
		stop("Были обнаружены нечисловые переменные. Сравнение выборок отменено, рекомендуется внимательно проверить и убрать лишние столбцы из анализа.")

	# Retrieve data, set variables
	n_measures = ifelse(multiple, strtoi(measures_input()), 2)
	in_data = get_data()
	num_vars = ncol(in_data) / n_measures
	data_names = get_names()[1:num_vars]

	new_data = custom.melt(in_data, n_measures)
	measure = new_data[[1]]
	new_data = new_data[-1]
	colname = lapply(1:n_measures, function(x) paste0(method_agg, " по замеру #", x))
	table_names = list(data_names, c(colname, c(method, "p", "Различия")))
	out_data = matrix(nrow = num_vars, ncol = length(table_names[[2]]), dimnames = table_names)

	lapply(1:num_vars, function (index)
	{
		# Calculate means/medians and analysis results
		func = ifelse(parametric, "mean", "median")
		aggs <- aggregate(new_data[[index]], by = list(measure), FUN = func, na.rm = TRUE)
		result = switch(method,
						"t" = t.test(new_data[[index]] ~ measure, paired = TRUE, na.rm = TRUE),
						"Z" = DescTools::SignTest(new_data[[index]] ~ measure, na.rm = TRUE),
						"W" = wilcox.test(new_data[[index]] ~ measure, paired = TRUE, na.rm = TRUE),
						"F" = oneway.test(new_data[[index]] ~ measure),
						"Q" = friedman.test(extract(in_data, index, n_measures)))

		# Fill the resulting table
		for (y in 1:n_measures)
			out_data[index, y] <<- sprintf(round(aggs[y, 2], 2), fmt = '%#.2f')
		out_data[index, y + 1] <<- sprintf(round(result$statistic[[1]], 3), fmt = '%#.3f')
		out_data[index, y + 2] <<- strong.p(result$p.value, 0.05)
		out_data[index, y + 3] <<- ifelse(result$p.value > 0.05, "Отсутствуют", "Присутствуют")

		if (multiple)
		{
			# Calculate pairwise comparisons
			pwc = switch(method,
						 "F" = pairwise.t.test(new_data[[index]], measure, p.adjust.method = "BH")$p.value,
						 "H" = pairwise.wilcox.test(new_data[[index]], measure, p.adjust.method = "BH")$p.value)
			pwc[] = strong.p(pwc, 0.05)

			# Prepare and render detailed tables
			nt = paste0("table_", index)
			insertUI(selector = "#cds_details",
					 ui = tagList(tags$p(data_names[index]),
					 			 tableOutput(nt)))
			output[[nt]] = renderTable(pwc, rownames = TRUE, sanitize.text.function = function (x) {x})
		}

		# Generate the name for the plot and insert its UI
		n = paste0("plot_", index - 1)
		insertUI(selector = "#cds_plots",
				 ui = tagList(tags$p(data_names[index - 1]),
				 			 plotOutput(n)))

		if (parametric)
		{
			# Violin plots + mean points
			g = ggplot(new_data, aes(measure, new_data[[index]])) +
				geom_violin() +
				stat_summary(fun.y=mean, geom="point", size=2)
		}
		else
		{
			# Violin plots + quantiles
			g = ggplot(new_data, aes(measure, new_data[[index]])) +
				geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))
		}

		output[[n]] = renderCachedPlot({
			g +	labs(x = NULL, y = "Значение")
		}, cacheKeyExpr = list(measure, new_data[[index]], parametric))
	})

	output$cds_result_table <- renderTable(out_data, rownames = TRUE, sanitize.text.function = function (x) {x}, na = "")
}