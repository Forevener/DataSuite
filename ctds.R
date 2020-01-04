ds.ctds = function(method = "t")
{
	# Strings for output
	method_title = switch(method,
						  "t" = "t-критерия Стьюдента",
						  "Z" = "критерия знаков",
						  "W" = "W-критерия Уилкоксона")
	parametric = ifelse(method == "t", TRUE, FALSE)
	method_agg = ifelse(parametric, "Среднее ", "Медиана ")

	# Prepare UI
	removeUI(selector = "div[id^=ctds_]",
			 multiple = TRUE)
	insertUI(selector = "#key_div_ctds_table",
			 ui = tags$div(id = "ctds_table",
			 			  tags$p(paste0("Сравнение двух зависимых выборок с помощью ", method_title)),
			 			  tableOutput("ctds_result_table")))
	insertUI(selector = "#key_div_ctds_plots",
			 ui = tags$div(id = "ctds_plots"))

	# Excluding non-numeric variables could lead to unpredictable results - so it's better to just throw an error and let user check the data manually
	if (length(check.data(get_data(), get_names())$cols) > 0)
		stop("После устранения нечисловых переменных их число стало нечётным. Рекомендуется внимательно проверить данные.")

	# Retrieve data, set variables
	in_data = get_data()
	num_vars = ncol(in_data) / 2
	data_names = get_names()[1:num_vars]
	table_names = list(data_names, c(paste(method_agg, "до"), paste(method_agg, "после"), method, "p", "Различия"))
	out_data = matrix(nrow = num_vars, ncol = 5, dimnames = table_names)

	# Perform analysis
	for (index in 1:num_vars)
	{
		# Calculate means/medians and analysis results
		func = ifelse(parametric, "mean", "median")
		result = switch(method,
						"t" = t.test(in_data[[index]], in_data[[index+num_vars]], paired = TRUE, na.rm = TRUE),
						"Z" = DescTools::SignTest(in_data[[index]], in_data[[index+num_vars]], na.rm = TRUE),
						"W" = wilcox.test(in_data[[index]], in_data[[index+num_vars]], paired = TRUE, na.rm = TRUE))

		# Fill the resulting table
		out_data[index, 1] = sprintf(round(do.call(func, list(in_data[[index]], na.rm = TRUE)), 2), fmt = '%#.2f')
		out_data[index, 2] = sprintf(round(do.call(func, list(in_data[[index+num_vars]], na.rm = TRUE)), 2), fmt = '%#.2f')
		out_data[index, 3] = sprintf(round(result$statistic[[1]], 4), fmt = '%#.4f')
		out_data[index, 4] = sprintf(round(result$p.value, 6), fmt = '%#.6f')
		out_data[index, 5] = ifelse(result$p.value > 0.05, "Отсутствуют", "Присутствуют")
	}
	# Remove the indexing variable from the environment to prevent confusion
	rm(index)

	# Prepare data for plotting
	new_data = custom.melt(in_data, 2)

	# Prepare and render the plots
	lapply(2:(num_vars + 1), function (index)
	{
		# Generate the name for the plot
		n = paste0("plot_", index - 1)

		insertUI(selector = "#ctds_plots",
				 ui = tagList(tags$p(data_names[index - 1]),
				 			 plotOutput(n)))

		if (parametric)
		{
			# Violin plots + mean points
			g = ggplot(new_data, aes(new_data[[1]], new_data[[index]])) +
				geom_violin() +
				stat_summary(fun.y=mean, geom="point", size=2)
		}
		else
		{
			# Violin plots + quantiles
			g = ggplot(new_data, aes(new_data[[1]], new_data[[index]])) +
				geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))
		}

		output[[n]] = renderCachedPlot({
			g +	labs(x = NULL, y = "Значение")
		}, cacheKeyExpr = list(new_data[[1]], new_data[[index]], parametric))
	})

	# Render the resulting table
	output$ctds_result_table <- renderTable(out_data, rownames = TRUE, na = "")
}