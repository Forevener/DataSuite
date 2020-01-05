ds.cis = function(method = "t")
{
	# Strings for output
	method_title = switch(method,
						  "t" = "t-критерия Стьюдента",
						  "U" = "U-критерия Манна-Уитни",
						  "D" = "D-критерия Колмогорова-Смирнова",
						  "Z" = "Z-критерия Уалда-Вольфовица")
	parametric = ifelse(method == "t", TRUE, FALSE)
	multiple = ifelse(method == "H" || method == "F", TRUE, FALSE)
	method_agg = ifelse(parametric, "Среднее ", "Медиана ")

	# Prepare UI
	removeUI(selector = "div[id^=cis_]",
			 multiple = TRUE)
	insertUI(selector = "#key_div_cis_table",
			 ui = tags$div(id = "cis_table",
			 			  tags$p(paste0("Сравнение двух независимых выборок с помощью ", method_title)),
			 			  tableOutput("cis_result_table")))
	insertUI(selector = "#key_div_cis_plots",
			 ui = tags$div(id = "cis_plots"))
	if (multiple)
		insertUI(selector = "#key_div_cis_details",
				 ui = tags$div(id = "cis_details"))

	# Retrieve data, extract independent and dependent variables
	ind_var_i = strtoi(ifelse(multiple, indep_var_cmis(), indep_var_ctis()))
	ind_var = get_data()[[ind_var_i]]
	if (!is.factor(ind_var))
		ind_var = factor(ind_var)
	valid_data = check.data(get_data()[-ind_var_i], get_names()[-ind_var_i])
	in_data = valid_data$data
	data_names = valid_data$names

	# Variables
	num_vars = ncol(in_data)
	column_names = lapply(levels(ind_var), function(x) paste0(method_agg, x))
	names = list(data_names, c(column_names, method, "p", "Различия"))
	out_data = matrix(nrow = num_vars, ncol = length(levels(ind_var)) + 3, dimnames = names)

	# Perform analysis
	lapply(1:num_vars, function (index)
	{
		# Calculate means/medians and analysis results
		aggs = aggregate(in_data[[index]], by = list(ind_var), FUN = ifelse(parametric, "mean", "median"), na.rm = TRUE)
		result = switch(method,
						"t" = t.test(in_data[[index]] ~ ind_var, data = in_data, na.rm = TRUE),
						"U" = wilcox.test(in_data[[index]] ~ ind_var, data = in_data, correct = FALSE, na.rm = TRUE, exact = FALSE),
						"D" = ks.test(in_data[ind_var == levels(ind_var)[1], ][[index]], in_data[ind_var == levels(ind_var)[2], ][[index]], exact = FALSE),
						"Z" = DescTools::RunsTest(in_data[[index]] ~ ind_var, data = in_data, na.rm = TRUE),
						"F" = oneway.test(in_data[[index]] ~ ind_var, data = in_data),
						"H" = kruskal.test(in_data[[index]] ~ ind_var, data = in_data))

		# Fill the resulting table
		for (y in 1:length(levels(ind_var)))
			out_data[index, y] <<- sprintf(round(aggs[y, 2], 2), fmt = '%#.2f')
		out_data[index, y + 1] <<- sprintf(round(result$statistic[[1]], 4), fmt = '%#.4f')
		out_data[index, y + 2] <<- sprintf(round(result$p.value, 6), fmt = '%#.6f')
		out_data[index, y + 3] <<- ifelse(result$p.value > 0.05, "Отсутствуют", "Присутствуют")

		if (multiple)
		{
			# Calculate pairwise comparisons
			pwc = switch(method,
						 "F" = pairwise.t.test(in_data[[index]], ind_var, p.adjust.method = "BH")$p.value,
						 "H" = pairwise.wilcox.test(in_data[[index]], ind_var, p.adjust.method = "BH")$p.value)
			pwc[] = unlist(strong.p(pwc, 0.05))

			# Prepare and render detailed tables
			nt = paste0("table_", index)
			insertUI(selector = "#cis_details",
					 ui = tagList(tags$p(data_names[index]),
					 			 tableOutput(nt)))
			output[[nt]] = renderTable(pwc, rownames = TRUE, sanitize.text.function = function (x) {x})
		}

		# Generate and render the plots
		n = paste0("plot_", index)
		insertUI(selector = "#cis_plots",
				 ui = tagList(tags$p(data_names[index]),
				 			 plotOutput(n)))
		if (parametric)
		{
			# Violin plots + mean points
			g = ggplot(in_data, aes(ind_var, in_data[[index]])) +
				geom_violin() +
				stat_summary(fun.y=mean, geom="point", size=2)
		}
		else
		{
			# Violin plots + quantiles
			g = ggplot(in_data, aes(ind_var, in_data[[index]])) +
				geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))
		}
		output[[n]] = renderCachedPlot({
			g + labs(x = get_names()[ind_var_i], y = "Значение")
		}, cacheKeyExpr = list(ind_var, in_data[[index]], method))
	})

	# Render UI
	output$cis_result_table <- renderTable(out_data, rownames = TRUE, na = "")
}