ds.correlations = function(method)
{
	# Prepare UI
	removeUI(selector = "div[id^=corr_]",
			 multiple = TRUE)
	ending = switch(method,
					"spearman" = "критерию Спирмена",
					"kendall" = "критерию Кендалла",
					"pearson" = "критерию Пирсона")
	insertUI(selector = "#key_div_corr_tables",
			 ui = tags$div(id = "corr_tables",
			 			  tags$p(paste0("Коэффициенты корреляции по ", ending)),
			 			  tableOutput("corr_table_r"),
			 			  tags$p("p-значения для корреляций"),
			 			  tableOutput("corr_table_p")))
	insertUI(selector = "#key_div_corr_plots",
			 ui = tags$div(id = "corr_plots"))

	# Prepare and check the data
	in_data = get_data()
	data_names = get_names()

	# Get variables
	vars1 = strtoi(corr1_var_list())
	vars2 = strtoi(corr2_var_list())
	names = list(data_names[vars1], data_names[vars2])

	# Prepare the resulting tables
	out_data_r = matrix(nrow = length(vars1), ncol = length(vars2), dimnames = names)
	out_data_p = matrix(nrow = length(vars1), ncol = length(vars2), dimnames = names)

	lapply(1:length(vars1), function (i)
	{
		x = vars1[i]

		lapply(1:length(vars2), function (j)
		{
			y = vars2[j]

			# Calculating correlations
			result = cor.test(in_data[[x]], in_data[[y]], method = method)
			if (result$p.value <= 0.05)
			{
				out_data_r[i, j] <<- paste0("<strong>", sprintf(round(result$estimate[1], 4), fmt = '%#.4f'), "</strong>")
				out_data_p[i, j] <<- paste0("<strong>", sprintf(round(result$p.value, 5), fmt = '%#.5f'), "</strong>")
			}
			else
			{
				out_data_r[i, j] <<- sprintf(round(result$estimate[1], 4), fmt = '%#.4f')
				out_data_p[i, j] <<- sprintf(round(result$p.value, 5), fmt = '%#.5f')
			}

			# Drawing scatterplots
			n = paste0("plot_", i, "x", j)
			insertUI(selector = "#corr_plots",
					 ui = tagList(tags$p(paste(data_names[x], " & ", data_names[y])),
					 			 plotOutput(n)))

			output[[n]] = renderCachedPlot({
				ggplot(in_data, aes(in_data[[x]], in_data[[y]])) +
					geom_point() +
					geom_smooth() +
					labs(x = data_names[x], y = data_names[y])
			}, cacheKeyExpr = list(in_data[[x]], in_data[[y]]))
		})
	})

	# Render the results
	output$corr_table_r = renderTable(out_data_r, rownames = TRUE, sanitize.text.function = function (x) {x})
	output$corr_table_p = renderTable(out_data_p, rownames = TRUE, sanitize.text.function = function (x) {x})
}