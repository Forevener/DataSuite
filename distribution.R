ds.frequencytables = function()
{
	in_data = get_data()
	data_names = get_names()

	num_vars = ncol(in_data)

	removeUI(selector = "#dist_output")

	insertUI(selector = "#key_div_dist",
			 ui = tags$div(id = "dist_output"))

	lapply(1:num_vars, function (index)
	{
		n = paste0("table_", index)
		insertUI(selector = "#dist_output",
				 ui = tagList(tags$p(data_names[index]),
				 			 tableOutput(n))
		)

		tableA = questionr::freq(in_data[[index]], digits = 2, valid = FALSE)
		tableA$n = as.integer(tableA$n)

		output[[n]] = renderTable(tableA, rownames = TRUE)
	})
}

ds.distributionplots = function()
{
	removeUI(selector = "#dist_output")

	insertUI(selector = "#key_div_dist",
			 ui = tags$div(id = "dist_output"))

	in_data = get_data()
	data_names = get_names()

	num_vars = ncol(in_data)

	lapply(1:num_vars, function (index)
	{
		if (is.numeric(in_data[[index]]))
		{
			n = paste0("plot_dist_", index)
			insertUI(selector = "#dist_output",
					 ui = tagList(tags$p(data_names[index]),
					 			 plotOutput(n))
			)

			fact = levels(factor(in_data[[index]]))
			dmin = min(in_data[[index]], na.rm = TRUE)
			dmax = max(in_data[[index]], na.rm = TRUE)
			scale_step = min(diff(as.numeric(fact)))
			plot_breaks = seq(dmin, dmax, by = scale_step)
			if (length(plot_breaks) > 18)
			{
				new_step = scale_step * round(length(plot_breaks) / 14)
				plot_breaks = seq(dmin, dmax, by = new_step)
				bw = abs(dmax - dmin) / (length(plot_breaks) - 2)
				hist_call = geom_histogram(fill = "white", colour = "black", binwidth = bw, center = 0.5, closed = "left", na.rm = TRUE)
			}
			else
			{
				bw = abs(dmax - dmin) / (length(plot_breaks) - 1)
				hist_call = geom_histogram(fill = "white", colour = "black", binwidth = bw, na.rm = TRUE)
			}

			output[[n]] = renderCachedPlot({
				ggplot(in_data, aes(in_data[[index]])) +
					hist_call +
					stat_function(fun = function(x) dnorm(x, mean = mean(in_data[[index]], na.rm = TRUE), sd = sd(in_data[[index]], na.rm = TRUE)) * bw * length(in_data[[index]][!is.na(in_data[[index]])]),
								  color = "red", size = 1, na.rm = TRUE) +
					labs(x = "Значения", y = "Количество") +
					scale_x_continuous(breaks = plot_breaks)
			}, cacheKeyExpr = in_data[[index]])
		}
	})
}

ds.shapirowilk = function()
{
	removeUI(selector = "#dist_output")

	insertUI(selector = "#key_div_dist",
			 ui = tags$div(id = "dist_output",
			 			  tags$p("Критерий Шапиро-Уилка для проверки нормальности распределения"),
			 			  tableOutput("sw_table")))

	in_data = get_data()
	data_names = get_names()

	num_vars = ncol(in_data)
	names = list(data_names, c("W", "p", "Распределение"))
	out_data = matrix(nrow = num_vars, ncol = 3, dimnames = names)

	for (index in 1:num_vars)
	{
		if (is.numeric(in_data[[index]]))
		{
			result = shapiro.test(in_data[[index]])
			out_data[index, 1] = sprintf(round(result$statistic, 5), fmt = '%#.5f')
			out_data[index, 2] = sprintf(round(result$p.value, 6), fmt = '%#.6f')
			out_data[index, 3] = ifelse(result$p.value > 0.05, "Нормальное", "Отличается от нормального")
		}
		else
		{
			out_data[index, 3] = "Переменная не является числовой"
		}
	}

	output$sw_table <- renderTable(out_data, rownames = TRUE)
}