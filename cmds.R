ds.repeatedanova = function()
{
	removeUI(selector = "div[id^=cmds_]",
			 multiple = TRUE)

	insertUI(selector = "#key_div_cmds_table",
			 ui = tags$div(id = "cmds_table",
			 			  tags$p("Сравнение нескольких независимых выборок с помощью дисперсионного анализа повторяющихся наблюдений"),
			 			  tableOutput("cmds_result_table")))

	insertUI(selector = "#key_div_cmds_details",
			 ui = tags$div(id = "cmds_details"))

	insertUI(selector = "#key_div_cmds_plots",
			 ui = tags$div(id = "cmds_plots"))

	in_data = get_data()
	measures = strtoi(measures_input())

	if (ncol(in_data) %% measures != 0)
	{
		showNotification("Количество столбцов не делится на количество замеров - проверьте наличие нужных данных и отсутствие лишних", type = "error")
		return(NULL)
	}

	new_data = custom.melt(in_data, measures)
	num_vars = ncol(new_data) - 1
	data_names = get_names()[1:num_vars]
	factors = factor(as.character(new_data[[1]]))
	colname = lapply(1:measures, function(x) paste0("Среднее по замеру #", x))
	names = list(data_names, append(colname, c("F", "p", "Различия")))
	out_data = data.frame(matrix(nrow = num_vars, ncol = length(names[[2]]), dimnames = names))

	lapply(2:(num_vars + 1), function (index)
	{
		i = index - 1
		if (is.numeric(new_data[[index]]))
		{
			# Calculating repeated measures ANOVA and means
			means <- aggregate(new_data[[index]], by = list(factors), FUN = "mean", na.rm = TRUE)
			result = oneway.test(new_data[[index]] ~ new_data[[1]])

			for (y in 1:measures)
				out_data[i, y] <<- means[y, 2]

			out_data[i, y + 1] <<- sprintf(round(result$statistic[[1]], 3), fmt = '%#.3f')
			out_data[i, y + 2] <<- result$p.value
			out_data[i, y + 3] <<- ifelse(result$p.value > 0.05, "Отсутствуют", "Присутствуют")

			# Building detailed tables and drawing violin plots
			n1 = paste0("table_", i)
			n2 = paste0("plot_", i)
			insertUI(
				selector = "#cmds_details",
				ui = tagList(tags$p(data_names[i]),
							 tableOutput(n1)))
			insertUI(
				selector = "#cmds_plots",
				ui = tagList(tags$p(data_names[i]),
							 plotOutput(n2)))

			p_table = pairwise.t.test(new_data[[index]], new_data[[1]], p.adjust.method = "BH")$p.value
			p_table[] = unlist(strong.p(p_table, 0.05))
			output[[n1]] = renderTable(p_table, rownames = TRUE, sanitize.text.function = function (x) {x})
			output[[n2]] = renderCachedPlot({
				ggplot(new_data, aes(as.character(new_data[[1]]), new_data[[index]])) +
					geom_violin() +
					stat_summary(fun.y=mean, geom="point", size=2) +
					labs(x = "Замер", y = "Значение")
			}, cacheKeyExpr = list(new_data[[1]], new_data[[index]]))
		}
		else
		{
			out_data[i, measures + 3] = "Переменная не является числовой"
		}
	})

	out_data[[measures + 2]] = strong.p(out_data[[measures + 2]], 0.05)

	output$cmds_result_table <- renderTable(out_data, rownames = TRUE, sanitize.text.function = function (x) {x}, na = "")
}

ds.friedman = function()
{
	removeUI(selector = "div[id^=cmds_]",
			 multiple = TRUE)

	insertUI(selector = "#key_div_cmds_table",
			 ui = tags$div(id = "cmds_table",
			 			  tags$p("Сравнение нескольких независимых выборок с помощью дисперсионного анализа Фридмана"),
			 			  tableOutput("cmds_result_table")))

	insertUI(selector = "#key_div_cmds_details",
			 ui = tags$div(id = "cmds_details"))

	insertUI(selector = "#key_div_cmds_plots",
			 ui = tags$div(id = "cmds_plots"))

	in_data = get_data()
	measures = strtoi(measures_input())

	if (ncol(in_data) %% measures != 0)
	{
		showNotification("Количество столбцов не делится на количество замеров - проверьте наличие нужных данных и отсутствие лишних", type = "error")
		return(NULL)
	}

	new_data = custom.melt(in_data, measures)
	num_vars = ncol(new_data) - 1
	data_names = get_names()[1:num_vars]
	factors = factor(as.character(new_data[[1]]))
	colname = lapply(1:measures, function(x) paste0("Медиана по замеру #", x))
	names = list(data_names, append(colname, c("F", "p", "Различия")))
	out_data = data.frame(matrix(nrow = num_vars, ncol = length(names[[2]]), dimnames = names))

	lapply(2:(num_vars + 1), function (index)
	{
		i = index - 1
		if (is.numeric(new_data[[index]]))
		{
			# Calculating Friedman's ANOVA and medians
			medians <- aggregate(new_data[[index]], by = list(factors), FUN = "median", na.rm = TRUE)
			result = friedman.test(extract(in_data, i, measures))

			for (y in 1:measures)
				out_data[i, y] <<- medians[y, 2]

			out_data[i, y + 1] <<- sprintf(round(result$statistic[[1]], 3), fmt = '%#.3f')
			out_data[i, y + 2] <<- result$p.value
			out_data[i, y + 3] <<- ifelse(result$p.value > 0.05, "Отсутствуют", "Присутствуют")

			# Building detailed tables and drawing violin plots
			n1 = paste0("table_", i)
			n2 = paste0("plot_", i)
			insertUI(selector = "#cmds_details",
				ui = tagList(tags$p(data_names[i]),
							  tableOutput(n1)))
			insertUI(selector = "#cmds_plots",
				ui = tagList(tags$p(data_names[i]),
							  plotOutput(n2)))

			p_table = pairwise.wilcox.test(new_data[[index]], new_data[[1]], p.adjust.method = "BH")$p.value
			p_table[] = unlist(strong.p(p_table, 0.05))
			output[[n1]] = renderTable(p_table, rownames = TRUE, sanitize.text.function = function (x) {x})
			output[[n2]] = renderCachedPlot({
				ggplot(new_data, aes(as.character(new_data[[1]]), new_data[[index]])) +
					geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
					labs(x = "Замер", y = "Значение")
			}, cacheKeyExpr = list(new_data[[1]], new_data[[index]]))
		}
		else
		{
			out_data[i, measures + 3] <<- "Переменная не является числовой"
		}
	})

	out_data[[measures + 2]] = strong.p(out_data[[measures + 2]], 0.05)

	output$cmds_result_table <- renderTable(out_data, rownames = TRUE, sanitize.text.function = function (x) {x}, na = "")
}