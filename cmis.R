ds.onewayanova <- function()
{
	removeUI(selector = "div[id^=cmis_]",
			 multiple = TRUE)

	insertUI(selector = "#key_div_cmis_table",
			 ui = tags$div(id = "cmis_table",
			 			  tags$p("Сравнение нескольких независимых выборок с помощью дисперсионного анализа по критерию Уэлча"),
			 			  tableOutput("cmis_result_table")))

	insertUI(selector = "#key_div_cmis_details",
			 ui = tags$div(id = "cmis_details"))

	insertUI(selector = "#key_div_cmis_plots",
			 ui = tags$div(id = "cmis_plots"))

	in_data = get_data()
	data_names = get_names()

	if (indep_var_cmis() == "0")
	{
		showNotification("Не выбрана независимая переменная!", type = "error")
		return(NULL)
	}

	num_vars = ncol(in_data)
	ind_var = strtoi(indep_var_cmis())
	factors <- factor(in_data[[ind_var]])
	colname = lapply(levels(factors), function(x) paste0("Среднее ", x))
	names = list(data_names[-ind_var], append(colname, c("F", "p", "Различия")))
	out_data = matrix(nrow = num_vars - 1, ncol = length(levels(factors)) + 3, dimnames = names)

	series = 1:num_vars
	series = series[-ind_var]
	lapply(1:length(series), function (i)
	{
		index = series[i]
		if (is.numeric(in_data[[index]]))
		{
			# Calculating one-way ANOVA results and means
			means <- aggregate(in_data[[index]], by = list(factors), FUN = "mean", na.rm = TRUE)
			result = oneway.test(in_data[[index]] ~ in_data[[ind_var]], data = in_data)
			for (y in 1:length(levels(factors)))
				out_data[i, y] <<- sprintf(round(means[y, 2], 2), fmt = '%#.2f')

			out_data[i, y + 1] <<- sprintf(round(result$statistic, 4), fmt = '%#.4f')
			out_data[i, y + 2] <<- sprintf(round(result$p.value, 6), fmt = '%#.6f')
			out_data[i, y + 3] <<- ifelse(result$p.value > 0.05, "Отсутствуют", "Присутствуют")

			# Building detailed tables
			nt = paste0("table_", i)
			insertUI(selector = "#cmis_details",
				ui = tagList(tags$p(data_names[index]),
							  tableOutput(nt)))

			pwc = pairwise.t.test(in_data[[index]], in_data[[ind_var]], p.adjust.method = "BH")$p.value
			pwc[] = unlist(strong.p(pwc, 0.05))
			output[[nt]] = renderTable(pwc, rownames = TRUE, sanitize.text.function = function (x) {x})

			# Drawing violin plots
			np = paste0("plot_", i)
			insertUI(selector = "#cmis_plots",
				ui = tagList(tags$p(data_names[index]),
							 plotOutput(np)))

			output[[np]] = renderCachedPlot({
				ggplot(in_data, aes(as.character(in_data[[ind_var]]), in_data[[index]])) +
					geom_violin() +
					stat_summary(fun.y=mean, geom="point", size=2) +
					labs(x = data_names[ind_var], y = "Значение")
			}, cacheKeyExpr = list(in_data[[ind_var]], in_data[[index]]))
		}
		else
		{
			out_data[i, length(levels(factors)) + 3] <<- "Переменная не является числовой"
		}
	})

	output$cmis_result_table <- renderTable(out_data, rownames = TRUE, na = "")
}

ds.kruskalwallis = function()
{
	removeUI(selector = "div[id^=cmis_]",
			 multiple = TRUE)

	insertUI(selector = "#key_div_cmis_table",
			 ui = tags$div(id = "cmis_table",
			 			  tags$p("Сравнение нескольких независимых выборок с помощью H-критерия Краскела-Уоллиса"),
			 			  tableOutput("cmis_result_table")))

	insertUI(selector = "#key_div_cmis_details",
			 ui = tags$div(id = "cmis_details"))

	insertUI(selector = "#key_div_cmis_plots",
			 ui = tags$div(id = "cmis_plots"))

	in_data = get_data()
	data_names = get_names()

	if (indep_var_cmis() == "0")
	{
		showNotification("Не выбрана независимая переменная!", type = "error")
		return(NULL)
	}

	num_vars = ncol(in_data)
	ind_var = strtoi(indep_var_cmis())
	factors <- factor(in_data[[ind_var]])
	colname = lapply(levels(factors), function(x) paste0("Медиана ", x))
	names = list(data_names[-ind_var], append(colname, c("H", "p", "Различия")))
	out_data = matrix(nrow = num_vars - 1, ncol = length(levels(factors)) + 3, dimnames = names)

	series = 1:num_vars
	series = series[-ind_var]
	lapply(1:length(series), function (i)
	{
		index = series[i]
		if (is.numeric(in_data[[index]]))
		{
			# Calculating K-W results and medians
			medians <- aggregate(in_data[[index]], by = list(factors), FUN = "median", na.rm = TRUE)
			result = kruskal.test(in_data[[index]] ~ in_data[[ind_var]], data = in_data)
			for (y in 1:length(levels(factors)))
				out_data[i, y] <<- medians[y, 2]

			out_data[i, y + 1] <<- sprintf(round(result$statistic[[1]], 3), fmt = '%#.3f')
			out_data[i, y + 2] <<- sprintf(round(result$p.value, 6), fmt = '%#.6f')
			out_data[i, y + 3] <<- ifelse(result$p.value > 0.05, "Отсутствуют", "Присутствуют")

			# Building detailed tables
			nt = paste0("table_", i)
			insertUI(selector = "#cmis_details",
				ui = tagList(tags$p(data_names[index]),
							  tableOutput(nt)))

			pwc = pairwise.wilcox.test(in_data[[index]], in_data[[ind_var]], p.adjust.method = "BH")$p.value
			pwc[] = unlist(strong.p(pwc, 0.05))
			output[[nt]] = renderTable(pwc, rownames = TRUE, sanitize.text.function = function (x) {x})

			# Drawing violin plots
			np = paste0("plot_", i)
			insertUI(selector = "#cmis_plots",
				ui = tagList(tags$p(data_names[index]),
							  plotOutput(np)))

			output[[np]] = renderCachedPlot({
				ggplot(in_data, aes(as.character(in_data[[ind_var]]), in_data[[index]])) +
					geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
					labs(x = data_names[ind_var], y = "Значение")
			}, cacheKeyExpr = list(in_data[[ind_var]], in_data[[index]]))
		}
		else
		{
			out_data[i, length(levels(factors)) + 3] <<- "Переменная не является числовой"
		}
	})

	output$cmis_result_table <- renderTable(out_data, rownames = TRUE, na = "")
}