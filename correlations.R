ds.correlations = function(method)
{
	if (is.null(in_data))
	{
		showNotification("Не загружены данные для обработки!")
		return(NULL)
	}
	if (indep_var_ctis() == "0")
	{
		showNotification("Нет подходящих независимых переменных для данного вида анализа")
		return(NULL)
	}

	removeUI(
		selector = "div[id^='tab3_table']",
		multiple = TRUE)
	removeUI(
		selector = "div[id^='tab4_plot']",
		multiple = TRUE)

	vars1 = strtoi(corr1_var_list())
	vars2 = strtoi(corr2_var_list())

	names = list(colnames(in_data[vars1]), colnames(in_data[vars2]))
	out_data_r = matrix(nrow = length(vars1), ncol = length(vars2), dimnames = names)
	out_data_p = matrix(nrow = length(vars1), ncol = length(vars2), dimnames = names)

	for (i in 1:length(vars1))
	{
		x = vars1[i]

		for (j in 1:length(vars2))
		{
			y = vars2[j]

			# Calculating Spearman's correlations
			result = cor.test(in_data[[x]], in_data[[y]], method = method)
			if (result$p.value <= 0.05)
			{
				out_data_r[i, j] = paste0("<strong>", sprintf(round(result$estimate[1], 4), fmt = '%#.4f'), "</strong>")
				out_data_p[i, j] = paste0("<strong>", sprintf(round(result$p.value, 5), fmt = '%#.5f'), "</strong>")
			}
			else
			{
				out_data_r[i, j] = sprintf(round(result$estimate[1], 4), fmt = '%#.4f')
				out_data_p[i, j] = sprintf(round(result$p.value, 5), fmt = '%#.5f')
			}

			# Drawing scatterplots
			n = paste0("plot_", i, "-", j)
			insertUI(
				selector = "#tab4bottom",
				ui = tags$div(id = paste0("tab4_plot", i, "-", j), tags$p(paste(colnames(in_data)[x], " & ", colnames(in_data)[y])), plotOutput(n)))
			local({
				l_x = x
				l_y = y
				output[[n]] = renderPlot({
					ggplot(in_data, aes(in_data[[l_x]], in_data[[l_y]])) +
						geom_point() +
						geom_smooth() +
						labs(x = colnames(in_data[l_x]), y = colnames(in_data[l_y]))
				})
			})
		}
	}

	# Building additional p-value table
	n = "table_p.values"
	insertUI(
		selector = "#tab3bottom",
		ui = tags$div(id = "tab3_table1", tableOutput(n))
		)
	output[[n]] = renderTable(out_data_p, rownames = TRUE, sanitize.text.function = function (x) {x})

	output$out_table <- renderTable(out_data_r, rownames = TRUE, sanitize.text.function = function (x) {x})
	updateTabsetPanel(session, "mainTabs", selected = "Tab2")
}