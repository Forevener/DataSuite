ds.distributionplots = function()
{
	in_data = get_data()
	data_names = get_names()

	if ((is.null(in_data)) || (ncol(in_data) < 1))
	{
		showNotification("Не загружены данные для обработки!", type = "warning")
		return(NULL)
	}

	num_vars = ncol(in_data)

	removeUI(
		selector = "div[id^='tab4_plot']",
		multiple = TRUE)

	for (index in 1:num_vars)
	{
		if (is.numeric(in_data[[index]]))
		{
			n = paste0("plot_", index)
			insertUI(
				selector = "#tab4bottom",
				ui = tags$div(id = paste0("tab4_plot", index), tags$p(data_names[index]), plotOutput(n)))
			local({
				l = index
				fact = levels(factor(in_data[[l]]))
				dmin = min(in_data[[l]], na.rm = TRUE)
				dmax = max(in_data[[l]], na.rm = TRUE)
				scale_step = min(diff(as.numeric(fact)), na.rm = TRUE)
				plot_breaks = seq(dmin, dmax, by = scale_step)
				if (length(plot_breaks) > 18)
				{
					new_step = scale_step * round(length(plot_breaks) / 14)
					plot_breaks = seq(dmin - new_step, dmax + new_step, by = new_step)
					bw = abs(dmax - dmin) / (length(plot_breaks) - 3)
				}
				else
				{
					bw = abs(dmax - dmin) / (length(plot_breaks) - 1)
				}

				output[[n]] = renderPlot({
					ggplot(in_data, aes(in_data[[l]])) +
						geom_histogram(fill = "white", colour = "black", binwidth = bw, na.rm = TRUE) +
						stat_function(fun = function(x) dnorm(x, mean = mean(in_data[[l]], na.rm = TRUE), sd = sd(in_data[[l]], na.rm = TRUE)) * bw * length(in_data[[l]][!is.na(in_data[[l]])]),
									  color = "red", size = 1, na.rm = TRUE) +
						labs(x = "Значения", y = "Количество") +
						scale_x_continuous(breaks = plot_breaks)
				})
			})
		}
	}

	updateTabsetPanel(session, "mainTabs", selected = "Tab4")
}
