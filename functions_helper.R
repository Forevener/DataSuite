custom.melt = function(dataset, times)
{
	repeats = as.integer(times)
	if (is.integer(repeats))
	{
		rows = nrow(dataset)
		cols = ncol(dataset)
		new_cols = cols / repeats
		result = data.frame(matrix(nrow = rows * repeats, ncol = new_cols + 1, dimnames = list(NULL, c("Замер", colnames(dataset[1:new_cols])))))
		n = 1
		for (x in 1:rows)
		{
			for (y in 0:(repeats - 1))
			{
				starting = 1 + new_cols * y
				result[n, 1] = as.character(y + 1)
				result[n, 2:(new_cols + 1)] = dataset[x, starting:(starting + new_cols - 1)]
				n = n + 1
			}
		}
		return (result)
	}
	else
	{
		warning("Argument 'times' is not integer")
	}
}

extract = function(dataset, column_n, measures)
{
	result = matrix(nrow = nrow(dataset), ncol = measures)
	columns = ncol(dataset) / measures

	for (index in 0:(measures - 1))
	{
		result[, index + 1] = dataset[[column_n + index * columns]]
	}

	return(result)
}

last = function(x)
{
	return (x[length(x)])
}

strong.p = function(data, level)
{
	lapply(data, function(x) {
		if (!is.na(x) && is.numeric(x))
		{
			if (x <= level)
				return(paste0("<strong>", sprintf(round(x, 5), fmt = '%#.5f'), "</strong>"))
			else
				return(sprintf(round(x, 5), fmt = '%#.5f'))
		}
		else
		{
			return(x)
		}
	})
}

fa.plot.data = function(real, simulated, resampled)
{
	factors = length(real)
	series = 1:factors

	if (length(resampled) > 0)
	{
		plot_data = data.frame(
			"Фактор" = rep(series, 3),
			"СобственноеЗначение" = c(real, simulated, resampled),
			"Категория" = factor(c(rep("Реальные данные", factors), rep("Данные симуляции", factors), rep("Данные ресэмплинга", factors)), levels = c("Реальные данные", "Данные симуляции", "Данные ресэмплинга"), ordered = TRUE)
		)
	}
	else
	{
		plot_data = data.frame(
			"Фактор" = rep(series, 2),
			"СобственноеЗначение" = c(real, simulated),
			"Категория" = factor(c(rep("Реальные данные", factors), rep("Данные симуляции", factors)), levels = c("Реальные данные", "Данные симуляции"), ordered = TRUE)
		)
	}

	return(data.frame(plot_data))
}

scree.ggplot = function(plot_data, axis.title = "Факторы")
{
	ggplot(data = plot_data, aes(`Фактор`, `СобственноеЗначение`, colour = `Категория`, linetype = `Категория`)) +
		geom_line() +
		geom_hline(yintercept = 1) +
		geom_point(aes(alpha = `Категория`), shape = 0, size = 2) +
		scale_alpha_manual(values = c(1, 0, 0)) +
		scale_color_manual(values = c("blue", "red", "orange")) +
		labs(x = axis.title, y = "Собственные значения") +
		theme(
			legend.title = element_blank(),
			legend.justification = c(1, 1),
			legend.position = c(1, 1),
			legend.margin = margin(6, 6, 6, 6)
		)
}
