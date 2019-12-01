ds.kruskalwallis = function(in_data, ind_var)
{
	num_vars = ncol(in_data)
	factors <- factor(in_data[[ind_var]])
	colname = c()

	for (index in 1:length(levels(factors)))
		colname = append(colname, paste("Медиана ", levels(factors)[index]))

	names = list(colnames(in_data[, ind_var * -1]), append(colname, c("H", "p", "Различия")))
	out_data = matrix(nrow = num_vars - 1, ncol = length(levels(factors)) + 3, dimnames = names)

	series = 1:num_vars
	series = series[-ind_var]
	for (i in 1:length(series))
	{
		index = series[i]
		if (is.numeric(in_data[[index]]))
		{
			medians <- aggregate(in_data[[index]], by = list(factors), FUN = "median", na.rm = TRUE)
			result = kruskal.test(in_data[[index]] ~ in_data[[ind_var]], data = in_data, na.rm = TRUE)
			for (y in 1:length(levels(factors)))
				out_data[i, y] = medians[y, 2]

			out_data[i, y + 1] = sprintf(round(result$statistic[[1]], 3), fmt = '%#.3f')
			out_data[i, y + 2] = sprintf(round(result$p.value, 6), fmt = '%#.6f')
			out_data[i, y + 3] = ifelse(result$p.value > 0.05, "Отсутствуют", "Присутствуют")
		}
		else
		{
			for (y in 1:length(levels(factors)))
				out_data[i, y] = "-"

			out_data[i, y + 1] = "-"
			out_data[i, y + 2] = "-"
			out_data[i, y + 3] = "Переменная не является числовой"
		}
	}

	return(out_data)
}