ds.mannwhitney = function(in_data, ind_var)
{
	num_vars = ncol(in_data)
	factors <- factor(in_data[[ind_var]])
	names = list(colnames(in_data[, ind_var * -1]), c(paste("Медиана ", levels(factors)[1]), paste("Медиана ", levels(factors)[2]), "U", "p", "Различия"))
	out_data = matrix(nrow = num_vars - 1, ncol = 5, dimnames = names)

	series = 1:num_vars
	series = series[-ind_var]
	for (i in 1:length(series))
	{
		index = series[i]
		if (is.numeric(in_data[[index]]))
		{
			medians <- aggregate(in_data[[index]], by = list(factors), FUN = "median", na.rm = TRUE)
			result = wilcox.test(in_data[[index]] ~ in_data[[ind_var]], data = in_data, correct = FALSE, na.rm = TRUE)
			out_data[i, 1] = medians[1, 2]
			out_data[i, 2] = medians[2, 2]
			out_data[i, 3] = sprintf(round(result$statistic[[1]], 3), fmt = '%#.3f')
			out_data[i, 4] = sprintf(round(result$p.value, 6), fmt = '%#.6f')
			out_data[i, 5] = ifelse(result$p.value > 0.05, "Отсутствуют", "Присутствуют")
		}
		else
		{
			out_data[i, 1] = "-"
			out_data[i, 2] = "-"
			out_data[i, 3] = "-"
			out_data[i, 4] = "-"
			out_data[i, 5] = "Переменная не является числовой"
		}
	}

	return(out_data)
}