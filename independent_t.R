ds.independent_ttest = function(in_data, ind_var)
{
	num_vars = ncol(in_data)
	factors <- factor(in_data[[ind_var]])
	names = list(colnames(in_data[, ind_var * -1]), c(paste("Среднее ", levels(factors)[1]), paste("Среднее ", levels(factors)[2]), "T", "p", "Различия"))
	out_data = matrix(nrow = num_vars - 1, ncol = 5, dimnames = names)

	series = 1:num_vars
	series = series[-ind_var]
	for (i in 1:length(series))
	{
		index = series[i]
		if (is.numeric(in_data[[index]]))
		{
			means <- aggregate(in_data[[index]], by = list(factors), FUN = "mean", na.rm = TRUE)
			result = t.test(in_data[[index]] ~ in_data[[ind_var]], data = in_data)
			out_data[i, 1] = sprintf(round(means[1, 2], 2), fmt = '%#.2f')
			out_data[i, 2] = sprintf(round(means[2, 2], 2), fmt = '%#.2f')
			out_data[i, 3] = sprintf(round(result$statistic[[1]], 4), fmt = '%#.4f')
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