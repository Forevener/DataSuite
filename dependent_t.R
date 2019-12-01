ds.dependent_ttest = function(in_data)
{
	num_vars = ncol(in_data) / 2
	names = list(colnames(in_data)[1:num_vars], c("Медиана до", "Медиана после", "H", "p", "Различия"))
	out_data = matrix(nrow = num_vars, ncol = 5, dimnames = names)

	for (index in 1:num_vars)
	{
		if (is.numeric(in_data[[index]]))
		{
			result = t.test(in_data[[index]], in_data[[index+num_vars]], paired = TRUE, na.rm = TRUE)
			out_data[index, 1] = sprintf(round(mean(in_data[[index]], na.rm = TRUE), 2), fmt = '%#.2f')
			out_data[index, 2] = sprintf(round(mean(in_data[[index+num_vars]], na.rm = TRUE), 2), fmt = '%#.2f')
			out_data[index, 3] = sprintf(round(result$statistic[[1]], 4), fmt = '%#.4f')
			out_data[index, 4] = sprintf(round(result$p.value, 6), fmt = '%#.6f')
			out_data[index, 5] = ifelse(result$p.value > 0.05, "Отсутствуют", "Присутствуют")
		}
		else
		{
			out_data[index, 1] = "-"
			out_data[index, 2] = "-"
			out_data[index, 3] = "-"
			out_data[index, 4] = "-"
			out_data[index, 5] = "Переменная не является числовой"
		}
	}

	return(out_data)
}