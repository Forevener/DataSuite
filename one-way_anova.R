ds.onewayanova <- function(in_data, ind_var)
{
	num_vars = ncol(in_data)
	factors <- factor(in_data[[ind_var]])
	colname = c()

	for (index in 1:length(levels(factors)))
		colname = append(colname, paste("Среднее ", levels(factors)[index]))

	names = list(colnames(in_data[, ind_var * -1]), append(colname, c("F", "p", "Различия")))
	out_data = matrix(nrow = num_vars - 1, ncol = length(levels(factors)) + 3, dimnames = names)

	series = 1:num_vars
	series = series[-ind_var]
	for (i in 1:length(series))
	{
		index = series[i]
		if (is.numeric(in_data[[index]]))
		{
			means <- aggregate(in_data[[index]], by = list(factors), FUN = "mean", na.rm = TRUE)
			result = summary(aov(in_data[[index]] ~ in_data[[ind_var]], data = in_data))
			for (y in 1:length(levels(factors)))
				out_data[i, y] = sprintf(round(means[y, 2], 2), fmt = '%#.2f')

			out_data[i, y + 1] = sprintf(round(result[[1]][1, 4], 4), fmt = '%#.4f')
			out_data[i, y + 2] = sprintf(round(result[[1]][1, 5], 6), fmt = '%#.6f')
			out_data[i, y + 3] = ifelse(result[[1]][1, 5] > 0.05, "Отсутствуют", "Присутствуют")
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