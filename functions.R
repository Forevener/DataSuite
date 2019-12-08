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
