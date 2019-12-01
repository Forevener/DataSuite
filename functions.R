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
