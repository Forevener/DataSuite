# `grouping_vars` and `main_data` come from analyzis function

if (length(grouping_vars) > 0) {
	combinations <- tidyr::crossing(main_data[grouping_names])
	colnames(combinations) <- colnames(main_data[grouping_names])
	series <- nrow(combinations)
} else {
	series <- 1
}