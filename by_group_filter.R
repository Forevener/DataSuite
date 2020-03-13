# `grouping_vars` and `main_data` come from analyzis function
# `index` comes from loop through all combinations
# `combinations` comes from by_group_combinations

if (length(grouping_vars) > 0) {
  condition <- apply(
    sapply(1:ncol(combinations), function(col_n) {
      main_data[[names(combinations)[col_n]]] == combinations[[index, col_n]]
    }),
    1,
    all
  )
  columns <- !(colnames(main_data) %in% grouping_names)
  in_data <- main_data[condition, columns]
} else {
  in_data <- main_data
}
