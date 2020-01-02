ds.regression <- function(optimal = FALSE)
{
	# Prepare UI
	removeUI(selector = "div[id^=regression_]",
			 multiple = TRUE)
	insertUI(selector = "#key_div_regression_tables",
			 ui = tags$div(id = "regression_tables"))
	insertUI(selector = "#key_div_regression_plots",
			 ui = tags$div(id = "regression_plots"))

	# Check if independent variables are selected
	if (length(indep_vars_reg()) < 1)
	{
		showNotification("Не выбраны независимые переменные!", type = "error")
		return(NULL)
	}

	# Retrieve the data
	in_data = get_data()
	data_names = get_names()

	# Prepare variables, factorize independent ones
	num_vars = ncol(in_data)
	ind_vars_i = strtoi(indep_vars_reg())
	# Could be useful for special categorical variables selector later
	# for (i in 1:length(ind_vars_i))
	# {
	# 	x = ind_vars_i[i]
	# 	if (is.numeric(in_data[[x]]))
	# 		in_data[[x]] = as.factor(in_data[[x]])
	# }
	ind_vars_n = colnames(in_data)[ind_vars_i]

	# Prepare the formula
	comm = ind_vars_n[1]
	for (i in 2:length(ind_vars_n))
	{
		comm = paste0(comm, " + ", ind_vars_n[i])
	}

	# Perform GLM
	series = (1:num_vars)[-ind_vars_i]
	lapply(1:length(series), function (i)
	{
		index = series[i]
		if (is.numeric(in_data[[index]]))
		{
			# Building the model
			form = as.formula(paste0(colnames(in_data)[index], " ~ ", comm))
			if (optimal)
			{
				new_data = data.frame(in_data[ind_vars_i], "y" = in_data[[index]])
				model = bestglm::bestglm(new_data, TopModels = 1)$BestModel
			}
			else
				model = glm(form, data = in_data)

			# Coefficients table
			tableA = data.frame(summary(model)$coefficients)
			tableA[[4]] = strong.p(tableA[[4]], 0.05)
			colnames(tableA) = c("Коэффициент", "Стандартная ошибка", "t", "p")

			# Quality indicators
			quality = paste0("Информационный критерий Акаике: ", round(AIC(model), 2), "\r\n",
							 "Информационный критерий Байеса: ", round(BIC(model), 2), "\r\n",
							 "Логарифмическая функция правдоподобия: ", round(logLik(model)[1], 2))

			# Adding the UI elements
			n = paste0("table_", index)
			nt = paste0("text_", index)
			np = paste0("plot_", index)
			insertUI(selector = "#regression_tables",
					 ui = tagList(tags$p(data_names[index]),
					 			 tags$p("Общая таблица"),
					 			 tableOutput(n),
					 			 tags$p("Показатели качества модели"),
					 			 verbatimTextOutput(nt)))
			insertUI(selector = "#regression_plots",
					 ui = tagList(tags$p(data_names[index]),
					 			 plotOutput(np)))

			# Render UI
			output[[n]] = renderTable(tableA, rownames = TRUE, sanitize.text.function = function (x) {x})
			output[[nt]] = renderText(quality)
			output[[np]] = renderCachedPlot({
				sjPlot::plot_model(model)
			}, cacheKeyExpr = model)
		}
	})
}