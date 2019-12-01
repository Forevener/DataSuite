library(shiny)
library(rhandsontable)
library(xlsx)
library(rcompanion)
library(questionr)
library(ggplot2)

shinyServer(function(input, output, session) {
	enc = reactive({input$radio})
	indep_var_ctis = reactive({input$ctis_dropdown})
	indep_var_cmis = reactive({input$cmis_dropdown})
	in_data = NULL

	observeEvent(input$upload, {

		in_file <- input$upload

		if (is.null(in_file))
			return(NULL)

		in_data <<- read.xlsx(file = in_file$datapath, sheetIndex = 1, header = TRUE, encoding = enc())

		selections_tis = list()
		selections_mis = list()
		for (index in 1:ncol(in_data))
		{
			names(index) = colnames(in_data[index])
			if (length(levels(factor(in_data[[index]]))) == 2)
				selections_tis = append(selections_tis, index)
			selections_mis = append(selections_mis, index)
		}
		if (length(selections_tis) >= 1)
			updateSelectInput(session, "ctis_dropdown", choices = selections_tis, selected = selections_tis[1])
		if (length(selections_mis) >= 1)
			updateSelectInput(session, "cmis_dropdown", choices = selections_mis, selected = selections_mis[1])

		output$in_table <- renderRHandsontable({rhandsontable(in_data)})
		showNotification("Файл успешно загружен!")
	})

	observeEvent(input$dp, {
		if (is.null(in_data))
		{
			showNotification("Не загружены данные для обработки!")
			return(NULL)
		}

		output$out_table <- renderTable(ds.parametric_descriptives(in_data), rownames = TRUE)
		updateTabsetPanel(session, "mainTabs", selected = "Tab2")
	})

	observeEvent(input$dnp, {
		if (is.null(in_data))
		{
			showNotification("Не загружены данные для обработки!")
			return(NULL)
		}

		output$out_table <- renderTable(ds.nonparametric_descriptives(in_data), rownames = TRUE)
		updateTabsetPanel(session, "mainTabs", selected = "Tab2")
	})

	observeEvent(input$ft, {
		if (is.null(in_data))
		{
			showNotification("Не загружены данные для обработки!")
			return(NULL)
		}

		num_vars = ncol(in_data)

		removeUI(
			selector = "div[id^='tab3_table']",
			multiple = TRUE)

		for (index in 1:num_vars)
		{
			n = paste0("table_", index)
			insertUI(
				selector = "#tab3bottom",
				ui = tags$div(id = paste0("tab3_table", index), tags$p(colnames(in_data)[index]), tableOutput(n)))
			local({
				l = index
				output[[n]] = renderTable(questionr::freq(in_data[[l]], digits = 2, valid = FALSE), rownames = TRUE)
			})
		}

		updateTabsetPanel(session, "mainTabs", selected = "Tab3")
	})

	observeEvent(input$distplots, {
		if (is.null(in_data))
		{
			showNotification("Не загружены данные для обработки!")
			return(NULL)
		}

		num_vars = ncol(in_data)

		removeUI(
			selector = "div[id^='tab4_plot']",
			multiple = TRUE)

		for (index in 1:num_vars)
		{
			if (is.numeric(in_data[[index]]))
			{
				n = paste0("plot_", index)
				insertUI(
					selector = "#tab4bottom",
					ui = tags$div(id = paste0("tab4_plot", index), tags$p(colnames(in_data)[index]), plotOutput(n)))
				local({
					l = index
					fact = levels(factor(in_data[[l]]))
					dmin = min(in_data[[l]], na.rm = TRUE)
					dmax = max(in_data[[l]], na.rm = TRUE)
					scale_step = min(diff(as.numeric(fact)), na.rm = TRUE)
					plot_breaks = seq(dmin, dmax, by = scale_step)
					if (length(plot_breaks) > 18)
					{
						new_step = scale_step * round(length(plot_breaks) / 14)
						plot_breaks = seq(dmin - new_step, dmax + new_step, by = new_step)
						bw = abs(dmax - dmin) / (length(plot_breaks) - 3)
					}
					else
					{
						bw = abs(dmax - dmin) / (length(plot_breaks) - 1)
					}

					output[[n]] = renderPlot({
						ggplot(in_data, aes(in_data[[l]])) +
							geom_histogram(fill = "white", colour = "black", binwidth = bw, na.rm = TRUE) +
							stat_function(fun = function(x) dnorm(x, mean = mean(in_data[[l]], na.rm = TRUE), sd = sd(in_data[[l]], na.rm = TRUE)) * bw * length(in_data[[l]][!is.na(in_data[[l]])]),
								color = "red", size = 1, na.rm = TRUE) +
							labs(x = "Значения", y = "Количество") +
							scale_x_continuous(breaks = plot_breaks)
					})
				})
			}
		}
		updateTabsetPanel(session, "mainTabs", selected = "Tab4")
	})

	observeEvent(input$sw, {
		if (is.null(in_data))
		{
			showNotification("Не загружены данные для обработки!")
			return(NULL)
		}

		output$out_table <- renderTable(ds.shapirowilk(in_data), rownames = TRUE)
		updateTabsetPanel(session, "mainTabs", selected = "Tab2")
	})

	observeEvent(input$mw, {
		if (is.null(in_data))
		{
			showNotification("Не загружены данные для обработки!")
			return(NULL)
		}
		if (indep_var_ctis() == "0")
		{
			showNotification("Нет подходящих независимых переменных для данного вида анализа")
			return(NULL)
		}

		num_vars = ncol(in_data)
		ind_var = strtoi(indep_var_ctis())
		removeUI(
			selector = "div[id^='tab4_plot']",
			multiple = TRUE)

		for (index in 1:num_vars)
		{
			if (is.numeric(in_data[[index]]))
			{
				n = paste0("plot_", index)
				insertUI(
					selector = "#tab4bottom",
					ui = tags$div(id = paste0("tab4_plot", index), tags$p(colnames(in_data)[index]), plotOutput(n)))
				local({
					l = index
					output[[n]] = renderPlot({
						ggplot(in_data, aes(in_data[[ind_var]], in_data[[l]])) +
							geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
							labs(x = colnames(in_data[ind_var]), y = "Значение")
					})
				})
			}
		}

		output$out_table <- renderTable(ds.mannwhitney(in_data, ind_var), rownames = TRUE)
		updateTabsetPanel(session, "mainTabs", selected = "Tab2")
	})

	observeEvent(input$ti, {
		if (is.null(in_data))
		{
			showNotification("Не загружены данные для обработки!")
			return(NULL)
		}
		if (indep_var_ctis() == "0")
		{
			showNotification("Нет подходящих независимых переменных для данного вида анализа")
			return(NULL)
		}

		num_vars = ncol(in_data)
		ind_var = strtoi(indep_var_ctis())
		removeUI(
			selector = "div[id^='tab4_plot']",
			multiple = TRUE)

		for (index in 1:num_vars)
		{
			if (is.numeric(in_data[[index]]))
			{
				n = paste0("plot_", index)
				insertUI(
					selector = "#tab4bottom",
					ui = tags$div(id = paste0("tab4_plot", index), tags$p(colnames(in_data)[index]), plotOutput(n)))
				local({
					l = index
					output[[n]] = renderPlot({
						ggplot(in_data, aes(in_data[[ind_var]], in_data[[l]])) +
							geom_violin() +
							stat_summary(fun.y=mean, geom="point", size=2) +
							labs(x = colnames(in_data[ind_var]), y = "Значение")
					})
				})
			}
		}

		output$out_table <- renderTable(ds.independent_ttest(in_data, strtoi(indep_var_ctis())), rownames = TRUE)
		updateTabsetPanel(session, "mainTabs", selected = "Tab2")
	})

	observeEvent(input$kw, {
		if (is.null(in_data))
		{
			showNotification("Не загружены данные для обработки!")
			return(NULL)
		}
		if (indep_var_cmis() == "0")
		{
			showNotification("Не выбрана независимая переменная!")
			return(NULL)
		}

		num_vars = ncol(in_data)
		ind_var = strtoi(indep_var_cmis())
		removeUI(
			selector = "div[id^='tab4_plot']",
			multiple = TRUE)

		for (index in 1:num_vars)
		{
			if (is.numeric(in_data[[index]]))
			{
				n = paste0("plot_", index)
				insertUI(
					selector = "#tab4bottom",
					ui = tags$div(id = paste0("tab4_plot", index), tags$p(colnames(in_data)[index]), plotOutput(n)))
				local({
					l = index
					output[[n]] = renderPlot({
						ggplot(in_data, aes(in_data[[ind_var]], in_data[[l]])) +
							geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
							labs(x = colnames(in_data[ind_var]), y = "Значение")
					})
				})
			}
		}

		output$out_table <- renderTable(ds.kruskalwallis(in_data, strtoi(indep_var_cmis())), rownames = TRUE)
		updateTabsetPanel(session, "mainTabs", selected = "Tab2")
	})

	observeEvent(input$ff, {
		if (is.null(in_data))
		{
			showNotification("Не загружены данные для обработки!")
			return(NULL)
		}
		if (indep_var_cmis() == "0")
		{
			showNotification("Не выбрана независимая переменная!")
			return(NULL)
		}

		num_vars = ncol(in_data)
		ind_var = strtoi(indep_var_cmis())
		removeUI(
			selector = "div[id^='tab4_plot']",
			multiple = TRUE)

		for (index in 1:num_vars)
		{
			if (is.numeric(in_data[[index]]))
			{
				n = paste0("plot_", index)
				insertUI(
					selector = "#tab4bottom",
					ui = tags$div(id = paste0("tab4_plot", index), tags$p(colnames(in_data)[index]), plotOutput(n)))
				local({
					l = index
					output[[n]] = renderPlot({
						ggplot(in_data, aes(in_data[[ind_var]], in_data[[l]])) +
							geom_violin() +
							stat_summary(fun.y=mean, geom="point", size=2) +
							labs(x = colnames(in_data[ind_var]), y = "Значение")
					})
				})
			}
		}

		output$out_table <- renderTable(ds.onewayanova(in_data, strtoi(indep_var_cmis())), rownames = TRUE)
		updateTabsetPanel(session, "mainTabs", selected = "Tab2")
	})


	observeEvent(input$td, {
		if (is.null(in_data))
		{
			showNotification("Не загружены данные для обработки!")
			return(NULL)
		}

		new_data = custom.melt(in_data, 2)
		num_vars = ncol(new_data)
		removeUI(
			selector = "div[id^='tab4_plot']",
			multiple = TRUE)

		for (index in 2:num_vars)
		{
			n = paste0("plot_", index - 1)
			insertUI(
				selector = "#tab4bottom",
				ui = tags$div(id = paste0("tab4_plot", index - 1), tags$p(colnames(new_data)[index]), plotOutput(n)))
			local({
				l = index
				output[[n]] = renderPlot({
					ggplot(new_data, aes(new_data[[1]], new_data[[l]])) +
						geom_violin() +
						stat_summary(fun.y=mean, geom="point", size=2) +
						labs(x = NULL, y = "Значение")
				})
			})
		}

		output$out_table <- renderTable(ds.dependent_ttest(in_data), rownames = TRUE)
		updateTabsetPanel(session, "mainTabs", selected = "Tab2")
	})


	observeEvent(input$st, {
		if (is.null(in_data))
		{
			showNotification("Не загружены данные для обработки!")
			return(NULL)
		}

		new_data = custom.melt(in_data, 2)
		num_vars = ncol(new_data)
		removeUI(
			selector = "div[id^='tab4_plot']",
			multiple = TRUE)

		for (index in 2:num_vars)
		{
			n = paste0("plot_", index - 1)
			insertUI(
				selector = "#tab4bottom",
				ui = tags$div(id = paste0("tab4_plot", index - 1), tags$p(colnames(new_data)[index]), plotOutput(n)))
			local({
				l = index
				output[[n]] = renderPlot({
					ggplot(new_data, aes(new_data[[1]], new_data[[l]])) +
						geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
						labs(x = NULL, y = "Значение")
				})
			})
		}

		output$out_table <- renderTable(ds.signtest(in_data), rownames = TRUE)
		updateTabsetPanel(session, "mainTabs", selected = "Tab2")
	})


	observeEvent(input$wmp, {
		if (is.null(in_data))
		{
			showNotification("Не загружены данные для обработки!")
			return(NULL)
		}

		new_data = custom.melt(in_data, 2)
		num_vars = ncol(new_data)
		removeUI(
			selector = "div[id^='tab4_plot']",
			multiple = TRUE)

		for (index in 2:num_vars)
		{
			n = paste0("plot_", index - 1)
			insertUI(
				selector = "#tab4bottom",
				ui = tags$div(id = paste0("tab4_plot", index - 1), tags$p(colnames(new_data)[index]), plotOutput(n)))
			local({
				l = index
				output[[n]] = renderPlot({
					ggplot(new_data, aes(new_data[[1]], new_data[[l]])) +
						geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
						labs(x = NULL, y = "Значение")
				})
			})
		}

		output$out_table <- renderTable(ds.wilcoxonmatchedpairs(in_data), rownames = TRUE)
		updateTabsetPanel(session, "mainTabs", selected = "Tab2")
	})
})