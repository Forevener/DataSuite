library(shiny)
library(rhandsontable)
library(xlsx)

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

		output$out_table <- renderTable(ds.mannwhitney(in_data, strtoi(indep_var_ctis())), rownames = TRUE)
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

		output$out_table <- renderTable(ds.onewayanova(in_data, strtoi(indep_var_cmis())), rownames = TRUE)
		updateTabsetPanel(session, "mainTabs", selected = "Tab2")
	})
})