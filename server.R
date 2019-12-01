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

	source("parametric_descriptives.r", encoding = "utf-8", local = TRUE)
	source("nonparametric_descriptives.r", encoding = "utf-8", local = TRUE)
	source("frequency_tables.r", encoding = "utf-8", local = TRUE)
	source("distribution_plots.r", encoding = "utf-8", local = TRUE)
	source("shapiro-wilk.r", encoding = "utf-8", local = TRUE)
	source("mann-whitney.r", encoding = "utf-8", local = TRUE)
	source("independent_t.r", encoding = "utf-8", local = TRUE)
	source("kruskal-wallis.r", encoding = "utf-8", local = TRUE)
	source("one-way_anova.r", encoding = "utf-8", local = TRUE)
	source("dependent_t.r", encoding = "utf-8", local = TRUE)
	source("sign_test.r", encoding = "utf-8", local = TRUE)
	source("wilcoxon_matched_pairs.r", encoding = "utf-8", local = TRUE)


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
		ds.parametric_descriptives()
	})

	observeEvent(input$dnp, {
		ds.nonparametric_descriptives()
	})

	observeEvent(input$ft, {
		ds.frequencytables()
	})

	observeEvent(input$distplots, {
		ds.distributionplots()
	})

	observeEvent(input$sw, {
		ds.shapirowilk()
	})

	observeEvent(input$mw, {
		ds.mannwhitney()
	})

	observeEvent(input$ti, {
		ds.independent_ttest()
	})

	observeEvent(input$kw, {
		ds.kruskalwallis()
	})

	observeEvent(input$ff, {
		ds.onewayanova()
	})

	observeEvent(input$td, {
		ds.dependent_ttest()
	})

	observeEvent(input$st, {
		ds.signtest()
	})

	observeEvent(input$wmp, {
		ds.wilcoxonmatchedpairs()
	})
})