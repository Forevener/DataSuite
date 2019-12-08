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
	indep_vars_css = reactive({input$vars_manova})
	measures_input = reactive({input$measures_number})
	corr1_var_list = reactive({input$cl1_dropdown})
	corr2_var_list = reactive({input$cl2_dropdown})

	in_data = NULL

	source("parametric_descriptives.R", encoding = "utf-8", local = TRUE)
	source("nonparametric_descriptives.R", encoding = "utf-8", local = TRUE)
	source("frequency_tables.R", encoding = "utf-8", local = TRUE)
	source("distribution_plots.R", encoding = "utf-8", local = TRUE)
	source("shapiro-wilk.R", encoding = "utf-8", local = TRUE)
	source("mann-whitney.R", encoding = "utf-8", local = TRUE)
	source("independent_t.R", encoding = "utf-8", local = TRUE)
	source("kruskal-wallis.R", encoding = "utf-8", local = TRUE)
	source("one-way_anova.R", encoding = "utf-8", local = TRUE)
	source("manova.R", encoding = "utf-8", local = TRUE)
	source("dependent_t.R", encoding = "utf-8", local = TRUE)
	source("sign_test.R", encoding = "utf-8", local = TRUE)
	source("wilcoxon_matched_pairs.R", encoding = "utf-8", local = TRUE)
	source("friedman_anova.R", encoding = "utf-8", local = TRUE)
	source("repeated_anova.R", encoding = "utf-8", local = TRUE)
	source("correlations.R", encoding = "utf-8", local = TRUE)
	#source("factor.R", encoding = "utf-8", local = TRUE)
	#source("cluster.R", encoding = "utf-8", local = TRUE)
	#source("regression.R", encoding = "utf-8", local = TRUE)

	observeEvent(input$upload, {

		in_file <- input$upload

		if (is.null(in_file))
			return(NULL)

		in_data <<- read.xlsx(file = in_file$datapath, sheetIndex = 1, header = TRUE, encoding = enc())

		selections_tis = list()
		selections_mis = list()
		selections_cor = list()
		for (index in 1:ncol(in_data))
		{
			names(index) = colnames(in_data[index])
			if (length(levels(factor(in_data[[index]]))) == 2)
				selections_tis = append(selections_tis, index)
			selections_mis = append(selections_mis, index)
			if (is.numeric(in_data[[index]]))
				selections_cor = append(selections_cor, index)
		}
		if (length(selections_tis) >= 1)
			updateSelectInput(session, "ctis_dropdown", choices = selections_tis, selected = selections_tis[1])
		if (length(selections_mis) >= 1)
		{
			updateSelectInput(session, "cmis_dropdown", choices = selections_mis, selected = selections_mis[1])
			updateSelectInput(session, "vars_manova", choices = selections_mis)
		}
		if (length(selections_cor) >= 1)
		{
			updateSelectInput(session, "cl1_dropdown", choices = selections_cor)
			updateSelectInput(session, "cl2_dropdown", choices = selections_cor)
		}

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

	observeEvent(input$mav, {
		ds.manova()
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

	observeEvent(input$fd, {
		ds.friedman()
	})

	observeEvent(input$rma, {
		ds.repeatedanova()
	})

	observeEvent(input$cp, {
		ds.correlations("pearson")
	})

	observeEvent(input$cs, {
		ds.correlations("spearman")
	})
})