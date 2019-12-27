shinyServer(function(input, output, session) {
	source("layout.R", encoding = "utf-8", local = TRUE)
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
	source("factor.R", encoding = "utf-8", local = TRUE)
	source("cluster.R", encoding = "utf-8", local = TRUE)
	#source("regression.R", encoding = "utf-8", local = TRUE)
	#source("alpha.R", encoding = "utf-8", local = TRUE)

	enc = reactive({input$radio})
	indep_var_ctis = reactive({input$ctis_dropdown})
	indep_var_cmis = reactive({input$cmis_dropdown})
	indep_vars_css = reactive({input$vars_manova})
	measures_input = reactive({input$measures_number})
	corr1_var_list = reactive({input$cl1_dropdown})
	corr2_var_list = reactive({input$cl2_dropdown})
	included_vars = reactive({input$include_vars})
	factors_limit = reactive({input$factors_number})
	clusters_num = reactive({input$clusters_number})

	get_data = reactive({
		base_data()[input$in_table_rows_all, strtoi(included_vars())]
	})

	get_names = reactive({
		base_names()[strtoi(included_vars())]
	})

	output$data_info = renderText({
		paste0(
			"Количество переменных: ",
			ifelse(is.null(base_data()), 0, length(included_vars())),
			"\r\nКоличество испытуемых: ",
			length(input$in_table_rows_all)
		)
	})

	base_data = reactiveVal()
	base_names = reactiveVal()

	observeEvent(input$upload, {

		in_file <- input$upload

		if (is.null(in_file))
			return(NULL)

		temp_data = read.xlsx(file = in_file$datapath, sheetIndex = 1, header = TRUE, encoding = enc())
		temp_names = colnames(read.xlsx(file = in_file$datapath, sheetIndex = 1, header = TRUE, encoding = enc(), rowIndex = c(1, 2), check.names = FALSE))

		selections = list()

		# Removing empty columns
		index = 1
		repeat
		{
			if (index > ncol(temp_data))
				break
			if (all(is.na(temp_data[index])))
			{
				temp_data = temp_data[-index]
				if (index <= length(temp_names))
					temp_names = temp_names[-index]
			}
			else
			{
				names(index) = temp_names[index]
				selections = append(selections, index)
				index = index + 1
			}
		}

		# Removing empty rows
		index = 1
		repeat
		{
			if (index > nrow(temp_data))
				break
			if (all(is.na(temp_data[index, ])))
			{
				temp_data = temp_data[-index, ]
			}
			else
			{
				index = index + 1
			}
		}

		updatePickerInput(session, "include_vars", choices = selections, selected = selections)
		base_data(temp_data)
		base_names(temp_names)

		# Clear all results
		clear.ui()

		output$in_table <- renderDT(base_data(), filter = list(position = "top"), options = list(scrollX = TRUE))
		showNotification("Файл успешно загружен!", type = "message")
	})

	observeEvent(input$navbar, {
		if (req(input$navbar) == "Обработка данных")
		{
			if (!is.null(base_data()))
				fill.dropdowns()
			else
				showNotification("Не загружены данные для обработки", type = "warning")
		}
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

	observeEvent(input$sp, {
		ds.screeplot()
	})

	observeEvent(input$fa, {
		ds.factoranalysis()
	})

	observeEvent(input$hc, {
		ds.dendro()
	})

	observeEvent(input$ca, {
		ds.clusteranalysis()
	})
})