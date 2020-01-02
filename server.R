shinyServer(function(input, output, session) {
	source("functions_layout.R", encoding = "utf-8", local = TRUE)
	source("descriptives.R", encoding = "utf-8", local = TRUE)
	source("distribution.R", encoding = "utf-8", local = TRUE)
	source("ctis.R", encoding = "utf-8", local = TRUE)
	source("cmis.R", encoding = "utf-8", local = TRUE)
	source("ctds.R", encoding = "utf-8", local = TRUE)
	source("cmds.R", encoding = "utf-8", local = TRUE)
	source("correlations.R", encoding = "utf-8", local = TRUE)
	source("reliability.R", encoding = "utf-8", local = TRUE)
	source("factor.R", encoding = "utf-8", local = TRUE)
	source("cluster.R", encoding = "utf-8", local = TRUE)
	source("manova.R", encoding = "utf-8", local = TRUE)
	source("regression.R", encoding = "utf-8", local = TRUE)

	enc = reactive({input$file_encoding})
	indep_var_ctis = reactive({input$si_var_ctis})
	indep_var_cmis = reactive({input$si_var_cmis})
	indep_vars_css = reactive({input$si_vars_manova})
	indep_vars_reg = reactive({input$si_vars_regression})
	measures_input = reactive({input$measures_number})
	corr1_var_list = reactive({input$si_var1_corr})
	corr2_var_list = reactive({input$si_var2_corr})
	included_vars = reactive({input$si_include_vars})
	factors_limit = reactive({input$factors_number})
	factoring_method = reactive({input$si_factoring_method})
	factor_rotation = reactive({input$si_factor_rotation})
	factor_normalize = reactive({input$cb_normalize})
	clusters_num = reactive({input$clusters_number})
	optimize_glm = reactive({input$cb_optimal_glm})

	get_data = reactive({
		base_data()[input$in_table_rows_all, strtoi(included_vars()), drop = FALSE]
	})

	get_names = reactive({
		base_names()[strtoi(included_vars())]
	})

	ui_ready = FALSE

	output$data_info = renderText({
		if (!is.null(base_data()))
			paste0(
				"Количество переменных: ", length(included_vars()),
				" из ", ncol(base_data()),
				"\r\nКоличество испытуемых: ", length(input$in_table_rows_all),
				" из ", nrow(base_data())
			)
	})

	base_data = reactiveVal()
	base_names = reactiveVal()

	observeEvent(input$upload, {

		in_file <- input$upload

		if (is.null(in_file))
			return(NULL)
# TODO: Various import formats
		temp_data = xlsx::read.xlsx(file = in_file$datapath, sheetIndex = 1, header = TRUE, encoding = enc())
		temp_names = colnames(xlsx::read.xlsx(file = in_file$datapath, sheetIndex = 1, header = TRUE, encoding = enc(), rowIndex = c(1, 2), check.names = FALSE))

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

		updatePickerInput(session, "si_include_vars", choices = selections, selected = selections)
		base_data(temp_data)
		base_names(temp_names)

		# Clear all previous results
		#clear.ui()

		output$in_table <- renderDT(base_data(), filter = list(position = "top"), options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Russian.json')))
		show("data_box")
		showNotification("Файл успешно загружен!", type = "message")
	})

	observeEvent(input$sidebar_tabs, {
		if (input$sidebar_tabs != "data_upload")
		{
			if (!ui_ready)
			{
				if(!is.null(base_data()))
				{
					in_data = get_data()

					if (nrow(in_data) > 0 && ncol(in_data) > 0)
					{
						fill.dropdowns()
						show(selector = "div.hidden_div")
						ui_ready <<- TRUE
					}
					else
					{
						hide(selector = "div.hidden_div")
						showNotification("Данные для обработки отсутствуют, проверьте фильтры.", type = "warning")
					}
				}
				else
				{
					showNotification("Не загружены данные для обработки!", type = "warning")
				}
			}
		}
		else
		{
			ui_ready <<- FALSE
		}
	})

	observeEvent(input$ab_parametric_desc, {
		ds.execute(ds.parametric_descriptives(), showSelector = "div[class^=desc_box")
	})

	observeEvent(input$ab_nonparametric_desc, {
		ds.execute(ds.nonparametric_descriptives(), showSelector = "div[class^=desc_box")
	})

	observeEvent(input$ab_frequency_tables, {
		ds.execute(ds.frequencytables(), showSelector = "div[class^=dist_box")
	})

	observeEvent(input$ab_distplots, {
		ds.execute(ds.distributionplots(), showSelector = "div[class^=dist_box")
	})

	observeEvent(input$ab_shapirowilk, {
		ds.execute(ds.shapirowilk(), showSelector = "div[class^=dist_box")
	})

	observeEvent(input$ab_mannwhitney, {
		ds.execute(ds.mannwhitney(), showSelector = "div[class^=ctis_box")
	})

	observeEvent(input$ab_ttestindependent, {
		ds.execute(ds.independent_ttest(), showSelector = "div[class^=ctis_box")
	})

	observeEvent(input$ab_kruskallwallis, {
		ds.execute(ds.kruskalwallis(), showSelector = "div[class^=cmis_box")
	})

	observeEvent(input$ab_welch, {
		ds.execute(ds.onewayanova(), showSelector = "div[class^=cmis_box")
	})

	observeEvent(input$ab_ttestdependent, {
		ds.execute(ds.dependent_ttest(), showSelector = "div[class^=ctds_box")
	})

	observeEvent(input$ab_signtest, {
		ds.execute(ds.signtest(), showSelector = "div[class^=ctds_box")
	})

	observeEvent(input$ab_wilcoxonmp, {
		ds.execute(ds.wilcoxonmatchedpairs(), showSelector = "div[class^=ctds_box")
	})

	observeEvent(input$ab_friedman, {
		ds.execute(ds.friedman(), showSelector = "div[class^=cmds_box")
	})

	observeEvent(input$ab_repeatedmeasures, {
		ds.execute(ds.repeatedanova(), showSelector = "div[class^=cmds_box")
	})

	observeEvent(input$ab_cor_pearson, {
		ds.execute(ds.correlations("pearson"), showSelector = "div[class^=corr_box")
	})

	observeEvent(input$ab_cor_spearman, {
		ds.execute(ds.correlations("spearman"), showSelector = "div[class^=corr_box")
	})

	observeEvent(input$ab_reliability, {
		ds.execute(ds.reliability(), showSelector = "div[class^=reli_box")
	})

	observeEvent(input$ab_screeplot, {
		ds.execute(ds.screeplot(), "div[class^=fa_box]", "div[class^=fa_box_a]")
	})

	observeEvent(input$ab_factoranalysis, {
		ds.execute(ds.factoranalysis(), showSelector = "div[class^=fa_box]")
	})

	observeEvent(input$ab_dendro, {
		ds.execute(ds.dendro(), showSelector = "div[class^=clust_box]")
	})

	observeEvent(input$ab_clustering, {
		ds.execute(ds.clusteranalysis(), showSelector = "div[class^=clust_box]")
	})

	observeEvent(input$ab_manova, {
		ds.execute(ds.manova(), showSelector = "div[class^=manova_box")
	})

	observeEvent(input$ab_regression, {
		ds.execute(ds.regression(optimize_glm()), showSelector = "div[class^=regression_box")
	})
})