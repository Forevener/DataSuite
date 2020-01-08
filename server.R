# TODO: Various import formats, clearUI()
shinyServer(function(input, output, session) {
  # Embed separate files
  source("functions_server.R", encoding = "utf-8", local = TRUE)
  source("descriptives.R", encoding = "utf-8", local = TRUE)
  source("distribution.R", encoding = "utf-8", local = TRUE)
  source("comparison_indep.R", encoding = "utf-8", local = TRUE)
  source("comparison_dep.R", encoding = "utf-8", local = TRUE)
  source("correlations.R", encoding = "utf-8", local = TRUE)
  source("reliability.R", encoding = "utf-8", local = TRUE)
  source("factor.R", encoding = "utf-8", local = TRUE)
  source("cluster.R", encoding = "utf-8", local = TRUE)
  source("manova.R", encoding = "utf-8", local = TRUE)
  source("regression.R", encoding = "utf-8", local = TRUE)

  enc <- reactive({
    input$file_encoding
  })
  indep_var_ctis <- reactive({
    input$si_var_ctis
  })
  indep_var_cmis <- reactive({
    input$si_var_cmis
  })
  indep_vars_css <- reactive({
    input$si_vars_manova
  })
  indep_vars_reg <- reactive({
    input$si_vars_regression
  })
  measures_input <- reactive({
    input$measures_number
  })
  corr1_var_list <- reactive({
    input$si_var1_corr
  })
  corr2_var_list <- reactive({
    input$si_var2_corr
  })
  included_vars <- reactive({
    input$si_include_vars
  })
  factors_limit <- reactive({
    input$factors_number
  })
  factoring_method <- reactive({
    input$si_factoring_method
  })
  factor_rotation <- reactive({
    input$si_factor_rotation
  })
  factor_normalize <- reactive({
    input$cb_normalize
  })
  clusters_num <- reactive({
    input$clusters_number
  })
  optimize_glm <- reactive({
    input$cb_optimal_glm
  })

  # Data variables
  base_data <- reactiveVal()
  base_names <- reactiveVal()
  get_data <- reactive({
    base_data()[input$in_table_rows_all, strtoi(included_vars()), drop = FALSE]
  })
  get_names <- reactive({
    base_names()[strtoi(included_vars())]
  })

  # Where are we running?
  isLocal <- !nzchar(Sys.getenv("SHINY_PORT"))

  onSessionStart = isolate({
    cat(file = stderr(), paste0("Session started: ", session$token, "\r\n")) # USER TESTING TRACING
  })
  onSessionEnded(function() {
    cat(file = stderr(), paste0("Session ended: ", session$token, "\r\n")) # USER TESTING TRACING
  })

  # Internationalization
  i18n <- Translator$new(translation_csvs_path = "./translations")
  # Should specify config.yaml somewhere

  observeEvent(session$clientData, {
    lang <- parseQueryString(session$clientData$url_search)$lang
    if (any(lang == i18n$languages)) {
      updatePrettyRadioButtons(session, "rb_language", selected = lang)
    } else {
      updatePrettyRadioButtons(session, "rb_language", selected = "English")
    }
  })

  observeEvent(
    input$rb_language, # ignoreInit = TRUE # when English is not first in the list of choices
    {
      i18n$set_translation_language(input$rb_language)
      source("ui_dynamic.R", encoding = "utf-8", local = TRUE)
      updateTabItems(session, "sidebar_tabs", "data_upload")
    }
  )

  ui_ready <- FALSE

  output$data_info <- renderText({
    if (!is.null(base_data())) {
      selected_cols <- length(included_vars())
      total_cols <- ncol(base_data())
      selected_rows <- length(input$in_table_rows_all)
      total_rows <- nrow(base_data())

      paste0(
        glue(i18n$t("Количество переменных: {selected_cols} из {total_cols}")), "\r\n",
        glue(i18n$t("Количество испытуемых: {selected_rows} из {total_rows}"))
      )
    }
  })

  output$additionalItems <- renderUI({
    if (isLocal) {
      tagList(
        "Debug options:",
        actionLink("ab_debug_browser", "browser()"),
        # prettyRadioButtons("rb_error_action", "action on error", choices = c("NULL", "recover", "browser"))
        prettySwitch("ps_handle_errors", "Error handling", value = TRUE, status = "success")
      )
    }
  })

  observeEvent(input$upload, {
    in_file <- input$upload
    cat(file = stderr(), paste0("File uploaded: ", in_file$name, "\r\n")) # USER TESTING TRACING

    if (is.null(in_file)) {
      return(NULL)
    }

    temp_data <- readxl::read_excel(in_file$datapath)
    temp_data <- janitor::remove_empty(temp_data)
    temp_names <- colnames(temp_data)
    colnames(temp_data) = janitor::make_clean_names(temp_names, case = "none")
    cat(file = stderr(), paste0("File structure: ", capture.output(str(temp_data)), "\r\n")) # USER TESTING TRACING

    selections <- temp_names %isnameof% 1:length(temp_names)
    updatePickerInput(session, "si_include_vars", choices = selections, selected = selections)

    base_data(temp_data)
    base_names(temp_names)

    # Clear all previous results
    # clear.ui()

    output$in_table <- renderDT(base_data(), filter = list(position = "top"), options = list(language = list(url = glue("//cdn.datatables.net/plug-ins/1.10.11/i18n/{i18n$translation_language}.json"))))
    show("data_box")
    showNotification(i18n$t("Файл успешно загружен!"), type = "message")
  })

  observeEvent(input$sidebar_tabs, {
    if (input$sidebar_tabs != "data_upload") {
      if (!ui_ready) {
        if (!is.null(base_data())) {
          in_data <- get_data()

          if (nrow(in_data) > 0 && ncol(in_data) > 0) {
            fill.dropdowns()
            show(selector = "div.hidden_div")
            ui_ready <<- TRUE
          }
          else {
            hide(selector = "div.hidden_div")
            showNotification(i18n$t("Данные для обработки отсутствуют, проверьте фильтры."), type = "warning")
          }
        }
        else {
          showNotification(i18n$t("Не загружены данные для обработки!"), type = "warning")
        }
      }
    }
    else {
      ui_ready <<- FALSE
    }
  })

  observeEvent(input$ab_parametric_desc, {
    ds.execute(ds.descriptives("parametric"), showSelector = "div[class^=desc_box")
  })

  observeEvent(input$ab_nonparametric_desc, {
    ds.execute(ds.descriptives("nonparametric"), showSelector = "div[class^=desc_box")
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

  observeEvent(input$ab_waldwolfowitz, {
    if (is.null(indep_var_ctis())) {
      showNotification(i18n$t("Нет подходящих независимых переменных для данного вида анализа"), type = "error")
    } else {
      ds.execute(ds.cis("Z"), "div[class^=cis_box_b", "div[class^=cis_box_a")
    }
  })

  observeEvent(input$ab_kolmogorovsmirnov, {
    if (is.null(indep_var_ctis())) {
      showNotification(i18n$t("Нет подходящих независимых переменных для данного вида анализа"), type = "error")
    } else {
      ds.execute(ds.cis("D"), "div[class^=cis_box_b", "div[class^=cis_box_a")
    }
  })

  observeEvent(input$ab_mannwhitney, {
    if (is.null(indep_var_ctis())) {
      showNotification(i18n$t("Нет подходящих независимых переменных для данного вида анализа"), type = "error")
    } else {
      ds.execute(ds.cis("U"), "div[class^=cis_box_b", "div[class^=cis_box_a")
    }
  })

  observeEvent(input$ab_ttestindependent, {
    if (is.null(indep_var_ctis())) {
      showNotification(i18n$t("Нет подходящих независимых переменных для данного вида анализа"), type = "error")
    } else {
      ds.execute(ds.cis("t"), "div[class^=cis_box_b", "div[class^=cis_box_a")
    }
  })

  observeEvent(input$ab_kruskallwallis, {
    if (is.null(indep_var_cmis())) {
      showNotification(i18n$t("Не выбрана независимая переменная!"), type = "error")
    } else {
      ds.execute(ds.cis("H"), showSelector = "div[class^=cis_box")
    }
  })

  observeEvent(input$ab_welch, {
    if (is.null(indep_var_cmis())) {
      showNotification(i18n$t("Не выбрана независимая переменная!"), type = "error")
    } else {
      ds.execute(ds.cis("F"), showSelector = "div[class^=cis_box")
    }
  })

  observeEvent(input$ab_ttestdependent, {
    if (ncol(get_data()) %% 2 != 0) {
      showNotification(i18n$t("Количество столбцов нечётное - проверьте наличие нужных данных и отсутствие лишних"), type = "error")
    } else {
      ds.execute(ds.cds("t"), "div[class^=cds_box_b", "div[class^=cds_box_a")
    }
  })

  observeEvent(input$ab_signtest, {
    if (ncol(get_data()) %% 2 != 0) {
      showNotification(i18n$t("Количество столбцов нечётное - проверьте наличие нужных данных и отсутствие лишних"), type = "error")
    } else {
      ds.execute(ds.cds("Z"), "div[class^=cds_box_b", "div[class^=cds_box_a")
    }
  })

  observeEvent(input$ab_wilcoxonmp, {
    if (ncol(get_data()) %% 2 != 0) {
      showNotification(i18n$t("Количество столбцов нечётное - проверьте наличие нужных данных и отсутствие лишних"), type = "error")
    } else {
      ds.execute(ds.cds("W"), "div[class^=cds_box_b", "div[class^=cds_box_a")
    }
  })

  observeEvent(input$ab_friedman, {
    if (ncol(get_data()) %% strtoi(measures_input()) != 0) {
      showNotification(i18n$t("Количество столбцов не делится на количество замеров - проверьте наличие нужных данных и отсутствие лишних"), type = "error")
    } else {
      ds.execute(ds.cds("Q"), showSelector = "div[class^=cds_box")
    }
  })

  observeEvent(input$ab_repeatedmeasures, {
    if (ncol(get_data()) %% strtoi(measures_input()) != 0) {
      showNotification(i18n$t("Количество столбцов не делится на количество замеров - проверьте наличие нужных данных и отсутствие лишних"), type = "error")
    } else {
      ds.execute(ds.cds("F"), showSelector = "div[class^=cds_box")
    }
  })

  observeEvent(input$ab_cor_pearson, {
    if ((length(corr1_var_list()) < 1) || (length(corr1_var_list()) < 1)) {
      showNotification(i18n$t("Не выбраны переменные для анализа"), type = "warning")
    } else {
      ds.execute(ds.correlations("pearson"), showSelector = "div[class^=corr_box")
    }
  })

  observeEvent(input$ab_cor_kendall, {
    if ((length(corr1_var_list()) < 1) || (length(corr1_var_list()) < 1)) {
      showNotification(i18n$t("Не выбраны переменные для анализа"), type = "warning")
    } else {
      ds.execute(ds.correlations("kendall"), showSelector = "div[class^=corr_box")
    }
  })

  observeEvent(input$ab_cor_spearman, {
    if ((length(corr1_var_list()) < 1) || (length(corr1_var_list()) < 1)) {
      showNotification(i18n$t("Не выбраны переменные для анализа"), type = "warning")
    } else {
      ds.execute(ds.correlations("spearman"), showSelector = "div[class^=corr_box")
    }
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
    if (length(indep_vars_css()) < 2) {
      showNotification(i18n$t("Не выбрано достаточно независимых переменных!"), type = "error")
    } else {
      ds.execute(ds.manova(), showSelector = "div[class^=manova_box")
    }
  })

  observeEvent(input$ab_regression, {
    if (length(indep_vars_reg()) < 1) {
      showNotification(i18n$t("Не выбраны независимые переменные!"), type = "error")
    } else {
        ds.execute(ds.regression(optimize_glm()), showSelector = "div[class^=regression_box")
    }
  })

  # Debug options
  observeEvent(input$ab_debug_browser, {
    browser()
  })

  # observeEvent(input$rb_error_action, {
  # 	if (input$rb_error_action == "NULL")
  # 		action = NULL
  # 	else
  # 		action = sym(input$rb_error_action)
  #
  # 	options(shiny.error = action)
  # })
})
