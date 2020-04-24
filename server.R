# TODO: graceful UI reset on tab switch, plot size settings
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

  # Where are we running?
  isLocal <- !nzchar(Sys.getenv("SHINY_PORT"))

  # Inputs were not filled with data yet
  ui_ready <- FALSE

  # Session start/end functionality
  onSessionStart <- isolate({
    cat(file = stderr(), paste0("Session started: ", session$token, "\r\n")) # USER TESTING TRACING
  })
  onSessionEnded(function() {
    cat(file = stderr(), paste0("Session ended: ", session$token, "\r\n")) # USER TESTING TRACING
  })

  # Internationalization
  i18n <- Translator$new(translation_csvs_path = "./translations")
  # Should specify config.yaml somewhere

  # Application settings
  settings_default <- list(
    lang = "English",
    p = 0.05,
    fa_cut = 0.3,
    fa_load = 0.5
  )
  settings <- reactiveVal()
  observeEvent(session, {
    q <- getQueryString()
    if (length(q) > 0) {
      s <- update_list(settings_default, q)
    } else {
      s <- settings_default
    }
    settings(s)
  })

  observe({
    s <- settings()
    current_lang <- i18n$translation_language
    if (length(current_lang) < 1) {
      current_lang <- ""
    }
    if (current_lang != s$lang) {
      set_language(s$lang)
    }
    user_settings <- named_diff(settings_default, settings())
    q <- ifelse(length(user_settings) > 0, list_to_query(user_settings), "")
    updateQueryString(q, mode = "replace")
  })

  included_vars <- reactive({
    input$si_include_vars
  })
  current_tab <- reactive({
    input$sidebar_tabs
  })

  # Data variables
  base_data <- reactiveVal()
  base_names <- reactiveVal()
  get_data <- reactive({
    droplevels(base_data()[input$in_table_rows_all, strtoi(included_vars()), drop = FALSE])
  })
  get_names <- reactive({
    base_names()[strtoi(included_vars())]
  })

  observeEvent(
    input$rb_language,
    ignoreInit = TRUE,
    {
      if (settings()$lang != input$rb_language) {
        settings(update_list(settings(), c("lang" = input$rb_language)))
      }
    }
  )

  observeEvent(
    input$sli_fa_cut,
    ignoreInit = TRUE,
    {
      if (settings()$fa_cut != input$sli_fa_cut) {
        settings(update_list(settings(), c("fa_cut" = input$sli_fa_cut)))
      }
    }
  )

  observeEvent(
    input$sli_fa_load,
    ignoreInit = TRUE,
    {
      if (settings()$fa_load != input$sli_fa_load) {
        settings(update_list(settings(), c("fa_load" = input$sli_fa_load)))
      }
    }
  )

  observeEvent(
    input$ni_p_level,
    ignoreInit = TRUE,
    {
      p = as.numeric(input$ni_p_level)
      if (is.numeric(p) && settings()$p != input$ni_p_level) {
        settings(update_list(settings(), c("p" = p)))
      }
    }
  )

  observeEvent(input$al_credits, {
    showModal(
      modalDialog(
        footer = NULL,
        size = "l",
        easyClose = TRUE,
        HTML(readr::read_file(glue("./translations/help/{i18n$translation_language}/credits.html")))
      )
    )
  })

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

  output$sdb_r_sidebar <- renderUI({
    if (!is.null(current_tab())) {
      HTML(readr::read_file(glue("./translations/help/{i18n$translation_language}/{current_tab()}.html")))
    }
  })

  observeEvent(input$sidebar_tabs, {
    if (input$sidebar_tabs != "data_upload") {
      if (!ui_ready) {
        if (!is.null(base_data())) {
          in_data <- get_data()

          if (nrow(in_data) > 0 && ncol(in_data) > 0) {
            fill_inputs()
            show(selector = "div.hidden_div")
            ui_ready <<- TRUE
          } else {
            hide(selector = "div.hidden_div")
            showNotification(i18n$t("Данные для обработки отсутствуют, проверьте фильтры."), type = "warning")
          }
        } else {
          showNotification(i18n$t("Не загружены данные для обработки!"), type = "warning")
        }
      }
    } else {
      ui_ready <<- FALSE
    }
  })

  observeEvent(input$si_reli_vars, {
    selections <- get_names()[strtoi(input$si_reli_vars)] %isnameof% 1:length(input$si_reli_vars)
    updatePickerInput(session, "si_reli_reversed_items", choices = selections)
  })

  observeEvent(input$si_dep_vars_regression, {
    conflict <- intersect(input$si_dep_vars_regression, input$si_ind_vars_regression)
    if (length(conflict) > 0) {
      updatePickerInput(session, "si_ind_vars_regression", selected = setdiff(input$si_ind_vars_regression, conflict))
    }
  })

  observeEvent(input$si_ind_vars_regression, {
    conflict <- intersect(input$si_dep_vars_regression, input$si_ind_vars_regression)
    if (length(conflict) > 0) {
      updatePickerInput(session, "si_dep_vars_regression", selected = setdiff(input$si_dep_vars_regression, conflict))
    }
  })

  # Analyzes launchers
  observeEvent(input$upload, {
    ds_execute(upload_file(input$upload))
  })

  observeEvent(input$ab_parametric_desc, {
    ds_execute(ds.descriptives("parametric"), showSelector = "div[class^=desc_box")
  })

  observeEvent(input$ab_nonparametric_desc, {
    ds_execute(ds.descriptives("nonparametric"), showSelector = "div[class^=desc_box")
  })

  observeEvent(input$ab_frequency_tables, {
    ds_execute(ds.frequencytables(), showSelector = "div[class^=dist_box")
  })

  observeEvent(input$ab_distplots, {
    ds_execute(ds.distributionplots(), showSelector = "div[class^=dist_box")
  })

  observeEvent(input$ab_shapirowilk, {
    ds_execute(ds.shapirowilk(), showSelector = "div[class^=dist_box")
  })

  observeEvent(input$ab_waldwolfowitz, {
    if (is.null(input$si_var_ctis)) {
      showNotification(i18n$t("Нет подходящих независимых переменных для данного вида анализа"), type = "error")
    } else {
      ds_execute(ds.cis("Z"), "div[class^=cis_box_b", "div[class^=cis_box_a")
    }
  })

  observeEvent(input$ab_kolmogorovsmirnov, {
    if (is.null(input$si_var_ctis)) {
      showNotification(i18n$t("Нет подходящих независимых переменных для данного вида анализа"), type = "error")
    } else {
      ds_execute(ds.cis("D"), "div[class^=cis_box_b", "div[class^=cis_box_a")
    }
  })

  observeEvent(input$ab_mannwhitney, {
    if (is.null(input$si_var_ctis)) {
      showNotification(i18n$t("Нет подходящих независимых переменных для данного вида анализа"), type = "error")
    } else {
      ds_execute(ds.cis("U"), "div[class^=cis_box_b", "div[class^=cis_box_a")
    }
  })

  observeEvent(input$ab_ttestindependent, {
    if (is.null(input$si_var_ctis)) {
      showNotification(i18n$t("Нет подходящих независимых переменных для данного вида анализа"), type = "error")
    } else {
      ds_execute(ds.cis("t"), "div[class^=cis_box_b", "div[class^=cis_box_a")
    }
  })

  observeEvent(input$ab_kruskallwallis, {
    if (is.null(input$si_var_cmis)) {
      showNotification(i18n$t("Не выбрана независимая переменная!"), type = "error")
    } else {
      ds_execute(ds.cis("H"), showSelector = "div[class^=cis_box")
    }
  })

  observeEvent(input$ab_welch, {
    if (is.null(input$si_var_cmis)) {
      showNotification(i18n$t("Не выбрана независимая переменная!"), type = "error")
    } else {
      ds_execute(ds.cis("F"), showSelector = "div[class^=cis_box")
    }
  })

  observeEvent(input$ab_ttestdependent, {
    if (ncol(get_data()) %% 2 != 0) {
      showNotification(i18n$t("Количество столбцов нечётное - проверьте наличие нужных данных и отсутствие лишних"), type = "error")
    } else {
      ds_execute(ds.cds("t"), "div[class^=cds_box_b", "div[class^=cds_box_a")
    }
  })

  observeEvent(input$ab_signtest, {
    if (ncol(get_data()) %% 2 != 0) {
      showNotification(i18n$t("Количество столбцов нечётное - проверьте наличие нужных данных и отсутствие лишних"), type = "error")
    } else {
      ds_execute(ds.cds("Z"), "div[class^=cds_box_b", "div[class^=cds_box_a")
    }
  })

  observeEvent(input$ab_wilcoxonmp, {
    if (ncol(get_data()) %% 2 != 0) {
      showNotification(i18n$t("Количество столбцов нечётное - проверьте наличие нужных данных и отсутствие лишних"), type = "error")
    } else {
      ds_execute(ds.cds("W"), "div[class^=cds_box_b", "div[class^=cds_box_a")
    }
  })

  observeEvent(input$ab_friedman, {
    if (ncol(get_data()) %% strtoi(input$measures_number) != 0) {
      showNotification(i18n$t("Количество столбцов не делится на количество замеров - проверьте наличие нужных данных и отсутствие лишних"), type = "error")
    } else {
      ds_execute(ds.cds("Q"), showSelector = "div[class^=cds_box")
    }
  })

  observeEvent(input$ab_repeatedmeasures, {
    if (ncol(get_data()) %% strtoi(input$measures_number) != 0) {
      showNotification(i18n$t("Количество столбцов не делится на количество замеров - проверьте наличие нужных данных и отсутствие лишних"), type = "error")
    } else {
      ds_execute(ds.cds("F"), showSelector = "div[class^=cds_box")
    }
  })

  observeEvent(input$ab_cor_pearson, {
    if ((length(input$si_var1_corr) < 1) || (length(input$si_var1_corr) < 1)) {
      showNotification(i18n$t("Не выбраны переменные для анализа"), type = "warning")
    } else {
      ds_execute(ds.correlations("pearson"), showSelector = "div[class^=corr_box")
    }
  })

  observeEvent(input$ab_cor_kendall, {
    if ((length(input$si_var1_corr) < 1) || (length(input$si_var1_corr) < 1)) {
      showNotification(i18n$t("Не выбраны переменные для анализа"), type = "warning")
    } else {
      ds_execute(ds.correlations("kendall"), showSelector = "div[class^=corr_box")
    }
  })

  observeEvent(input$ab_cor_spearman, {
    if ((length(input$si_var1_corr) < 1) || (length(input$si_var1_corr) < 1)) {
      showNotification(i18n$t("Не выбраны переменные для анализа"), type = "warning")
    } else {
      ds_execute(ds.correlations("spearman"), showSelector = "div[class^=corr_box")
    }
  })

  observeEvent(input$ab_reliability, {
    if (is.null(input$si_reli_vars)) {
      showNotification(i18n$t("Не выбраны переменные для анализа"), type = "warning")
    } else {
      ds_execute(ds.reliability(), showSelector = "div[class^=reli_box")
    }
  })

  observeEvent(input$ab_screeplot, {
    ds_execute(ds.screeplot(), "div[class^=fa_box]", "div[class^=fa_box_a]")
  })

  observeEvent(input$ab_factoranalysis, {
    ds_execute(ds.factoranalysis(), showSelector = "div[class^=fa_box]")
  })

  observeEvent(input$ab_dendro, {
    ds_execute(ds.dendro(), showSelector = "div[class^=clust_box]")
  })

  observeEvent(input$ab_clustering, {
    ds_execute(ds.clusteranalysis(), showSelector = "div[class^=clust_box]")
  })

  observeEvent(input$ab_manova, {
    if (length(input$si_vars_manova) < 2) {
      showNotification(i18n$t("Не выбрано достаточно независимых переменных!"), type = "error")
    } else {
      ds_execute(ds.manova(), showSelector = "div[class^=manova_box")
    }
  })

  observeEvent(input$ab_glm, {
    if (length(input$si_ind_vars_regression) < 1) {
      showNotification(i18n$t("Не выбраны независимые переменные!"), type = "error")
    } else if (length(input$si_dep_vars_regression) < 1) {
      showNotification(i18n$t("Не выбраны зависимые переменные!"), type = "error")
    } else {
      ds_execute(ds.glm(), showSelector = "div[class^=regression_box")
    }
  })

  observeEvent(input$ab_optimalglms, {
    if (length(input$si_ind_vars_regression) < 1) {
      showNotification(i18n$t("Не выбраны независимые переменные!"), type = "error")
    } else if (length(input$si_dep_vars_regression) < 1) {
      showNotification(i18n$t("Не выбраны зависимые переменные!"), type = "error")
    } else {
      ds_execute(ds.optimalglms(), "div[class^=regression_box_b", "div[class^=regression_box_a")
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
