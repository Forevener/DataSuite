source("functions_ui.R", encoding = "utf-8", local = TRUE)

output$sdb_sidebar <- renderUI({
  sidebarMenu(
    id = "sidebar_tabs",
    menuItem(i18n$t("Ввод и отбор данных"), tabName = "data_upload"),
    menuItem(i18n$t("Описательная статистика"), tabName = "descriptive"),
    menuItem(i18n$t("Распределение"), tabName = "distribution"),
    menuItem(i18n$t("Сравнение независимых выборок"), tabName = "comparison_IS"),
    menuItem(i18n$t("Сравнение зависимых выборок"), tabName = "comparison_DS"),
    menuItem(i18n$t("Корреляционный анализ"), tabName = "correlations"),
    menuItem(i18n$t("Надёжность и согласованность"), tabName = "reliability"),
    menuItem(i18n$t("Факторный анализ"), tabName = "factor"),
    menuItem(i18n$t("Кластерный анализ"), tabName = "cluster"),
    menuItem(i18n$t("МФ-дисперсионный анализ"), tabName = "manova"),
    menuItem(i18n$t("Регрессионный анализ"), tabName = "regression"),
    uiOutput("additionalItems")
  )
})

output$sdb_body <- renderUI({
  tabItems(
    tabItem(
      "data_upload",
      box(
        selectInput("file_encoding", h5(i18n$t("Кодировка файла")), choices = list("UTF-8" = "UTF-8", "Windows 1251" = "windows-1251", "Windows 1252" = "windows-1252"), selected = "UTF-8"),
        helpText(i18n$t("Если текст в таблицах отображается некорректно, выберите другую кодировку и загрузите файл данных заново")),
        fileInput("upload", label = i18n$t("Загрузить файл данных"), buttonLabel = i18n$t("Обзор"), placeholder = i18n$t("Файл не выбран"), accept = c(".xlsx", ".xls"))
      ),
      box(
        helpText(i18n$t("Если требуется исключить некоторые переменные из анализа (например, группирующие переменные при сравнении зависимых выборок), укажите их в следующем списке")),
        ds_picker("si_include_vars", i18n$t("Выбор переменных для анализа"), TRUE, TRUE),
        verbatimTextOutput("data_info")
      ),
      fluidRow(
        hidden_box(
          class = NULL, id = "databox", width = 12,
          div(style = "overflow-x: auto", DTOutput("in_table"))
        )
      )
    ),
    tabItem(
      "descriptive",
      hidden_box(
        actionButton("ab_parametric_desc", i18n$t("Параметрическая")),
        actionButton("ab_nonparametric_desc", i18n$t("Непараметрическая"))
      ),
      fluidRow(hidden_box(
        class = "desc_box", width = 12, title = i18n$t("Результаты"),
        tags$div(id = "key_div_desc", style = "overflow-x: auto")
      ))
    ),
    tabItem(
      "distribution",
      hidden_box(
        actionButton("ab_frequency_tables", i18n$t("Частоты встречаемости")),
        actionButton("ab_distplots", i18n$t("Графики распределения")),
        actionButton("ab_shapirowilk", i18n$t("Критерий Шапиро-Уилка"))
      ),
      fluidRow(hidden_box(
        class = "dist_box", width = 12, title = i18n$t("Результаты"),
        tags$div(id = "key_div_dist", style = "overflow-x: auto")
      ))
    ),
    tabItem(
      "comparison_IS",
      hidden_box(
        title = i18n$t("Две выборки"),
        ds_picker("si_var_ctis", i18n$t("Независимая переменная")),
        actionButton("ab_mannwhitney", i18n$t("U-критерий Манна-Уитни")),
        actionButton("ab_waldwolfowitz", i18n$t("Z-критерий Уалда-Вольфовица")),
        actionButton("ab_kolmogorovsmirnov", i18n$t("D-критерий Колмогорова-Смирнова")),
        actionButton("ab_ttestindependent", i18n$t("t-критерий Стьюдента"))
      ),
      hidden_box(
        title = i18n$t("Несколько выборок"),
        ds_picker("si_var_cmis", i18n$t("Независимая переменная")),
        actionButton("ab_kruskallwallis", i18n$t("H-критерий Краскела-Уоллиса")),
        actionButton("ab_welch", i18n$t("F-критерий Уэлча"))
      ),
      fluidRow(hidden_box(
        class = "cis_box_a", width = 12, title = i18n$t("Результаты"),
        tags$div(id = "key_div_cis_table", style = "overflow-x: auto")
      )),
      fluidRow(hidden_box(
        class = "cis_box_b", width = 12, title = i18n$t("Подробности"), collapsible = TRUE, collapsed = TRUE,
        tags$div(id = "key_div_cis_details", style = "overflow-x: auto")
      )),
      fluidRow(hidden_box(
        class = "cis_box_a", width = 12, title = i18n$t("Графики"), collapsible = TRUE, collapsed = TRUE,
        tags$div(id = "key_div_cis_plots", style = "overflow-x: auto")
      ))
    ),
    tabItem(
      "comparison_DS",
      hidden_box(
        title = i18n$t("Две выборки"),
        actionButton("ab_ttestdependent", i18n$t("t-критерий Стьюдента")),
        actionButton("ab_signtest", i18n$t("Критерий знаков")),
        actionButton("ab_wilcoxonmp", i18n$t("W-критерий Уилкоксона"))
      ),
      hidden_box(
        title = i18n$t("Несколько выборок"),
        numericInput("measures_number", h5(i18n$t("Количество замеров")), value = 3, min = 3),
        actionButton("ab_friedman", i18n$t("Q-критерий Фридмана")),
        actionButton("ab_repeatedmeasures", i18n$t("Анализ повторяющихся наблюдений"))
      ),
      fluidRow(hidden_box(
        class = "cds_box_a", width = 12, title = i18n$t("Результаты"),
        tags$div(id = "key_div_cds_table", style = "overflow-x: auto")
      )),
      fluidRow(hidden_box(
        class = "cds_box_b", width = 12, title = i18n$t("Подробности"), collapsible = TRUE, collapsed = TRUE,
        tags$div(id = "key_div_cds_details", style = "overflow-x: auto")
      )),
      fluidRow(hidden_box(
        class = "cds_box_a", width = 12, title = i18n$t("Графики"), collapsible = TRUE, collapsed = TRUE,
        tags$div(id = "key_div_cds_plots", style = "overflow-x: auto")
      ))
    ),
    tabItem(
      "correlations",
      hidden_box(
        ds_picker("si_var1_corr", i18n$t("Строки матрицы"), TRUE),
        ds_picker("si_var2_corr", i18n$t("Столбцы матрицы"), TRUE),
        actionButton("ab_cor_pearson", i18n$t("r-критерий Пирсона")),
        actionButton("ab_cor_kendall", i18n$t("тау-критерий Кендалла")),
        actionButton("ab_cor_spearman", i18n$t("ро-критерий Спирмена"))
      ),
      fluidRow(hidden_box(
        class = "corr_box", width = 12, title = i18n$t("Результаты"),
        tags$div(id = "key_div_corr_tables", style = "overflow-x: auto")
      )),
      fluidRow(hidden_box(
        class = "corr_box", width = 12, title = i18n$t("Графики"), collapsible = TRUE, collapsed = TRUE,
        tags$div(id = "key_div_corr_plots", style = "overflow-x: auto")
      ))
    ),
    tabItem(
      "reliability",
      hidden_box(
        ds_picker("si_reli_vars", i18n$t("Выбор переменных для анализа"), TRUE, TRUE),
        ds_picker("si_reli_reversed_items", i18n$t("Обратные пункты/шкалы"), TRUE, TRUE),
        actionButton("ab_reliability", i18n$t("Альфа Кронбаха и др."))
      ),
      fluidRow(hidden_box(
        class = "reli_box", width = 12, title = i18n$t("Результаты"),
        tags$div(id = "key_div_reli_table", style = "overflow-x: auto")
      ))
    ),
    tabItem(
      "factor",
      hidden_box(
        flowLayout(
          selectInput("si_factoring_method", h5(i18n$t("Метод факторизации")), choices = list(i18n$t("Главные компоненты"), i18n$t("Минимальные остатки"), i18n$t("Наименьшие квадраты"), i18n$t("Эмпирические наименьшие квадраты"), i18n$t("Взвешенные наименьшие квадраты"), i18n$t("Обобщённые взвешенные наименьшие квадраты"), i18n$t("Главные оси"), i18n$t("Максимальное сходство"), i18n$t("Минимальный хи-квадрат"), i18n$t("Минимальный ранг"), i18n$t("Альфа Кайзера-Коффи")) %isnameof% list("pc", "minres", "uls", "ols", "wls", "gls", "pa", "ml", "minchi", "minrank", "alpha")),
          selectInput("si_factor_rotation", h5(i18n$t("Вращение матрицы")), choices = list(i18n$t("Нет"), i18n$t("Варимакс"), i18n$t("Квартимакс"), i18n$t("Т Бентлера"), i18n$t("Эквамакс"), i18n$t("Варимин"), i18n$t("Т Геомин"), i18n$t("Двухфакторное"), i18n$t("Промакс"), i18n$t("Облимин"), i18n$t("Симплимакс"), i18n$t("Q Бентлера"), i18n$t("Q Геомин"), i18n$t("Биквартимин"), i18n$t("Кластерное")) %isnameof% list("none", "varimax", "quartimax", "bentlerT", "equamax", "varimin", "geominT", "bifactor", "Promax", "oblimin", "simplimax", "bentlerQ", "geominQ", "biquartimin", "cluster"), selected = "varimax"),
          actionButton("ab_screeplot", i18n$t("График осыпи")),
          numericInput("factors_number", h5(i18n$t("Количество факторов")), value = 1, min = 1),
          checkboxInput("cb_normalize", i18n$t("Нормализация вращения"), TRUE),
          actionButton("ab_factoranalysis", i18n$t("Факторный анализ"))
        )
      ),
      fluidRow(hidden_box(
        class = "fa_box_a", width = 12, title = i18n$t("Основные результаты"),
        tags$div(id = "key_div_fa_table", style = "overflow-x: auto")
      )),
      fluidRow(hidden_box(
        class = "fa_box_b", width = 12, title = i18n$t("Подробности"), collapsible = TRUE, collapsed = TRUE,
        tags$div(id = "key_div_fa_details", style = "overflow-x: auto")
      )),
      fluidRow(hidden_box(
        class = "fa_box_b", width = 12, title = i18n$t("Графики"), collapsible = TRUE, collapsed = TRUE,
        tags$div(id = "key_div_fa_plots", style = "overflow-x: auto")
      ))
    ),
    tabItem(
      "cluster",
      hidden_box(
        actionButton("ab_dendro", i18n$t("Дендрограмма и подбор количества")),
        numericInput("clusters_number", h5(i18n$t("Количество кластеров")), value = 2, min = 2),
        actionButton("ab_clustering", i18n$t("Кластерный анализ"))
      ),
      fluidRow(hidden_box(
        class = "clust_box", width = 12, title = i18n$t("Результаты"),
        tags$div(id = "key_div_clust_main", style = "overflow-x: auto")
      )),
      fluidRow(hidden_box(
        class = "clust_box", width = 12, title = i18n$t("Графики"), collapsible = TRUE, collapsed = TRUE,
        tags$div(id = "key_div_clust_plots", style = "overflow-x: auto")
      ))
    ),
    tabItem(
      "manova",
      hidden_box(
        ds_picker("si_vars_manova", i18n$t("Независимые переменные"), TRUE),
        actionButton("ab_manova", i18n$t("Многофакторный дисперсионный анализ"))
      ),
      fluidRow(hidden_box(
        class = "manova_box_a", width = 12, title = i18n$t("Результаты"),
        tags$div(id = "key_div_manova_tables", style = "overflow-x: auto")
      )),
      fluidRow(hidden_box(
        class = "manova_box_b", width = 12, title = i18n$t("Графики"), collapsible = TRUE, collapsed = TRUE,
        tags$div(id = "key_div_manova_plots", style = "overflow-x: auto")
      ))
    ),
    tabItem(
      "regression",
      hidden_box(
        ds_picker("si_vars_regression", i18n$t("Независимые переменные"), TRUE),
        prettyCheckbox("cb_optimal_glm", i18n$t("Выбирать оптимальную модель")),
        actionButton("ab_regression", i18n$t("Обобщённая линейная регрессия"))
      ),
      fluidRow(hidden_box(
        class = "regression_box", width = 12, title = i18n$t("Результаты"),
        tags$div(id = "key_div_regression_tables", style = "overflow-x: auto")
      )),
      fluidRow(hidden_box(
        class = "regression_box", width = 12, title = i18n$t("Графики"), collapsible = TRUE, collapsed = TRUE,
        tags$div(id = "key_div_regression_plots", style = "overflow-x: auto")
      ))
    )
  )
})
