shinyUI(
  dashboardPage(
    dashboardHeader(title = "Data Suite"),
    dashboardSidebar(width = 270,
                     sidebarMenu(
                       id = "sidebar_tabs",

                       menuItem("Ввод и отбор данных", tabName = "data_upload"),
                       menuItem("Описательная статистика", tabName = "descriptive"),
                       menuItem("Распределение", tabName = "distribution"),
                       menuItem("Сравнение независимых выборок",
                                menuSubItem("Двух", tabName = "comparison_TIS"),
                                menuSubItem("Нескольких", tabName = "comparison_MIS")
                       ),
                       menuItem("Сравнение зависимых выборок",
                                menuSubItem("Двух", tabName = "comparison_TDS"),
                                menuSubItem("Нескольких", tabName = "comparison_MDS")
                       ),
                       menuItem("Корреляционный анализ", tabName = "correlations"),
                       menuItem("Надёжность и согласованность", tabName = "reliability"),
                       menuItem("Факторный анализ", tabName = "factor"),
                       menuItem("Кластерный анализ", tabName = "cluster"),
                       menuItem("МФ-дисперсионный анализ", tabName = "manova"),
                       menuItem("Регрессионный анализ", tabName = "regression")
                     )
    ),
    dashboardBody(
      useShinyjs(),
      tabItems(
        tabItem("data_upload",
                box(
                  selectInput("file_encoding", h5("Кодировка файла"), choices = list("UTF-8" = "UTF-8", "Windows 1251" = "windows-1251", "Windows 1252" = "windows-1252"), selected = "UTF-8"),
                  helpText("Если текст в таблицах отображается некорректно, выберите другую кодировку и загрузите файл данных заново"),
                  fileInput("upload", label = "Загрузить файл данных")
                ),
                box(
                  helpText("Если требуется исключить некоторые переменные из анализа (например, группирующие переменные при сравнении зависимых выборок), укажите их в следующем списке"),
                  ds.picker("si_include_vars", "Выбор переменных для анализа", TRUE, TRUE),
                  verbatimTextOutput("data_info")
                ),
                fluidRow(
                  hidden.box(class = NULL, id = "data_box", width = 12,
                             div(style = "overflow-x: auto",  DTOutput("in_table"))
                  )
                )

        ),
        tabItem("descriptive",
                hidden.box(
                  actionButton("ab_parametric_desc", "Параметрическая"),
                  actionButton("ab_nonparametric_desc", "Непараметрическая")
                ),
                fluidRow(hidden.box(class = "desc_box", width =  12, title = "Результаты",
                                    tags$div(id = "key_div_desc", style = "overflow-x: auto")))
        ),
        tabItem("distribution",
                hidden.box(
                  actionButton("ab_frequency_tables", "Частоты встречаемости"),
                  actionButton("ab_distplots", "Графики распределения"),
                  actionButton("ab_shapirowilk", "Критерий Шапиро-Уилка")
                ),
                fluidRow(hidden.box(class = "dist_box", width =  12, title = "Результаты",
                                    tags$div(id = "key_div_dist", style = "overflow-x: auto")))
        ),
        tabItem("comparison_TIS",
                hidden.box(
                  selectInput("si_var_ctis", h5("Независимая переменная"), choices = list("-" = 0), selected = "-"),
                  actionButton("ab_mannwhitney", "U-критерий Манна-Уитни"),
                  actionButton("ab_ttestindependent", "t-критерий Стьюдента")
                ),
                fluidRow(hidden.box(class = "ctis_box", width =  12, title = "Результаты",
                                    tags$div(id = "key_div_ctis_table", style = "overflow-x: auto"))),
                fluidRow(hidden.box(class = "ctis_box", width =  12, title = "Графики", collapsible = TRUE, collapsed = TRUE,
                                    tags$div(id = "key_div_ctis_plots", style = "overflow-x: auto")))
        ),
        tabItem("comparison_MIS",
                hidden.box(
                  selectInput("si_var_cmis", h5("Независимая переменная"), choices = list("-" = 0), selected = "-"),
                  actionButton("ab_kruskallwallis", "H-критерий Краскела-Уоллиса"),
                  actionButton("ab_welch", "F-критерий Уэлча")
                ),
                fluidRow(hidden.box(class = "cmis_box", width =  12, title = "Основные результаты",
                                    tags$div(id = "key_div_cmis_table", style = "overflow-x: auto"))),
                fluidRow(hidden.box(class = "cmis_box", width =  12, title = "Подробности", collapsible = TRUE, collapsed = TRUE,
                                    tags$div(id = "key_div_cmis_details", style = "overflow-x: auto"))),
                fluidRow(hidden.box(class = "cmis_box", width =  12, title = "Графики", collapsible = TRUE, collapsed = TRUE,
                                    tags$div(id = "key_div_cmis_plots", style = "overflow-x: auto")))
        ),
        tabItem("comparison_TDS",
                hidden.box(
                  actionButton("ab_ttestdependent", "t-критерий Стьюдента"),
                  actionButton("ab_signtest", "Критерий знаков"),
                  actionButton("ab_wilcoxonmp", "W-критерий Уилкоксона")
                ),
                fluidRow(hidden.box(class = "ctds_box", width =  12, title = "Результаты",
                                    tags$div(id = "key_div_ctds_table", style = "overflow-x: auto"))),
                fluidRow(hidden.box(class = "ctds_box", width =  12, title = "Графики", collapsible = TRUE, collapsed = TRUE,
                                    tags$div(id = "key_div_ctds_plots", style = "overflow-x: auto")))
        ),
        tabItem("comparison_MDS",
                hidden.box(
                  numericInput("measures_number", h5("Количество замеров"), value = 3, min = 3),
                  actionButton("ab_friedman", "Критерий Фридмана"),
                  actionButton("ab_repeatedmeasures", "Анализ повторяющихся наблюдений")
                ),
                fluidRow(hidden.box(class = "cmds_box", width =  12, title = "Основные результаты",
                                    tags$div(id = "key_div_cmds_table", style = "overflow-x: auto"))),
                fluidRow(hidden.box(class = "cmds_box", width =  12, title = "Подробности", collapsible = TRUE, collapsed = TRUE,
                                    tags$div(id = "key_div_cmds_details", style = "overflow-x: auto"))),
                fluidRow(hidden.box(class = "cmds_box", width =  12, title = "Графики", collapsible = TRUE, collapsed = TRUE,
                                    tags$div(id = "key_div_cmds_plots", style = "overflow-x: auto")))
        ),
        tabItem("correlations",
                hidden.box(
                  ds.picker("si_var1_corr", "Строки матрицы", TRUE),
                  ds.picker("si_var2_corr", "Столбцы матрицы", TRUE),
                  actionButton("ab_cor_pearson", "r-критерий Пирсона"),
                  actionButton("ab_cor_spearman", "ρ-критерий Спирмена")
                ),
                fluidRow(hidden.box(class = "corr_box", width =  12, title = "Результаты",
                                    tags$div(id = "key_div_corr_tables", style = "overflow-x: auto"))),
                fluidRow(hidden.box(class = "corr_box", width =  12, title = "Графики", collapsible = TRUE, collapsed = TRUE,
                                    tags$div(id = "key_div_corr_plots", style = "overflow-x: auto")))
        ),
        tabItem("reliability",
                hidden.box(
                  actionButton("ab_reliability", "α Кронбаха и др.")
                ),
                fluidRow(hidden.box(class = "reli_box", width =  12, title = "Результаты",
                                    tags$div(id = "key_div_reli_table", style = "overflow-x: auto")))
        ),
        tabItem("factor",
                hidden.box(
                  flowLayout(
                    selectInput("si_factoring_method", h5("Метод факторизации"), choices = list("Главные компоненты" = "pc", "Минимальные остатки" = "minres", "Наименьшие квадраты" = "uls", "Эмпирические наименьшие квадраты" = "ols", "Взвешенные наименьшие квадраты" = "wls", "Обобщённые взвешенные наименьшие квадраты" = "gls", "Главные оси" = "pa", "Максимальное сходство" = "ml", "Минимальный хи-квадрат" = "minchi", "Минимальный ранг" = "minrank", "Альфа Кайзера-Коффи" = "alpha")),
                    selectInput("si_factor_rotation", h5("Вращение матрицы"), choices = list("Нет" = "none", "Варимакс" = "varimax", "Квартимакс" = "quartimax", "Т Бентлера" = "bentlerT", "Эквамакс" = "equamax", "Варимин" = "varimin", "Т Геомин" = "geominT", "Двухфакторное" = "bifactor", "Промакс" = "Promax", "Облимин" = "oblimin", "Симплимакс" = "simplimax", "Q Бентлера" = "bentlerQ", "Q Геомин" = "geominQ", "Биквартимин" = "biquartimin", "Кластерное" = "cluster"), selected = "varimax"),
                    actionButton("ab_screeplot", "График осыпи"),
                    numericInput("factors_number", h5("Количество факторов"), value = 1, min = 1),
                    checkboxInput("cb_normalize", "Нормализация вращения", TRUE),
                    actionButton("ab_factoranalysis", "Факторный анализ")
                  )
                ),
                fluidRow(hidden.box(class = "fa_box_a", width =  12, title = "Основные результаты",
                                    tags$div(id = "key_div_fa_table", style = "overflow-x: auto"))),
                fluidRow(hidden.box(class = "fa_box_b", width =  12, title = "Подробности", collapsible = TRUE, collapsed = TRUE,
                                    tags$div(id = "key_div_fa_details", style = "overflow-x: auto"))),
                fluidRow(hidden.box(class = "fa_box_b", width =  12, title = "Графики", collapsible = TRUE, collapsed = TRUE,
                                    tags$div(id = "key_div_fa_plots", style = "overflow-x: auto")))
        ),
        tabItem("cluster",
                hidden.box(
                  actionButton("ab_dendro", "Дендрограмма и подбор количества"),
                  numericInput("clusters_number", h5("Количество кластеров"), value = 2, min = 2),
                  actionButton("ab_clustering", "Кластерный анализ")
                ),
                fluidRow(hidden.box(class = "clust_box", width =  12, title = "Результаты",
                                    tags$div(id = "key_div_clust_main", style = "overflow-x: auto"))),
                fluidRow(hidden.box(class = "clust_box", width =  12, title = "Графики", collapsible = TRUE, collapsed = TRUE,
                                    tags$div(id = "key_div_clust_plots", style = "overflow-x: auto")))
        ),
        tabItem("manova",
                hidden.box(
                  ds.picker("si_vars_manova", "Независимые переменные", TRUE),
                  actionButton("ab_manova", "Многофакторный дисперсионный анализ")
                ),
                fluidRow(hidden.box(class = "manova_box_a", width =  12, title = "Результаты",
                                    tags$div(id = "key_div_manova_tables", style = "overflow-x: auto"))),
                fluidRow(hidden.box(class = "manova_box_b", width =  12, title = "Графики", collapsible = TRUE, collapsed = TRUE,
                                    tags$div(id = "key_div_manova_plots", style = "overflow-x: auto")))
        ),
        tabItem("regression",
                hidden.box(
                  ds.picker("si_vars_regression", "Независимые переменные", TRUE),
                  prettyCheckbox("cb_optimal_glm", "Выбирать оптимальную модель"),
                  actionButton("ab_regression", "Обобщённая линейная регрессия")
                ),
                fluidRow(hidden.box(class = "regression_box", width =  12, title = "Результаты",
                                    tags$div(id = "key_div_regression_tables", style = "overflow-x: auto"))),
                fluidRow(hidden.box(class = "regression_box", width =  12, title = "Графики", collapsible = TRUE, collapsed = TRUE,
                                    tags$div(id = "key_div_regression_plots", style = "overflow-x: auto")))
        )
      )
    )
  )
)