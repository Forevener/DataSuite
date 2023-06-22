# TODO: Citations for each analysis/plot

output$sdb_l_sidebar <- renderUI({
  sidebarMenu(
    id = "sidebar_tabs",
    menuItem(i18n$t("Ввод и отбор данных"), tabName = "data_upload"),
    menuItem(i18n$t("Распределение"), tabName = "distribution"),
    menuItem(i18n$t("Описательная статистика"), tabName = "descriptive"),
    menuItem(i18n$t("Сравнение независимых выборок"), tabName = "comparison_IS"),
    menuItem(i18n$t("Сравнение зависимых выборок"), tabName = "comparison_DS"),
    menuItem(i18n$t("Корреляционный анализ"), tabName = "correlations"),
    menuItem(i18n$t("Надёжность и согласованность"), tabName = "reliability"),
    menuItem(i18n$t("Факторный анализ"), tabName = "factor"),
    menuItem(i18n$t("Кластерный анализ"), tabName = "cluster"),
    menuItem(i18n$t("МФ-дисперсионный анализ"), tabName = "manova"),
    menuItem(i18n$t("Регрессионный анализ"), tabName = "regression"),
    menuItem(i18n$t("Анализ мощности"), tabName = "power"),
    uiOutput("additionalItems")
  )
})

output$sdb_body <- renderUI({
  tabItems(
    tabItem(
      "data_upload",
      box(
        selectInput("file_encoding", h5(i18n$t("Кодировка файла")), choices = list(i18n$t("Автовыбор"), "UTF-8 / Unicode", "windows-1251 / CP1251", "windows-1252 / CP1252", "ISO-8859-1 / Latin1") %isnameof% list("NULL", "UTF-8", "windows-1251", "windows-1252", "ISO-8859-1")),
        helpText(i18n$t("Если текст в таблицах отображается некорректно, выберите другую кодировку и загрузите файл данных заново")),
        fileInput("upload", label = i18n$t("Загрузить файл данных"), buttonLabel = i18n$t("Обзор"), placeholder = i18n$t("Файл не выбран"), accept = c(".xlsx", ".xls", ".tsv", ".csv", ".ods", ".dta", ".por", ".sas", ".sav", ".zsav", ".xpt"))
      ),
      box(
        helpText(i18n$t("Если требуется исключить некоторые переменные из анализа (например, группирующие переменные при сравнении зависимых выборок), укажите их в следующем списке")),
        ds_picker("si_include_vars", i18n$t("Выбор переменных для анализа"), multiSelect = TRUE, actionsBox = TRUE),
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
      "distribution",
      hidden_box(
        title = i18n$t('Таблицы частот'),
        width = 4,
        actionButton("ab_frequency_tables", i18n$t("Частоты встречаемости"))
      ),
      hidden_box(
        title = i18n$t('Графики распределения'),
        width = 4,
        dropdownButton(
          icon = icon("wrench"),
          circle = FALSE,
          inline = TRUE,
          inputId = "ddb_distplot_type",
          radioButtons("rb_distplot_num", i18n$t("Тип графиков для числовых переменных"), choices = list("Гистограмма", "Полосный", "Плотность распределения", "Кумулятивный", "Полигональный", "Ящик с усами", "Скрипичный", "Точечный") %isnameof% list("hist", "bar", "density", "ecdf", "poly", "bw", "viol", "dot")),
          radioButtons("rb_distplot_cat", i18n$t("Тип графиков для нечисловых переменных"), choices = list("Полосный", "Круговой") %isnameof% list("bar", "pie"))
        ),
        actionButton("ab_distplots", i18n$t("Графики распределения"))
      ),
      hidden_box(
        title = i18n$t('Проверка нормальности'),
        width = 4,
        actionButton("ab_kolmogorovsmirnov_normality", i18n$t("Критерий Колмогорова-Смирнова")),
        actionButton("ab_lilliefors", i18n$t("Критерий Лиллиефорса")),
        actionButton("ab_shapirowilk", i18n$t("Критерий Шапиро-Уилка"))
      ),
      uiOutput("results_distribution")
    ),
    tabItem(
      "descriptive",
      hidden_box(
        actionButton("ab_parametric_desc", i18n$t("Параметрическая")),
        actionButton("ab_nonparametric_desc", i18n$t("Непараметрическая")),
        dropdownButton(
          icon = icon("wrench"),
          circle = FALSE,
          inline = TRUE,
          inputId = "ddb_custom_desc",
          tagList(
            checkboxGroupInput(
              inputId = "cbg_custom_desc",
              label = NULL,
              choices = list(
                i18n$t("Крайние значения"),
                i18n$t("Размах"),
                i18n$t("Среднее"),
                i18n$t("Стандартная ошибка среднего"),
                i18n$t("Среднее абсолютное отклонение"),
                i18n$t("Дисперсия"),
                i18n$t("Стандартное отклонение"),
                i18n$t("Коридор нормы"),
                i18n$t("Доверительный интервал"),
                i18n$t("Мода"),
                i18n$t("Медиана"),
                i18n$t("Квартили"),
                i18n$t("Квартильный размах"),
                i18n$t("Эксцесс"),
                i18n$t("Асимметрия")
              )
            ),
            actionButton("ab_custom_desc", i18n$t("Рассчитать"))
          )
        )
      ),
      uiOutput("results_descriptive")
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
        actionButton("ab_welch", i18n$t("F-критерий Уэлча")),
        actionButton("ab_chi_square", i18n$t("Критерий хи-квадрат"))
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
        ds_picker("si_var1_corr", i18n$t("Строки матрицы"), multiSelect = TRUE, actionsBox = TRUE),
        ds_picker("si_var2_corr", i18n$t("Столбцы матрицы"), multiSelect = TRUE, actionsBox = TRUE),
        ds_picker("si_adj_corr", i18n$t("Поправка на множественные измерения"), choices = list(i18n$t("Холм"), i18n$t("Хохберг"), i18n$t("Хоммель"), i18n$t("Бонферрони"), i18n$t("Бенджамини-Хохберг"), i18n$t("Бенджамини-Иекутиели"), i18n$t("Нет")) %isnameof% list("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "none")),
        actionButton("ab_cor_pearson", i18n$t("r-критерий Пирсона")),
        actionButton("ab_cor_kendall", i18n$t("тау-критерий Кендалла")),
        actionButton("ab_cor_spearman", i18n$t("ро-критерий Спирмена")),
        actionButton("ab_cor_plots", i18n$t("Графики разброса"))
      ),
      fluidRow(hidden_box(
        class = "corr_box_tables", width = 12, title = i18n$t("Результаты"),
        tags$div(id = "key_div_corr_tables", style = "overflow-x: auto")
      )),
      fluidRow(hidden_box(
        class = "corr_box_plots", width = 12, title = i18n$t("Графики"),
        tags$div(id = "key_div_corr_plots", style = "overflow-x: auto")
      ))
    ),
    tabItem(
      "reliability",
      hidden_box(
        ds_picker("si_reli_vars", i18n$t("Выбор переменных для анализа"), multiSelect = TRUE, actionsBox = TRUE),
        ds_picker("si_reli_reversed_items", i18n$t("Обратные пункты/шкалы"), multiSelect = TRUE, actionsBox = TRUE),
        actionButton("ab_reliability", i18n$t("Альфа Кронбаха и др."))
      ),
      fluidRow(hidden_box(
        class = "reli_box", width = 12, title = i18n$t("Результаты"),
        tags$div(id = "key_div_reli_table", style = "overflow-x: auto")
      )),
      fluidRow(hidden_box(
        class = "reli_box", width = 12, title = i18n$t("Подробности"), collapsible = TRUE, collapsed = TRUE,
        tags$div(id = "key_div_reli_details", style = "overflow-x: auto")
      ))
    ),
    tabItem(
      "factor",
      hidden_box(
        flowLayout(
          selectInput("si_factoring_method", h5(i18n$t("Метод факторизации")), choices = list(i18n$t("Главные компоненты"), i18n$t("Минимальные остатки"), i18n$t("Наименьшие квадраты"), i18n$t("Эмпирические наименьшие квадраты"), i18n$t("Взвешенные наименьшие квадраты"), i18n$t("Обобщённые взвешенные наименьшие квадраты"), i18n$t("Главные оси"), i18n$t("Максимальное сходство"), i18n$t("Минимальный хи-квадрат"), i18n$t("Минимальный ранг"), i18n$t("Альфа Кайзера-Коффи")) %isnameof% list("pc", "minres", "uls", "ols", "wls", "gls", "pa", "ml", "minchi", "minrank", "alpha")),
          selectInput("si_factor_rotation", h5(i18n$t("Вращение матрицы")), choices = list(i18n$t("Нет"), i18n$t("Варимакс"), i18n$t("Квартимакс"), i18n$t("Т Бентлера"), i18n$t("Эквамакс"), i18n$t("Варимин"), i18n$t("Т Геомин"), i18n$t("Двухфакторное"), i18n$t("Промакс"), i18n$t("Облимин"), i18n$t("Симплимакс"), i18n$t("Q Бентлера"), i18n$t("Q Геомин"), i18n$t("Биквартимин"), i18n$t("Кластерное")) %isnameof% list("none", "varimax", "quartimax", "bentlerT", "equamax", "varimin", "geominT", "bifactor", "Promax", "oblimin", "simplimax", "bentlerQ", "geominQ", "biquartimin", "cluster"), selected = "varimax"),
          actionButton("ab_screeplot", i18n$t("Подбор количества факторов")),
          numericInput("factors_number", h5(i18n$t("Количество факторов")), value = 1, min = 1),
          checkboxInput("cb_normalize", i18n$t("Нормализация вращения"), TRUE),
          actionButton("ab_factoranalysis", i18n$t("Факторный анализ"))
        )
      ),
      hidden_box(
        flowLayout(
          sliderInput("sli_fa_cut", i18n$t("Отсекать нагрузки"), min = 0, max = 1, step = 0.01, value = isolate(settings()$fa_cut)),
          sliderInput("sli_fa_load", i18n$t("Выделять нагрузки"), min = 0, max = 1, step = 0.01, value = isolate(settings()$fa_load))
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
        ds_picker("si_vars_manova", i18n$t("Независимые переменные"), multiSelect = TRUE, actionsBox = TRUE),
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
        ds_picker("si_dep_vars_regression", i18n$t("Зависимые переменные"), multiSelect = TRUE, actionsBox = TRUE),
        ds_picker("si_ind_vars_regression", i18n$t("Независимые переменные"), multiSelect = TRUE, actionsBox = TRUE),
        actionButton("ab_optimalglms", i18n$t("Оптимальные модели")),
        actionButton("ab_glm", i18n$t("Обобщённая линейная регрессия"))
      ),
      fluidRow(hidden_box(
        class = "regression_box_a", width = 12, title = i18n$t("Результаты"),
        tags$div(id = "key_div_regression_tables", style = "overflow-x: auto")
      )),
      fluidRow(hidden_box(
        class = "regression_box_b", width = 12, title = i18n$t("Графики"), collapsible = TRUE, collapsed = TRUE,
        tags$div(id = "key_div_regression_plots", style = "overflow-x: auto")
      ))
    ),
    tabItem(
      "power",
      box(
        width = 12,
        selectInput("si_pwr_type", i18n$t("Тип критерия"), choices = list(
          i18n$t("Пропорция"),
          paste(i18n$t("Две пропорции"), i18n$t("(одинаковые размеры выборок)")),
          paste(i18n$t("Две пропорции"), i18n$t("(различные размеры выборок)")),
          paste(i18n$t("t-критерий"), i18n$t("(одинаковые размеры выборок)")),
          paste(i18n$t("t-критерий"), i18n$t("(различные размеры выборок)")),
          i18n$t("Однофакторный дисперсионный анализ"),
          i18n$t("Хи-квадрат"),
          i18n$t("Корреляционный анализ"),
          i18n$t("Обобщённая линейная модель")
        ) %isnameof% list(
          "p", "2p", "2p2n", "t", "t2n", "anova", "chisq", "r", "f2"
        )),
        column(
          4,
          sliderInput("sl_pwr_power", i18n$t("Мощность"), min = 0.8, max = 1, value = 0.8, step = 0.01),
          sliderInput("sl_pwr_p_value", "p", min = 0, max = 0.05, value = 0.05, step = 0.0001),
          sliderInput("sl_pwr_sample_size", i18n$t("Размер выборки"), min = 0, max = 1500, value = 30, step = 1),
          sliderInput("sl_pwr_es", i18n$t("Величина эффекта"), min = 0, max = 1, value = 0.8, step = 0.01)
        ),
        column(
          8,
          tableOutput("to_pwr_output")
        )
      )
    )
  )
})

output$sdb_r_sidebar_settings <- renderUI({
  tagList(
    tags$div(tags$h4(i18n$t("Настройки")), style = "text-align: center"),
    tags$br(),
    numericInput(
      "ni_p_level",
      i18n$t("Значимый p-уровень"),
      isolate(settings()$p),
      min = 0,
      max = 1,
      step = 0.01
    ),
    tags$br(),
    actionButton("ab_by_group", i18n$t("Выбрать группирующие переменные")),
    hidden(
      tags$div(
        id = "panel_by_group",
        panel(
          style = "height:300px; overflow-y: auto",
          checkboxGroupInput(
            "cbg_by_group",
            label = NULL
          )
        )
      )
    )
  )
})

output$sdb_r_sidebar_language <- renderUI({
  radioGroupButtons(
    "si_language",
    #choiceNames = t(i18n$get_translations()["Русский", ]),
    #choiceValues = i18n$get_languages(),
    choiceNames = c("English", "Русский"),
    choiceValues = c("en", "ru"),
    selected = get_current_language(),
    direction = "vertical",
    checkIcon = list(
      yes = icon("check")
    ),
    justified = TRUE
  )
})
