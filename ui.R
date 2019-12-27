shinyUI(
  navbarPage(
    id = "navbar",
    title = "Анализ данных",
    header = tags$head(
      tags$style(HTML("hr {border-top: 1px solid #000000;}")),
    ),
    tabPanel(
      "Ввод и фильтрация данных",
      inputPanel(
        radioButtons("radio", h5("Кодировка файла"), choices = list("UTF-8" = "UTF-8", "Windows 1251" = "windows-1251", "Windows 1252" = "windows-1252"), selected = "UTF-8"),
        helpText("Если текст в таблицах отображается некорректно, выберите другую кодировку и загрузите файл данных заново"),
        fileInput("upload", label = "Загрузить файл данных")
      ),
      inputPanel(
        helpText("Если требуется исключить некоторые переменные из анализа (например, группирующие переменные при сравнении зависимых выборок), укажите их в следующем списке"),
        ds.picker("include_vars", "Выбор переменных для анализа", TRUE, TRUE),
        verbatimTextOutput("data_info")
      ),
      DTOutput("in_table")
    ),

    tabPanel(
      "Обработка данных",
      sidebarLayout(
        sidebarPanel(
          helpText("Описательная статистика:"),
          actionButton("dp", "Параметрическая"),
          actionButton("dnp", "Непараметрическая"),
          actionButton("ft", "Частоты встречаемости"),
          hr(),
          helpText("Проверка распределения:"),
          actionButton("distplots", "Графики распределения"),
          actionButton("sw", "Критерий Шапиро-Уилка"),
          hr(),
          helpText("Сравнение двух независимых выборок:"),
          selectInput("ctis_dropdown", h5("Независимая переменная"), choices = list("-" = 0), selected = "-"),
          actionButton("mw", "U-критерий Манна-Уитни"),
          actionButton("ti", "t-критерий Стьюдента"),
          hr(),
          helpText("Сравнение нескольких независимых выборок:"),
          selectInput("cmis_dropdown", h5("Независимая переменная"), choices = list("-" = 0), selected = "-"),
          actionButton("kw", "H-критерий Краскела-Уоллиса"),
          actionButton("ff", "Однофакторный дисперсионный анализ"),
          hr(),
          helpText("Сравнение независимых выборок по нескольким критериям:"),
          ds.picker("vars_manova", "Независимые переменные", TRUE),
          actionButton("mav", "Многофакторный дисперсионный анализ"),
          hr(),
          helpText("Сравнение двух зависимых выборок:"),
          actionButton("td", "t-критерий Стьюдента"),
          actionButton("st", "Критерий знаков"),
          actionButton("wmp", "W-критерий Уилкоксона"),
          hr(),
          helpText("Сравнение нескольких зависимых выборок:"),
          numericInput("measures_number", h5("Количество замеров"), value = 3, min = 3),
          actionButton("fd", "Критерий Фридмана"),
          actionButton("rma", "Анализ повторяющихся наблюдений"),
          hr(),
          helpText("Выявление взаимосвязи:"),
          ds.picker("cl1_dropdown", "Строки матрицы", TRUE),
          ds.picker("cl2_dropdown", "Столбцы матрицы", TRUE),
          actionButton("cp", "r-критерий Пирсона"),
          actionButton("cs", "ρ-критерий Спирмена"),
          hr(),
          helpText("Сокращение размерности:"),
          actionButton("sp", "График осыпи"),
          numericInput("factors_number", h5("Количество факторов"), value = 1, min = 1),
          actionButton("fa", "Факторный анализ"),
          hr(),
          helpText("Группировка испытуемых:"),
          actionButton("hc", "Дендрограмма и подбор количества"),
          numericInput("clusters_number", h5("Количество кластеров"), value = 2, min = 2),
          actionButton("ca", "Кластерный анализ")
        ),

        mainPanel(
          tabsetPanel(
            id = "mainTabs",
            tabPanel(title = "Выходные данные", value = "Tab2", tableOutput("out_table")),
            tabPanel(title = "Дополнительные таблицы", value = "Tab3", tags$div(id = 'tab3bottom')),
            tabPanel(title = "Графики", value = "Tab4", tags$div(id = 'tab4bottom'))
          )
        )
      )
    )
  )
)