library(shiny)
library(rhandsontable)

shinyUI(
  fluidPage(
    tags$head(
      tags$style(HTML("hr {border-top: 1px solid #000000;}")),
    ),
  titlePanel("Анализ данных"),

  sidebarLayout(
    sidebarPanel(
      radioButtons("radio", h5("Кодировка файла"),choices = list("UTF-8" = "UTF-8", "Windows 1251" = "windows-1251", "Windows 1252" = "windows-1252"), selected = "UTF-8"),
      helpText("Если текст в таблицах отображается некорректно, выберите другую кодировку и загрузите файл данных заново"),
      fileInput("upload", label = "Загрузить файл данных"),
      hr(),
      helpText("Описательная статистика:"),
      actionButton("dp", "Параметрическая"),
      actionButton("dnp", "Непараметрическая"),
      actionButton("ft", "Частоты встречаемости"),
      actionButton("distplots", "Графики распределения"),
      hr(),
      helpText("Проверка распределения:"),
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
      selectInput("vars_manova", h5("Независимые переменные"), choices = list("-" = 0), multiple = TRUE, selected = "-"),
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
      selectInput("cl1_dropdown", h5("Строки матрицы"), choices = list("-" = 0), multiple = TRUE, selected = "-"),
      selectInput("cl2_dropdown", h5("Столбцы матрицы"), choices = list("-" = 0), multiple = TRUE, selected = "-"),
      actionButton("cp", "r-критерий Пирсона"),
      actionButton("cs", "ρ-критерий Спирмена")
    ),

    mainPanel(
      tabsetPanel(id = "mainTabs",
        tabPanel(title = "Входные данные", value = "Tab1", rHandsontableOutput("in_table")),
        tabPanel(title = "Выходные данные", value = "Tab2", tableOutput("out_table")),
        tabPanel(title = "Дополнительные таблицы", value = "Tab3", tags$div(id = 'tab3bottom')),
        tabPanel(title = "Графики", value = "Tab4", tags$div(id = 'tab4bottom'))
      )
    )
  )
)
)