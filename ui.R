library(shiny)
library(rhandsontable)

shinyUI(
  fluidPage(
    tags$head(
      tags$style(HTML("hr {border-top: 1px solid #000000;}"))
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
      actionButton("ff", "Однофакторный дисперсионный анализ")
    ),
    
    mainPanel(
      tabsetPanel(id = "mainTabs",
        tabPanel(title = "Входные данные", value = "Tab1", rHandsontableOutput("in_table")),
        tabPanel(title = "Выходные данные", value = "Tab2", tableOutput("out_table"))
      )
    )
  )
)
)