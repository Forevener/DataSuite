shinydashboardPlus::dashboardPage(
  header = shinydashboardPlus::dashboardHeader(
    title = "Data Suite",
    leftUi = tagList(
      tags$li(
        class="dropdown",
        id="header_button_donate",
        tags$a(
          icon("hand-holding-heart"),
          "Поддержать проект",
          href="https://yoomoney.ru/to/410013947982843",
          class="p-10",
          target="_blank",
          rel="noreferrer noopener"
        )
      )
    )
  ),
  sidebar = shinydashboardPlus::dashboardSidebar(
    width = 270,
    uiOutput("sdb_l_sidebar")
  ),
  body = dashboardBody(
    useShinyjs(),
    uiOutput("sdb_body")
  ),
  controlbar = shinydashboardPlus::dashboardControlbar(
    shinydashboardPlus::controlbarMenu(
      shinydashboardPlus::controlbarItem(
        icon("gear"),
        uiOutput("sdb_r_sidebar_settings")
      ),
      shinydashboardPlus::controlbarItem(
        icon("globe"),
        uiOutput("sdb_r_sidebar_language")
      )
    ),
    width = 300
  ),
  footer = shinydashboardPlus::dashboardFooter(
    left = actionLink("al_credits", glue("Mikhail Khukhrin, 2019 - {format(Sys.time(), '%Y')}")),
    right = actionLink("al_help", label = NULL, icon = icon("question-circle"))
  )
)
