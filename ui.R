shinydashboardPlus::dashboardPage(
  header = shinydashboardPlus::dashboardHeader(
    title = "Data Suite"
  ),
  sidebar = shinydashboardPlus::dashboardSidebar(
    width = 270,
    uiOutput("sdb_l_sidebar")
  ),
  body = dashboardBody(
    useShinyjs(),
    #???# tags$head(tags$style(HTML(".controlbar { height: 90vh; overflow-y: auto; }"))),
    uiOutput("sdb_body")
  ),
  controlbar = shinydashboardPlus::dashboardControlbar(
    shinydashboardPlus::controlbarMenu(
      shinydashboardPlus::controlbarItem(
        icon("gear"),
        uiOutput("sdb_r_sidebar_settings")
      ),
      shinydashboardPlus::controlbarItem(
        icon("language"),
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
