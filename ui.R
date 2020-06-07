dashboardPagePlus(
  dashboardHeaderPlus(
    enable_rightsidebar = TRUE,
    rightSidebarIcon = "gear",
    title = "Data Suite"
  ),
  dashboardSidebar(
    width = 270,
    uiOutput("sdb_l_sidebar")
  ),
  dashboardBody(
    useShinyjs(),
    tags$head(tags$style(HTML(".controlbar { height: 90vh; overflow-y: auto; }"))),
    uiOutput("sdb_body")
  ),
  rightSidebar(
    width = 300,
    .items = uiOutput("sdb_r_sidebar")
  ),
  dashboardFooter(
    left_text = actionLink("al_credits", glue("Mikhail Khukhrin, {format(Sys.time(), '%Y')}")),
    right_text = actionLink("al_help", label = NULL, icon = icon("question-circle"))
  )
)
