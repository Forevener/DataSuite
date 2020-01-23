dashboardPagePlus(
  dashboardHeaderPlus(
    left_menu = tagList(
      dropdownBlock(
        id = "ddb_language",
        icon = "globe",
        badgeStatus = NULL,
        prettyRadioButtons(
          "rb_language",
          label = icon("language"),
          choices = list.files("./translations/help")
        )
      ),
      dropdownBlock(
        id = "ddb_settings",
        icon = "gear",
        badgeStatus = NULL,
        uiOutput("ui_settings")
      )
    ),
    enable_rightsidebar = TRUE,
    rightSidebarIcon = "question-circle",
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
    width = 400,
    .items = uiOutput("sdb_r_sidebar")
  ),
  dashboardFooter(left_text = actionLink("al_credits", glue("Mikhail Khukhrin, {format(Sys.time(), '%Y')}")))
)
