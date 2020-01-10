dashboardPagePlus(
  dashboardHeaderPlus(
    left_menu = tagList(
      dropdownBlock(
        id = "dd_language",
        icon = "globe",
        badgeStatus = NULL,
        prettyRadioButtons(
          "rb_language",
          label = NULL,
          choices = c("English", "Russian"),
          selected = NULL
        )
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
    extendShinyjs("www/ds-cookies.js"),
    uiOutput("sdb_body")
  ),
  rightSidebar(
    .items = uiOutput("sdb_r_sidebar")
  )
)
