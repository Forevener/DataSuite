dashboardPagePlus(
  dashboardHeaderPlus(
    left_menu = tagList(
      dropdownBlock(
        id = "dd_language",
        icon = "globe",
        badgeStatus = NULL,
        prettyRadioButtons(
          "rb_language",
          NULL,
          choices = c("English", "Russian")
        )
      )
    ),
    title = "Data Suite"
  ),
  dashboardSidebar(
    width = 270,
    uiOutput("sdb_sidebar")
  ),
  dashboardBody(
    useShinyjs(),
    uiOutput("sdb_body")
  )
)
