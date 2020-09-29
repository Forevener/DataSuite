ds_picker <- function(id, label = NULL, choices = NULL, selected = NULL, multiSelect = FALSE, actionsBox = FALSE, rightAlign = "auto", ...) {
  pickerInput(
    inputId = id,
    label = label,
    choices = choices,
    selected = selected,
    multiple = multiSelect,
    ...,
    options = list(
      `actions-box` = actionsBox,
      `dropdown-Align-Right` = rightAlign,
      `live-search` = TRUE,
      `live-Search-Placeholder` = i18n$t("Поиск переменной по имени..."),
      `select-All-Text` = i18n$t("Выбрать все"),
      `deselect-All-Text` = i18n$t("Убрать все"),
      `none-Selected-Text` = i18n$t("Ничего не выбрано"),
      `none-Results-Text` = i18n$t("Нет соответствий")
    )
  )
}

hidden_box <- function(..., class = "hidden_div", id = NULL) {
  hidden(
    div(
      id = id,
      class = class,
      box(...)
    )
  )
}

hidden_box_plus <- function(..., class = "hidden_div", id = NULL) {
  hidden(
    div(
      id = id,
      class = class,
      boxPlus(closable = FALSE, ...)
    )
  )
}
wide_box <- function(...) {
  fluidRow(
    box(
      width = 12,
      style = "overflow-x: auto",
      ...
    )
  )
}
