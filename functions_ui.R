ds_picker <- function(id, label = NULL, multiSelect = FALSE, actionsBox = FALSE) {
  pickerInput(
    inputId = id,
    label = label,
    choices = NULL,
    selected = NULL,
    multiple = multiSelect,
    options = list(
      `actions-box` = actionsBox,
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
  hidden(div(id = id, class = class, box(...)))
}
