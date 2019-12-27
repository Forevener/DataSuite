ds.picker = function(id, label = NULL, multiSelect = FALSE, actionsBox = FALSE)
{
	pickerInput(
		inputId = id,
		label = label,
		choices = NULL,
		selected = NULL,
		multiple = multiSelect,
		options = list(
			`actions-box` = actionsBox,
			`live-search` = TRUE,
			`live-Search-Placeholder` = "Поиск переменной по имени...",
			`select-All-Text` = "Выбрать все",
			`deselect-All-Text` = "Убрать все",
			`none-Selected-Text` = "Ничего не выбрано",
			`none-Results-Text` = "Нет соответствий"
		)
	)
}