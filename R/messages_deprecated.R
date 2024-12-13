v3_tm_format = function(format, code) {
	id = "tm_format"
	ct = if (code == "") {
		paste0("Moreover, the format \"", format, "\" is unknown (also in tmap < 4.0)")
	} else {
		paste0("Instead, please use `", code, "`")
	}

	cli::cli_inform(c(
		"{.field [deprecated]} {.code tm_format(\"{format}\")} is deprecated as of tmap 4.0. {ct}"
	),
	.frequency_id = id,
	.frequency = "always"
	)
}

v3_tmap_format = function(format) {
	id = "tmap_format"

	ct = if (format %in% c("World", "World_wide", "NLD", "NLD_wide")) {
		paste0("Moreover, the format \"", format, "\" is unknown (also in tmap < 4.0)")
	} else {
		""
	}

	cli::cli_inform(c(
		"{.field [deprecated]} {.code tmap_format({.str {format}})} is deprecated as of tmap 4.0. {ct}"
	),
	.frequency_id = id,
	.frequency = "always"
	)
}

v3_tmap_format_add = function(format) {
	id = "tmap_format_add"
	cli::cli_inform(c(
		"{.field [deprecated]} {.fn tmap_format_add} is deprecated as of tmap 4.0. Please use {.code tm_layout(...)} instead, or create a style with {.fn tmap_options_save}"
	),
	.frequency_id = id,
	.frequency = "always"
	)
}
