message_init = function() {
	.TMAP$messages = character()
	invisible(NULL)
}

message_thrown = function(id) {
	id %in% .TMAP$messages
}

message_reg = function(id) {
	.TMAP$messages = c(.TMAP$messages, id)
	NULL
}

message_comp_scale = function() {
	if (!message_thrown("comp_scale")) {
		cli::cli_inform(c(
			"{.field [plot mode]} fit legend/component: Some legend items or map compoments do not fit well, and are therefore rescaled.",
			i = "Set the tmap option {.code component.autoscale =  FALSE} to disable rescaling."
			),
			.frequency_id = "comp_scale"
		)
		message_reg("comp_scale")
	}
	NULL
}

message_comp_high_wide = function(stack) {
	if (!message_thrown("comp_scale")) {
		cli::cli_inform(c(
			"{.field [plot mode]} legend/component: Some components or legends are too {.val {ifelse(stack == 'vertical', 'high', 'wide')}} and are therefore rescaled.",
			"i" = "Set the tmap option {.code component.autoscale = FALSE} to disable rescaling."
			),
			.frequency_id = "comp_scale"
		)
		message_reg("comp_scale")
	}
	NULL
}

message_c4a = function(old_palette_name, info, fullname = FALSE) {
	new1 = info$fullname
	new2 = info$name
	mess = 	paste0("c4a_", old_palette_name)

	if (!message_thrown(mess)) {
		if (fullname) {
			cli::cli_inform(
				"{.field [cols4all]} color palettes: use palettes from the R package cols4all. Run {.code cols4all::c4a_gui()} to explore them. The old palette name {.str {old_palette_name}} is named {.str {new1}}",
				.frequency_id = "cols4all"
			)
		} else {
			cli::cli_inform(
				"{.field [cols4all]} color palettes: use palettes from the R package cols4all. Run {.code cols4all::c4a_gui()} to explore them. The old palette name {.str {old_palette_name}} is named {.str {new2}} (in long format {.str {new1}})",
				.frequency_id = "cols4all"
			)

		}
		message_reg(mess)
	}
}

message_nothing_to_show = function(any_groups) {
	if (any_groups) {
		cli::cli_inform("{.field [nothing to show]} no data layers defined after {.fn tm_shape}",
						.frequency_id = "nothing")
	} else {
		cli::cli_inform("{.field [nothing to show]} no data layers defined",
						.frequency_id = "nothing")
	}
	NULL
}

message_wrapstack = function(horizontal = TRUE) {
	if (horizontal) {
		cli::cli_inform("{.field [facets]} use {.fn tm_facets_hstack} instead of {.fn tm_facets_wrap} to put the legends next to and aligned with the facets.")
	} else {
		cli::cli_inform("{.field [facets]} use {.fn tm_facets_vstack} instead of {.fn tm_facets_wrap} to put the legends next to and aligned with the facets.")
	}
	NULL
}

message_pos_auto = function(type) {
	if (!message_thrown("pos_auto")) {
		fun = if (type == "autoout") "tm_pos_auto_out()" else "tm_pos_auto_in()"
		fun2 = if (type == "autoout") "tm_pos_out()" else "tm_pos_in()"
		cli::cli_inform("{.field [position]} use {.val {fun2}} instead of {.val {fun}}. The latter should be used with {.fn tmap_options}.")
		message_reg("pos_auto")
	}
	NULL
}

error_dimvalues = function() {
	cli::cli_abort(
		"{.val dimvalues} has been used, but the shape object does not contain any dimensions; please use {.arg x} to specify variables",
		call = call("tm_vars")
	)
}

