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
				"{.field [cols4all]} color palettes: use palettes from the R package cols4all. Run {.run cols4all::c4a_gui()} to explore them. The old palette name {.str {old_palette_name}} is named {.val {new1}}",
				.frequency_id = "cols4all"
			)
		} else {
			cli::cli_inform(
				"{.field [cols4all]} color palettes: use palettes from the R package cols4all. Run {.run cols4all::c4a_gui()} to explore them. The old palette name {.str {old_palette_name}} is named {.str {new2}} (in long format {.str {new1}})",
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
		fun = if (type == "autoout") "tm_pos_auto_out" else "tm_pos_auto_in"
		fun2 = if (type == "autoout") "tm_pos_out" else "tm_pos_in"
		cli::cli_inform("{.field [position]} use {.fn {fun2}} instead of {.fn {fun}}. The latter should be used with {.fn tmap_options}.")
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

message_webgl_vars = function(supported, vary) {
	sup_text = paste(supported, collapse = ", ")
	var_text = paste(names(vary)[vary], collapse = ", ")
	var_sel = names(vary)[vary]

	cli::cli_inform("{.field [view mode]} WebGL enabled, but the only supported visual variables are: {.val {supported}}. The visual variable(s) {.val {var_sel}} are not supported. Set {.code use_WebGL = FALSE} to support them.")

}

message_webgl_hover = function(type) {
	cli::cli_inform("{.field [view mode]} WegGL enabled, but it does not support hover labels for layer type {.val {type}}. Set {.code use_WebGL = FALSE} to support them.",
					.frequency_id = "webGL_hover",
					.frequency = "once")
}


message_webgl_checks = function(checks, checkif) {
	vals = paste(paste(names(checkif)[!checks], checkif[!checks], sep = " = "), collapse = ", ")
	cli::cli_inform("{.field [view mode]} WebGL enabled, but the following visual variable only accept one value {.arg {vals}}. Set {.code use_WebGL = FALSE} to support them.",
					.frequency_id = "webGL_vv",
					.frequency = "once")
}

message_webgl_crs_simple = function() {
	cli::cli_inform("{.field [view mode]} WebGL does not work (yet) with projected map projections, so it has been disabled.",
					.frequency_id = "webGL_crs",
					.frequency = "once")

}


message_qtm_empty = function() {
	cli::cli_inform(
		"{.field [qtm()]} nothing to show. Either specify {.arg shp} or {.arg basemap}. Alternatively, switch to view mode with {.code tmap_mode(\"view\")}"
	)
}

message_layer_unused_args = function(layer_fun, args) {
	cli::cli_inform(
		"{.field [{layer_fun}()]} Argument{ifelse(length(args)>1, 's', '')} {.arg {args}} unknown."
	)
}

message_crs_property_unknown = function() {
	cli::cli_inform(
		"{.field [shiny]} some css styling (used for background and markers) cannot be used yet in shiny apps",
		.frequency_id = "shiny_css",
		.frequency = "once")
}

message_crs_property_not_used = function() {
	cli::cli_inform(
		"{.field [tm_crs()]} {.arg crs} specified, so {.arg property} is ignored"
	)
}

message_crs_ll = function() {
	cli::cli_inform(
		"{.field [tip]} Consider a suitable map projection, e.g. by adding {.code + tm_crs({.str auto})}.",
		.frequency_id = "crs",
		.frequency = "once"
	)
}

message_scale_interval_value0 = function(aes, values, layer) {
	suggestion = if (inherits(values, "tmapSeq")) {
		to = values$to
		power = values$power
		if (is.character(power)) power = paste0("\"", power, "\"")
		paste0("values = tm_seq(0.25, ", to, ", power = ", power, ")")
	} else {
		to = values[2]
		paste0("values = c(0.25, ", to, ")")
	}


	cli::cli_inform(
		"{.field [layer {layer}, tm_scale_intervals()]} By default the value of the visual variable {.code {aes}} for the first interval is 0. Consider {.code {suggestion}} (or another lower bound number than 0.25). Or alternatively, use {.fun tm_scale_continuous}.",
		.frequency_id = "size0"
	)
}

message_shiny_css = function() {
	cli::cli_inform(
		"{.field [layer {layer}, tm_scale_intervals()]} By default the value of the visual variable {.code {aes}} for the first interval is 0. Consider {.code {suggestion}} (or another lower bound number than 0.25). Or alternatively, use {.fun tm_scale_continuous}.",
		.frequency_id = "size0"
	)
}

message_basemaps_none = function(serv, z) {
	cli::cli_inform(
		"{.field [basemaps]} Tiles from {.str {serv}} at zoom level {z} couldn't be loaded",
		.frequency_id = "basemap_none",
		.frequency = "once"
	)

}

message_basemaps_blurry = function(serv) {
	cli::cli_inform(
		"{.field [basemaps]} Tiles from {.str {serv}} will be projected so details (e.g. text) could appear blurry",
		.frequency_id = "basemap_blurry",
		.frequency = "once"
	)
}


message_basemaps = function(is_stadia) {
	provider = ifelse(is_stadia, "Stadia", "Thunderforest")
	cli::cli_inform(
		"{.field [basemaps]} An API key is requierd for tiles from {.str {provider}}",
		.frequency_id = "basemap_API",
		.frequency = "once"
	)
}
