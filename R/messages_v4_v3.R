v3_only = function(fun) {
	setdiff(funs_v3[[fun]], funs_v4[[fun]])
}

v3_start_message = function() {
	if (!.TMAP$v3) {
		cli::cli_h1("tmap v3 code detected")
		.TMAP$v3 = TRUE
	}
	invisible(NULL)
}

v3_reset_flag = function() {
	.TMAP$v3 = FALSE
	invisible(NULL)
}


v3_use_component = function(arg = "title", comp = "tm_title", container = "tm_format") {
	v3_start_message()
	id = paste0(arg, comp, container)
	cli::cli_inform("{.field [v3->v4]} {.fn {container}}: use {.fn {comp}} instead of the {.arg {arg}} argument.", .frequency_id = id, .frequency = "always")
}

v3_title = function(fun) {
	id = paste0(fun, "title")
	cli::cli_inform("{.field [v3->v4]} {.fn {fun}}: use {.fn tm_title} instead of {.code {fun}(title = )}", .frequency_id = id, .frequency = "always")
}

v3_main_title = function(fun) {
	id = paste0(fun, "main.title")

	cli::cli_inform("{.field [v3->v4]} {.fn {fun}}: use {.fn tm_title} instead of {.code {fun}(main.title = )}", .frequency_id = id, .frequency = "always")
}

v3_convert2density = function(layer_fun) {
	id = paste0(layer_fun, "convert2density")
	cli::cli_inform(c(
		"{.field [v3->v4]} {.fn {layer_fun}} {.arg convert2density} is deprecated.",
		"i" = "Divide the variable values by the polygon areas manually (obtain the areas with 'sf::st_area()')."
	),
	.id = id,
	.frequency_id = "always"
	)
}

v3_tm_scale_instead_of_style = function(style, scale_fun, vv, layer_fun, arg_list) {
	m = paste0("scale_", scale_fun, "_vv_", vv, "style_", style)
	if (!message_thrown(m)) {
		if (scale_fun == "intervals") {
			arg_list$old = c("style", arg_list$old)
			arg_list$new = c("style", arg_list$new)
		}
		if (length(arg_list$old)) {
			x = if (arg_list$mult) {
				paste0("For small multiples, specify a 'tm_scale_' for each multiple, and put them in a list: '{vv}'.scale = list(<scale1>, <scale2>, ...)'")
			} else {
				NULL
			}
			al = do.call(paste, c(mapply(function(x,y) {
				if (x == y) paste0("'", x, "'") else paste0("'", x, "' (rename to '", y, "')")
			}, arg_list$old, arg_list$new, SIMPLIFY = FALSE), sep = ", "))
			xtra = paste0("Migrate the argument(s) ", al, " to 'tm_scale_", scale_fun, "(<HERE>)'")
		} else {
			xtra = NULL
			x = NULL
		}

		cli::cli_inform(c(
			"{.field [v3->v4]} {.fn {layer_fun}}: instead of {.code style = {.str {style}}}, use {vv}.scale = {.fn tm_scale_{scale_fun}}.",
			i = xtra,
			x
		))
		message_reg(m)
	}
	NULL
}

v3_tm_scale = function(scale_fun, vv, layer_fun, arg_list) {
	m = paste0("scale_", scale_fun, "_vv_", vv)

	if (!message_thrown(m)) {
		scale_fun = if (scale_fun == "") {
			"tm_scale"
		} else {
			paste0("tm_scale_", scale_fun)
		}
		if (length(arg_list$old)) {
			x = if (arg_list$mult) {
				paste0("For small multiples, specify a 'tm_scale_' for each multiple, and put them in a list: '", vv, ".scale = list(<scale1>, <scale2>, ...)'")
			} else {
				NULL
			}

			al = v3_list_text(olds = arg_list$old, news = arg_list$new)
			cli::cli_inform(c(
				"{.field [v3->v4]} {.fn tm_{layer_fun}}: migrate the argument(s) related to the scale of the visual variable {.var {vv}} namely {al} to {vv}.scale = {scale_fun}(<HERE>).",
				"i" = x
			))
		}
		message_reg(m)
	}
	NULL
}

v3_instead_message = function(arg_old, arg_new, fun) {
	v3_start_message()
	id <- paste0(fun, arg_old, arg_new)
	cli::cli_inform(c(
		"{.field [v3->v4]} {.fn {fun}}: use {.arg {arg_new}} instead of {.arg {arg_old}}."
	),
	.frequency_id = id,
	.frequency = "always"
	)
}

v3_instead = function(args_called, old, new, fun, extra_called = character()) {
	args = args_called$args
	called = args_called$called

	if (old %in% called) {
		args[[new]] = args[[old]]
		args[[old]] = NULL

		# may be needed to trigger something (e.g. "shape" to use symbols instead of dots)
		if (length(extra_called)) {
			called = unique(c(called, extra_called))
		}
		v3_instead_message(arg_old = old, arg_new = new, fun = fun)
	}
	list(args = args, called = called)
}

v3_instead_value = function(args_called, old, new, fun, value_old, value_new) {
	args = args_called$args
	called = args_called$called

	if (old %in% called) {
		if (identical(args[[old]], value_old)) {
			args[[old]] = NULL
			args[[new]] = value_new
			value_old = value_old %||% NULL
			cli::cli_inform(c(
				"{.field [v3->v4]} {.fn {fun}}: use {.code {new} = {value_new}} instead of {.code {old} = {value_old}}."
			))
			list(args = args, called = called)
		} else {
			v3_instead(args_called, old, new, fun)
		}
	} else {
		list(args = args, called = called)
	}
}

v3_list_init = function() {
	.TMAP$v3_list = list(old = character(), new = character(), mult = FALSE)
	invisible(NULL)
}
v3_list_impute_item = function(name, new_name, mult = FALSE) {
	.TMAP$v3_list$old = c(.TMAP$v3_list$old, name)
	.TMAP$v3_list$new = c(.TMAP$v3_list$new, new_name)
	.TMAP$v3_list$mult = .TMAP$v3_list$mult || mult
}
v3_list_get = function() {
	.TMAP$v3_list
}

v3_impute = function(args, name, value, new_name = name) {
	if (name %in% names(args)) {
		res = args[[name]]
		if (is.list(res) && !any(grepl("format", name, fixed = TRUE)) && !is.list(res[[1]])) {
			res = res[[1]]
			mult = TRUE
		} else if (new_name %in% c("title", "na.show", "orientation", "reverse", "interval.closure", "drop.levels", "midpoint", "as.count", "values.repeat", "values.scale", "value.na", "value.null", "value.neutral", "label.na", "label.null")) {
			res = res[1]
			mult = TRUE
		} else {
			mult = FALSE
		}
		v3_list_impute_item(name, new_name, mult)
		res
	} else value
}

v3_list_text = function(olds, news) {
	do.call(paste, c(mapply(function(x,y) {
		if (x == y) {
			paste0("'", x, "'")
		} else if (nchar(y) < 20) {
			paste0("'", x, "' (rename to '", y, "')")
		} else {
			paste0("'", x, "' (use '", y, "')")
		}
	}, olds , news, SIMPLIFY = FALSE), sep = ", "))
}

v3_tm_legend = function(fun, vv, arg_list) {
	if (length(arg_list$old)) {
		al = v3_list_text(olds = arg_list$old, news = arg_list$new)
		cli::cli_inform("{.field [v3->v4]} {.fn {fun}}: migrate the argument(s) related to the legend of the visual variable {.var {vv}} namely {al} to '{vv}.legend = tm_legend(<HERE>)'")
	}
	NULL
}

v3_tm_facets = function(arg_list) {
	if (length(arg_list$old)) {
		al = v3_list_text(olds = arg_list$old, news = arg_list$new)
		cli::cli_inform(c(
			"{.field [v3->v4]} {.fn tm_facets}: rename the following argument(s): {al}."
		),
		.frequency_id = "facets2",
		.frequency = "always"
		)
	}
	NULL
}

v3_tm_facets_free_scales = function() {
	cli::cli_inform(c(
		"{.field [v3->v4]} {.fn tm_facets}: migrate each  'free.scales.<X>' argument to the argument '<X>.free' of the corresponding layer function."
		),
		.frequency_id = "facets",
		.frequency = "always"
	)
}



v3_tm_legend_hide = function(fun, arg, vv) {
	cli::cli_inform(c(
		"{.field [v3->v4]} {.fn {fun}}: use {.code {vv}.legend = tm_legend_hide()} instead of {.code {arg} = FALSE}."
	),
	.frequency_id = "hide-legend",
	.frequency = "always"
	)
}



v3_tm_legend_general = function(fun) {
	v3_start_message()
	cli::cli_inform(c(
		"{.field [v3->v4]} {.fn {fun}}: use 'tm_legend()' inside a layer function, e.g. 'tm_polygons(..., fill.legend = tm_legend())'"
	),
	.frequency_id = "legend-general",
	.frequency = "regularly"
	)
}




v3_tm_chart_hist = function(layer_fun, vv, arg) {
	cli::cli_inform(c(
		"{.field [v3->v4]} {.fn {layer_fun}}: use {.code {vv}.chart = tm_chart_histogram()} instead of {.code {arg} = TRUE}."
	),
	.frequency_id = "chart-hist",
	.frequency = "regularly"
	)
}

v3_message_col_fill = function(layer_fun) {
	cli::cli_inform(c(
		"{.field [v3->v4]} {.fn {layer_fun}}: use 'fill' for the fill color of polygons/symbols (instead of 'col'), and 'col' for the outlines (instead of 'border.col')."
	),
	.frequency_id = "col-fill",
	.frequency = "regularly"
	)
}

v3_message_vv_null = function(layer_fun) {
	cli::cli_inform(c(
		"{.field [v3->v4]} {.fn {layer_fun}}: use {.val {NA}} instead of {.code NULL} to hide a visual variable."
	),
	.frequency_id = "hide-vv",
	.frequency = "regularly"
	)
}


v3_message_fill_alpha = function(layer_fun = layer_fun) {
	v3_instead_message("alpha", "fill_alpha", layer_fun)
}

v3_message_col_alpha = function(layer_fun = layer_fun, orig = "border.alpha") {
	v3_instead_message(arg_old = orig, arg_new = "col_alpha", layer_fun)
}

v3_add_legend = function(type, args) {
	newtype = c(fill = "polygons", symbol = "symbols", line = "lines")
	cli::cli_inform(
		"{.field [v3->v4]} {.fn tm_add_legend}:  use {.code type = {.val {newtype[type]}}} instead of {.code type = {.val {type}}}.",
		.frequency_id = "legend-type"
	)
	if ("col" %in% args && !c("fill" %in% args)) {
		cli::cli_inform(
			"{.field [v3->v4]} {.fn tm_add_legend}:  use {.arg fill} instead of {.arg col} for the fill color of {newtype[type]}.",
			.frequency_id = "legend-fill"
		)
	}
	if ("border.col" %in% args) {
		cli::cli_inform(
			"{.field [v3->v4]} {.fn tm_add_legend}:  use {.arg col} instead of {.arg border.col} for the outline color of {newtype[type]}.",
			.frequency_id = "legend-border-col"
		)
	}
	newtype[type]
}

v3_layer_opt = function(olds, news, layer_fun) {
	x = v3_list_text(olds, news)
	cli::cli_inform(
		"{.field [v3->v4]} {.fn {layer_fun}}: migrate the layer options {x} to 'options = opt_{layer_fun}(<HERE>)'",
		.frequency_id = "opt-tm"
	)
}


v3_tm_rgb = function(r, g, b, dim = NA) {
	if (is.na(dim)) {
		cli::cli_inform(
			"{.field [v3->v4]} {.fn tm_rgb}: instead of using r = {r}, g = {g} and b = {b} , please use col = tm_vars(c({r}, {g}, {b}), multivariate = TRUE)",
			.frequency_id = "tm-rgb"
		)
	} else {
		cli::cli_inform(
			"{.field [v3->v4]} {.fn tm_rgb}: instead of using r = {r}, g = {g} and b = {b} , please use col = tm_vars(dimvalues = c({r}, {g}, {b}), multivariate = TRUE)",
			.frequency_id = "tm-rgb"
		)
	}
}


v3_opt = function(fun, old, new) {
	cli::cli_inform(
		"{.field [v3->v4]} {.fn {fun}}: use {new} instead of {old}",
		.frequency_id = "tm-opt"
	)
}

v3_message_rgb_alpha = function(layer_fun) {
	cli::cli_inform(
		"{.field [v3->v4]} {.fn {layer_fun}}: use {.arg col_alpha} instead of {.arg alpha}",
		.frequency_id = "tm-rgb"
	)
}


v3_message_rgb_opt = function(layer_fun, opt, value) {
	cli::cli_inform(
		"{.field [v3->v4]} {.fn {layer_fun}}: use {.code options = opt_tm_rgb({opt} = {value})} instead of {.arg {opt} = {value}}",
		.frequency_id = "tm-rgb"
	)
}

v3_message_rgb_maxV = function(layer_fun, opt, value) {
	cli::cli_inform(
		"{.field [v3->v4]} {.fn {layer_fun}}: use {.code col.scale = tm_scale_rgb(max_color_value = {value})} instead of {.arg max.value = {value}}",
		.frequency_id = "tm-rgb"
	)
}

v3_rivers = function() {
	cli::cli_inform(
		"{.field [v3->v4]} tmap data: use {.code World_rivers} instead of {.code rivers}",
		.frequency_id = "rivers"
	)
}

v3_simplify = function(name, args) {
	names(args)[names(args) == "simplify"] = "fact"

	txt = paste(mapply(paste, names(args), unname(args), MoreArgs = list(sep = " = "), USE.NAMES = FALSE), collapse = ", ")

	cli::cli_inform(
		"{.field [v3->v4]} {.fn tm_shape}: {.arg simplify} cannot be used anymore. Please use {.code tmaptools::simplify_shape({name}, {txt})} instead",
		.frequency_id = "simplify"
	)
}
