v3_only = function(fun) {
	setdiff(funs_v3[[fun]], funs_v4[[fun]])
}

v3_start_message = function() {
	if (!.TMAP$v3) {
		message("-- tmap v3 code detected --")
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
	message(paste0("[v3->v4] ", container, "(): use ", comp, "() instead of the argument '", arg, "'"))
}

v3_title = function(fun) {
	message("[v3->v4] ", fun, "(): use 'tm_title()' instead of the 'title' argument of '", fun, "'")	
}

v3_main_title = function(fun) {
	message("[v3->v4] ", fun, "(): use 'tm_title()' instead of the 'main.title' argument of '", fun, "'")	
}

v3_convert2density = function(layer_fun) {
	message(paste0("[v3->v4] ", layer_fun, "(): convert2density is deprecated: divide the variable values by the polygon areas manually (obtain the areas with 'sf::st_area()')"))
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
				paste0(". For small multiples, specify a 'tm_scale_' for each multiple, and put them in a list: '", vv, ".scale = list(<scale1>, <scale2>, ...)'")
			} else {
				""
			}
			al = do.call(paste, c(mapply(function(x,y) {
				if (x == y) paste0("'", x, "'") else paste0("'", x, "' (rename to '", y, "')")
			}, arg_list$old, arg_list$new, SIMPLIFY = FALSE), sep = ", "))
			xtra = paste0(" and migrate the argument(s) ", al, " to 'tm_scale_", scale_fun, "(<HERE>)'")
		} else {
			xtra = ""
			x = ""
		}
		
		message(paste0("[v3->v4] ", layer_fun, "(): instead of 'style = \"", style, "\"', use '", vv, ".scale = tm_scale_", scale_fun, "()'", xtra, x))
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
				paste0(". For small multiples, specify a 'tm_scale_' for each multiple, and put them in a list: '", vv, ".scale = list(<scale1>, <scale2>, ...)'")
			} else {
				""
			}
			
			al = v3_list_text(olds = arg_list$old, news = arg_list$new)
			message("[v3->v4] ", layer_fun, "(): migrate the argument(s) related to the scale of the visual variable '", vv, "', namely ", al, " to '", vv, ".scale = ", scale_fun, "(<HERE>)'", x)
		}
		message_reg(m)
	}
	NULL
}





v3_instead_message = function(arg_old, arg_new, fun) {
	v3_start_message()
	message(paste0("[v3->v4] ", fun, "(): use '", arg_new, "' instead of '", arg_old, "'"))
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
		message(paste0("[v3->v4] ", fun, "(): use '", new, "' instead of '", old, "'"))
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
			if (is.null(value_old)) value_old = "NULL" 
			message(paste0("[v3->v4] ", fun, "(): use '", new, " = ", value_new, "' instead of '", old, " = ", value_old, "'"))
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
		if (is.list(res) && length(grep("format", name, fixed =TRUE)) == 0 && !is.list(res[[1]])) {
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
		message(paste0("[v3->v4] ", fun, "(): migrate the argument(s) related to the legend of the visual variable '", vv, "', namely ", al, " to '", vv, ".legend = tm_legend(<HERE>)'"))
	}
	NULL
}

v3_tm_facets = function(arg_list) {
	if (length(arg_list$old)) {
		al = v3_list_text(olds = arg_list$old, news = arg_list$new)
		message(paste0("[v3->v4] tm_facets(): rename the following argument(s): ", al))
	}
	NULL
}

v3_tm_facets_free_scales = function() {
	message(paste0("[v3->v4] tm_facets(): migrate each 'free.scales.<X>' argument to the argument '<X>.free' of the corresponding layer function"))
}



v3_tm_legend_hide = function(fun, arg, vv) {
	message("[v3->v4] ", fun, "(): use '", vv, ".legend = tm_legend_hide()' instead of '", arg, " = FALSE")
}



v3_tm_legend_general = function(fun) {
	v3_start_message()
	message(paste0("[v3->v4] ", fun, "(): use 'tm_legend()' inside a layer function, e.g. 'tm_polygons(..., fill.legend = tm_legend())'"))
}




v3_tm_chart_hist = function(layer_fun, vv, arg) {
	message(paste0("[v3->v4] ", layer_fun, "(): use '", vv, ".chart = tm_chart_histogram()' instead of '", arg, " = TRUE'"))
	
}

v3_message_col_fill = function(layer_fun = layer_fun) {
	message(paste0("[v3->v4] ", layer_fun, "(): use 'fill' for the fill color of polygons/symbols (instead of 'col'), and 'col' for the outlines (instead of 'border.col')"))
}

v3_message_vv_null = function(layer_fun = layer_fun) {
	message(paste0("[v3->v4] ", layer_fun, "(): use 'NA' instead of 'NULL' to hide a visual variable"))
}


v3_message_fill_alpha = function(layer_fun = layer_fun) {
	message(paste0("[v3->v4] ", layer_fun, "(): use 'fill_alpha' instead of 'alpha'"))
}

v3_message_col_alpha = function(layer_fun = layer_fun, orig = "border.alpha") {
	message(paste0("[v3->v4] ", layer_fun, "(): use 'col_alpha' instead of '", orig, "'"))
}

v3_add_legend = function(type, args) {
	newtype = c(fill = "polygons", symbol = "symbols", line = "lines")	
	message(paste0("[v3->v4] tm_add_legend(): use 'type = \"", newtype[type],"\"' instead of 'type = \"", type,"\"'"))
	if ("col" %in% args && !c("fill" %in% args)) {
		message(paste0("[v3->v4] tm_add_legend(): use 'fill' instead of 'coll' for the fill color of ", newtype[type]))
	}
	if ("border.col" %in% args) {
		message(paste0("[v3->v4] tm_add_legend(): use 'col' instead of 'border.col' for the outline color of ", newtype[type]))
	}
}

v3_opt = function(olds, news, layer_fun) {
	x = v3_list_text(olds, news)
	message("[v3->v4] ", layer_fun, "(): migrate the layer options ", x, " to 'options = opt_", layer_fun, "(<HERE>)'")
}

# v3_multiple = function(layer_fun, vv) {
# 	if (!message_thrown("multiple_args")) {
# 		message("[v3->v4] ", layer_fun, "(): use '", vv, ".scale = list(<scale1>, <scale2>, ...)' to specify small multiples")
# 		message_reg("multiple_args")
# 	}
# 
# }