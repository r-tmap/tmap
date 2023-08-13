#' define multivariate variable
#' 
#' define multivariate variable
#' 
#' @param ... variable names
#' @export
tm_mv = function(...) {
	list(c(...))
}

# tm_mv_dim = function(x, values) {
# 	structure(list(x = x, values = values), class = "tmapDimVars")
# }

tmapVars = function(x) {
	if (inherits(x, "tmapOption")) return(x)
	if (inherits(x, "tm_shape_vars")) return(structure(list(), class = "tmapShpVars"))
	
	cls = if (inherits(x, "AsIs")) "tmapAsIs" else "tmapVars"
	
	isL = is.list(x)
	if (!isL) {
		x = as.list(x)
	}# else {
	#	x = list(x)
	#}
	
	structure(x, class = cls)
}
format_aes_results = function(values, ord = NULL, legend) {
	legnr = vector(mode = "integer", length = length(values))
	legnr[1] = legend_save(legend)
	if (is.null(ord)) {
		list(values = values,
			 legnr = legnr)	
	} else {
		list(values = values,
			 ord = ord,
			 legnr = legnr)
	}
}





# set_legend_number = function(nr) {
# 	assign("legnr", nr, envir = .TMAP)
# }
legends_init = function() {
	assign("legs", list(), envir = .TMAP)
}

legend_save = function(legend) {
	if (!exists("legs", envir = .TMAP)) legends_init()
	legs = get("legs", envir = .TMAP)
	legs = c(legs, (list(legend)))
	assign("legs", legs, envir = .TMAP)
	length(legs)
}



data_type = function(x) {
	if (all(is.na(x))) {
		"na"
	} else if (is.ordered(x)) {
		"order"
	} else if (is.logical(x) || is.character(x) || is.factor(x)) {
		"factor"
	} else if (is.numeric(x)) {
		if (any(x < 0 & !is.na(x)) && any(x > 0 & !is.na(x))) {
			"div"
		} else {
			"seq"
		}
	} else {
		"unknown"
	}
}

data_type_grp = function(x) {
	if (x %in% c("seq", "div")) {
		"num"
	} else {
		x
	}
}

data_class = function(x, check_for_color_class = FALSE) {
	
	# if (all(is.na(x))) {
	# 	"na"
	# } else
	cls = if (is.numeric(x)) {
		y = without_units(x)
		subclass1 = if (is.integer(x)) "int" else "real"
		subclass2 = if (any(y < 0 & !is.na(y)) && any(y > 0 & !is.na(y))) {
			"div"
		} else {
			"seq"
		}
		c("num", subclass1, subclass2)
	} else {
		if (check_for_color_class) {
			w = which(!is.na(x))
			if (length(w) && all(valid_colors(head(x[w], 100)))) {
				c("asis", "color")	
			} else {
				subclass = if (is.ordered(x)) "ord" else "unord"
				c("fact", subclass)	
			}
		} else {
			subclass = if (is.ordered(x)) "ord" else "unord"
			c("fact", subclass)
		}
	}
	
	attr(cls, "units") = if (inherits(x, "units")) {
		paste0(" [", units(x), "]")
	} else ""
	attr(cls, "unique") = (length(unique(x)) == 1)
	cls
}


tmapScale = function(aes, value, scale, legend, free) {
	structure(list(aes = aes, value = tmapVars(value), scale = scale, legend = legend, free = free), class = c("tmapScale", "list"))
}

tmapScaleAuto = function(x1, scale, legend, o, aes, layer, layer_args, sortRev, bypass_ord, submit_legend = TRUE, x2 = NULL) {
	cls = data_class(x1, check_for_color_class = aes %in% c("col", "fill"))
	
	#if (cls[1] == "na")
	
	if (!is.null(x2)) {
		sc = "bivariate"
	} else if (cls[1] == "asis") {
		sc = "asis"	
	} else {
		sc_opt = getAesOption("scales.var", o, aes, layer, cls = cls)
		sc_pref = scale$fun_pref
		
		if (!is.null(sc_pref)) {
			if (sc_pref %in% c("categorical", "continuous", "continuous_log")) {
				sc = sc_pref
			} else {
				sc = sc_opt
			}
		} else {
			sc = sc_opt
		}
	}
	
	
	tm_scalefun = paste0("tm_scale_", sc)
	
	scale = scale[names(scale) %in% names(formals(tm_scalefun))]
	
	scale_new = do.call(tm_scalefun, args = scale)
	
	FUN = scale_new$FUN
	scale_new$FUN = NULL
	
	if (sc == "bivariate") {
		do.call(FUN, list(x1 = x1, x2 = x2, scale = scale_new, legend = legend, o = o, aes = aes, layer = layer, layer_args = layer_args, sortRev, bypass_ord, submit_legend))
	} else {
		do.call(FUN, list(x1 = x1, scale = scale_new, legend = legend, o = o, aes = aes, layer = layer, layer_args = layer_args, sortRev, bypass_ord, submit_legend))
	}
	
}
