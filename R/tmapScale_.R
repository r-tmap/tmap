#' define multivariate variable
#' 
#' define multivariate variable
#' 
#' @param ... variable names
#' @export
tm_mv = function(...) {
	list(c(...))
}

#' define multivariate variable based on stars dimension 
#' 
#' define multivariate variable based on stars dimension 
#' 
#' @param x dimension name
#' @param values values to be used
#' @export
tm_mv_dim = function(x, values) {
	structure(list(x = x, values = values), class = "tmapDimVars")
}

tmapVars = function(x) {
	if (inherits(x, "tmapOption")) return(x)
	if (inherits(x, "tm_shape_vars")) return(structure(list(ids = x$ids, n = x$n), class = "tmapShpVars"))
	if (inherits(x, "tm_mv_shape_vars")) return(structure(list(ids = x$ids, n = x$n), class = "tmapMVShpVars"))
	if (inherits(x, "tmapDimVars")) return(x)
	
	cls = if (inherits(x, "AsIs")) "tmapAsIs" else if (inherits(x, "tmapUsrCls")) "tmapUsrCls" else "tmapVars"
	
	isL = is.list(x)
	if (!isL) {
		x = as.list(x)
	}# else {
	#	x = list(x)
	#}
	
	structure(x, class = cls)
}

#' @param x x
#' @export
#' @name tmapUsrCls
#' @rdname tmap_internal
#' @keywords internal
tmapUsrCls = function(x) {
	structure(x, class = "tmapUsrCls")
}



format_aes_results = function(values, ord = NULL, legend, chart) {
	legnr = vector(mode = "integer", length = length(values))
	legnr[1] = legend_save(legend)
	
	crtnr = vector(mode = "integer", length = length(values))
	crtnr[1] = chart_save(chart)
	
	if (is.null(ord)) {
		list(values = values,
			 legnr = legnr,
			 crtnr = crtnr)	
	} else {
		list(values = values,
			 ord = ord,
			 legnr = legnr,
			 crtnr = crtnr)
	}
}





# set_legend_number = function(nr) {
# 	assign("legnr", nr, envir = .TMAP)
# }
legends_init = function() {
	assign("legs", list(), envir = .TMAP)
}

charts_init = function() {
	assign("charts", list(), envir = .TMAP)
}

legend_save = function(legend) {
	if (!exists("legs", envir = .TMAP)) legends_init()
	legs = get("legs", envir = .TMAP)
	legs = c(legs, (list(legend)))
	assign("legs", legs, envir = .TMAP)
	length(legs)
}

chart_save = function(legend) {
	if (!exists("charts", envir = .TMAP)) charts_init()
	charts = get("charts", envir = .TMAP)
	charts = c(charts, (list(legend)))
	assign("charts", charts, envir = .TMAP)
	length(charts)
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
	attr(cls, "unique") = (length(na.omit(unique(x)))==1)
	cls
}


#' @param aes aes
#' @param value value
#' @param scale scale
#' @param legend legend
#' @param chart chart
#' @param free free
#' @export
#' @name tmapScale
#' @rdname tmap_internal
#' @keywords internal
tmapScale = function(aes, value, scale, legend, chart, free) {
	structure(list(aes = aes, value = tmapVars(value), scale = scale, legend = legend, chart = chart, free = free), class = c("tmapScale", "list"))
}

tmapScaleAuto = function(x1, scale, legend, chart, o, aes, layer, layer_args, sortRev, bypass_ord, submit_legend = TRUE, ...) {
	args = list(...)
	k = length(args) + 1L
	if (length(args)) {
		names(args) = paste0("x", 2L:(length(args)+1L))
	}
	
	cls = data_class(x1, check_for_color_class = aes %in% c("col", "fill"))
	
	#if (cls[1] == "na")
	sc_opt = getAesOption("scales.var", o, aes, layer, cls = cls)
#if (aes == "fill") browser()	
	if (k == 2) {
		sc = "bivariate"
	} else if (k > 2) {
		stop("No default scale for multivariate variables", call. = FALSE)
		#sc = "composition"
	} else if (cls[1] == "asis") {
		sc = "asis"	
	} else if (attr(cls, "unique") && !(sc_opt == "asis")) {
		if ("num" %in% cls) {
			sc = "ordinal"
			message("The visual variable \"", aes, "\" of the layer \"", layer, "\" contains a unique value. Therefore a discrete scale is applied (tm_scale_discrete).")	
		} else {
			sc = "categorical"	
			message("The visual variable \"", aes, "\" of the layer \"", layer, "\" contains a unique value. Therefore a categorical scale is applied (tm_scale_categorical).")
		}
	} else {
		
		sc_pref = scale$fun_pref
		
		if (!is.null(sc_pref)) {
			if (sc_pref %in% c("categorical", "continuous", "continuous_log", "rank")) {
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
		do.call(FUN, list(x1 = x1, x2 = args[[1]], scale = scale_new, legend = legend, chart = chart, o = o, aes = aes, layer = layer, layer_args = layer_args, sortRev = sortRev, bypass_ord = bypass_ord, submit_legend = submit_legend))
	} else if (sc == "multi_continuous") {
		do.call(FUN, c(list(x1 = x1), args, list(scale = scale_new, legend = legend, chart = chart, o = o, aes = aes, layer = layer, layer_args = layer_args, sortRev = sortRev, bypass_ord = bypass_ord, submit_legend = submit_legend)))
	} else {
		do.call(FUN, list(x1 = x1, scale = scale_new, legend = legend, chart = chart, o = o, aes = aes, layer = layer, layer_args = layer_args, sortRev, bypass_ord, submit_legend))
	}
	
}
