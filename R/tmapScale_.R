#' @export
tm_mv = function(...) {
	list(c(...))
}

tmapVars = function(x) {
	if (inherits(x, "tmapOption")) return(x)
	if (inherits(x, "tm_shape_vars")) return(structure(list(), class = "tmapShpVars"))
	
	cls = if (inherits(x, "AsIs")) "tmapAsIs" else "tmapVars"

	isL = is.list(x)
	if (!isL) x = as.list(x)

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

data_class = function(x) {
	
	# if (all(is.na(x))) {
	# 	"na"
	# } else
	 cls = if (is.numeric(x)) {
	 	y = if (inherits(x, "units")) units::drop_units(x) else x
	 	
		subclass1 = if (is.integer(x)) "int" else "real"
		subclass2 = if (any(y < 0 & !is.na(y)) && any(y > 0 & !is.na(y))) {
			"div"
		} else {
			"seq"
		}
		c("num", subclass1, subclass2)
	} else {
		subclass = if (is.ordered(x)) "ord" else "unord"
		c("fact", subclass)
	}
	 
	attr(cls, "units") = if (inherits(x, "units")) {
		paste0(" [", units(x), "]")
	} else ""
	cls
}


tmapScale = function(aes, value, scale, legend, free) {
	structure(list(aes = aes, value = tmapVars(value), scale = scale, legend = legend, free = free), class = c("tmapScale", "list"))
}

tmapScaleAuto = function(x1, scale, legend, o, aes, layer, sortRev, bypass_ord) {
	cls = data_class(x1)
	
	#if (cls[1] == "na")
	
	sc = getAesOption("scales.var", o, aes, layer, cls = cls)
	
	tm_scalefun = paste0("tm_scale_", sc)
	
	scale_new = do.call(tm_scalefun, args = scale)
	
	
	FUN = scale_new$FUN
	scale_new$FUN = NULL
	do.call(FUN, list(x1 = x1, scale = scale_new, legend = legend, o = o, aes = aes, layer = layer, sortRev, bypass_ord))
	
}
