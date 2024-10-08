#' Map component: manual legend
#' 
#' Map component that adds a manual legend
#' 
#' @param ... visual variables and arguments passed on to `tm_legend()`.
#'   By default, the argument `type` is set to `"Symbols"`, which means that the
#'   supported visual variables are: `"fill"`, `"col"`, `"shape"`, `"size"`,
#'   `"fill_alpha"`, `"col_alpha"`, `"lty"`, `"lwd"`, `"linejoin"`, and `"lineend"`.
#' @param labels labels
#' @param type the layer type from which the visual variables (see `...`) are taken.
#'   Options: `"symbols"` (default), `"lines"`, `"polygons"`, and `"text"`.
#' @param title text of the title
#' @param design legend design
#' @param orientation legend orientation
#' @param group Name of the group to which this layer belongs. This is only
#'   relevant in view mode, where layer groups can be switched (see `group.control`)
#' @param group.control In view mode, the group control determines how
#'   layer groups can be switched on and off. Options: `"radio"` for radio
#'   buttons (meaning only one group can be shown), `"check"` for check boxes
#'   (so multiple groups can be shown), and `"none"` for no control
#'   (the group cannot be (de)selected).
#' @param resize.as.group resize.as.group
#' @param z z
#' @export
tm_add_legend = function(...,
						 labels,
						 type = "symbols",
						 title = "",
						 design = NULL,
						 orientation = NULL,
						 group = NA,
						 group.control = "check",
						 resize.as.group = FALSE, 
						 z = as.integer(NA)) {
	#args = lapply(as.list(match.call()[-1]), eval, envir = parent.frame())
	if (missing(labels)) stop("tm_add_legend: labels required", call. = FALSE)
	args = c(as.list(environment()), list(...))
	if (type %in% c("fill", "symbol", "line")) {
		v3_add_legend(type, names(args))
		if ("col" %in% names(args) && !c("fill" %in% names(args))) {
			args$fill = args$col
			args$col = NULL
		}
		if ("border.col" %in% names(args)) {
			args$col = args$border.col
			args$border.col = NULL
		}
	}
	tm_element_list(do.call(tm_element, c(args, list(subclass = c("tm_add_legend", "tm_component")))))
}

#' @export
#' @keywords internal
#' @rdname tmap_internal
toTitleCase = function(x) {
	paste0(toupper(substr(x,1,1)), tolower(substr(x,2, nchar(x))))
}


tmapAddedLegend = function(comp, o) {
	#message("tm_mouse_coordinates ignored for 'plot' mode")
	l = update_l(o = o, l = comp, v = "", mfun = toTitleCase(comp$type), unm = "", active = FALSE)

	fun = paste0("tm_", comp$type)
	if (!exists(fun)) {
		stop(paste0("type \"", comp$type, "\" not supported because tm_", comp$type,  " not found"), call. = FALSE)
	}
	res = do.call(fun, args = list())
	gp = res[[1]]$gpar
	
	for (gpi in names(gp)) {
		if (gpi %in% names(l)) {
			gp[[gpi]] = l[[gpi]]
		} else {
			gp[[gpi]] = getAesOption("value.const", o, aes = gpi, layer = comp$type)
		}
	}
	
	l$gp = gp
	
	l2 = within(l, {
		nitems = length(labels)
		dvalues = 1:nitems
		vvalues = 1:nitems
		vneutral = NA
		na.show = FALSE
		
	})
	l2
}
