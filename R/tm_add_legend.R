#' Map component: manual legend
#'
#' Map component that adds a manual legend.
#'
#' @param ... visual variables and arguments passed on to `tm_legend()`.
#'   By default, the argument `type` is set to `"symbols"`, which means that the
#'   supported visual variables are: `"fill"`, `"col"`, `"shape"`, `"size"`,
#'   `"fill_alpha"`, `"col_alpha"`, `"lty"`, `"lwd"`, `"linejoin"`, and `"lineend"`.
#'   The number of legend items will be equal to the maximum number of specific values (and specified labels.)
#' @param labels labels by default `""` (so omitted)
#' @param type the layer type from which the visual variables (see `...`) are taken.
#'   Options: `"symbols"` (default), `"lines"`, `"polygons"`, and `"text"`.
#' @param title `r .doc_opt("legend.title")`
#' @param design `r .doc_opt("legend.design")`
#' @param orientation `r .doc_opt("legend.orientation")`
#' @param position `r .doc_opt("legend.position")`
#' @inheritParams tm_title
#' @param group Name of the group to which this layer belongs. This is only
#'   relevant in view mode, where layer groups can be switched (see `group.control`)
#' @param group.control In view mode, the group control determines how
#'   layer groups can be switched on and off. Options: `"radio"` for radio
#'   buttons (meaning only one group can be shown), `"check"` for check boxes
#'   (so multiple groups can be shown), and `"none"` for no control
#'   (the group cannot be (de)selected).
#' @example ./examples/tm_add_legend.R
#' @export
tm_add_legend = function(...,
						 labels = "",
						 type = "symbols",
						 title = "",
						 design = NULL,
						 orientation = NULL,
						 position = NULL,
						 group_id = NA_character_,
						 group = NA,
						 group.control = "check",
						 z = NA_integer_) {

	args = lapply(as.list(rlang::call_match(defaults = TRUE)[-1]), eval, envir = parent.frame())

	args = warning_group_args(args)

	if (type %in% c("fill", "symbol", "line")) {
		args$type = v3_add_legend(type, names(args))
		if ("col" %in% names(args) && !c("fill" %in% names(args)) && type != "line") {
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
		} else if (!is.na(gp[[gpi]])) {
			gp[[gpi]] = getAesOption("value.const", o, aes = gpi, layer = comp$type)
		}
	}

	l$gp = gp

	l2 = within(l, {
		nitems = max(length(labels), vapply(gp, length, FUN.VALUE = integer(1), USE.NAMES = FALSE))
		labels = rep(labels, length.out = nitems)
		dvalues = 1:nitems
		vvalues = 1:nitems
		vneutral = NA
		na.show = FALSE

	})
	l2
}
