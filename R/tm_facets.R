#' Specify facets
#'
#' @description
#' * `tm_facets_wrap()` specify facets for one grouping variable (so one faceting dimension).
#' * `tm_facets_(hv)stack()` stacks the facets either horizontally or vertically (one grouping variable).
#' * `tm_facets_grid()` supports up to three faceting dimensions.
#' * `tm_facets_pagewise()` can be used to replace the old `along` argument.
#' * `tm_facets_flip()` can be used to flip facets.
#' * `tm_facets()` is the core function, but it is recommended to use the other functions.
#'
#' @param by Group by variable (only for a facet wrap or facet stack)
#' @param rows Variable that specifies the rows (only for a facet grid)
#' @param columns Variable that specifies the columns (only for a facet grid)
#' @param pages Variable that specifies the pages (only for a facet grid)
#' @param as.layers show facets as layers?
#' @param nrow Number of rows
#' @param ncol Number of columns
#' @param byrow Should facets be wrapped by row?
#' @param orientation For facet stack: horizontal or vertical?
#' @param free.coords Logical. If the `by` argument is specified, should each
#'   map has its own coordinate ranges? By default `TRUE`, unless facets are shown
#'   in as different layers (`as.layers = TRUE`)
#' @param drop.units Logical. If the `by` argument is specified, should
#'   non-selected spatial units be dropped? If `FALSE`, they are plotted where
#'   mapped aesthetics are regarded as missing values. Not applicable for
#'   raster shapes. By default `TRUE`.
#' @param drop.empty.facets Logical. If the `by` argument is specified, should
#'   empty facets be dropped? Empty facets occur when the `by`-variable contains
#'   unused levels. When `TRUE` and two `by`-variables are specified, empty rows
#'   and columns are dropped.
#' @param drop.NA.facets Logical. If the `by` argument is specified, and all
#'   data values for specific facets are missing, should these facets be dropped?
#'   `FALSE` by default. In v3, it was called `showNA`.
#' @param sync Logical. Should the navigation in view mode (zooming and panning)
#'   be synchronized? By default `TRUE` if the facets have the same bounding box.
#'   This is generally the case when rasters are plotted, or when `free.coords` is
#'   `FALSE`.
#' @param na.text Text used for facets of missing values. In v3, it was `textNA`.
#' @param scale.factor Number that determines how the elements (e.g. font sizes,
#'   symbol sizes, line widths) of the small multiples are scaled in relation to
#'   the scaling factor of the shapes. The elements are scaled to the `scale.factor`th
#'   root of the scaling factor of the shapes. So, for `scale.factor = 1`,
#'   they are scaled proportional to the scaling of the shapes. Since elements,
#'   especially text, are often too small to read, a higher value is recommended.
#'   By default, `scale.factor = 2`.
#' @param type `"grid"`, `"wrap"` or `"stack"`
#' @param along deprecated Please use `tm_facets_pagewise()`
#' @param free.scales deprecated. Please use the `.free` arguments in the layer functions, e.g. `fill.free` in `tm_polygons`.
#' @param ... used to catch deprecated arguments
#' @example ./examples/tm_facets.R
#' @seealso \href{https://r-tmap.github.io/tmap/articles/basics_facets}{Vignette about facets}
#' @export
tm_facets = function(by = NULL,
					 rows = NULL,
					 columns = NULL,
					 pages = NULL,
					 as.layers = FALSE,
					 nrow = NA,
					 ncol = NA,
					 byrow = TRUE,
					 orientation = NA,
					 free.coords = NA,
					 drop.units = TRUE,
					 drop.empty.facets = TRUE,
					 drop.NA.facets = FALSE,
					 sync = TRUE,
					 na.text = NA,
					 scale.factor=2,
					 type = NA, # grid, wrap or stack
					 along = NULL,
					 free.scales = NULL,
					 ...) {

	args = list(...)
	args_called = names(rlang::call_match()[-1])

	if (any(v3_only("tm_facets") %in% names(args))) {
		layer_fun = "facets"
		v3_start_message()

		v3_list_init()
		drop.empty.facets = v3_impute(args, "showNA", TRUE, paste0("drop.empty.facets = ", !args$showNA))
		na.text = v3_impute(args, "textNA", NA, "na.text")
		drop.units = v3_impute(args, "drop.shapes", TRUE, "drop.units")
		v3_tm_facets(arg_list = v3_list_get())

		if (any(substr(names(args), 1, 12) == "free.scales.")) {
			v3_tm_facets_free_scales()
		}

	}



	if (!is.null(along)) {
		warning("The 'along' argument of 'tm_facets()' is deprecated as of tmap 4.0. Please use 'pages' instead.", call. = FALSE)
		pages = along
	}

	if (!is.null(by)) {
		if (is.na(type)) type = "wrapstack"
		rows = NULL
		columns = NULL
		pages = NULL
	}
	if (!is.null(rows) || !is.null(columns) || !is.null(pages)) {
		type = "grid"
		by = NULL
	}



	x = tm_element_list(tm_element(
		type = type,
		by = by,
		rows = rows,
		columns = columns,
		as.layers = as.layers,
		pages = pages,
		nrows = nrow,
		ncols = ncol,
		orientation = orientation,
		free.coords = free.coords,
		drop.units = drop.units,
		drop.empty.facets = drop.empty.facets,
		drop.NA.facets = drop.NA.facets,
		sync = sync,
		na.text = na.text,
		scale.factor = scale.factor,
		calls = args_called,
		subclass = "tm_facets"))

	if (!is.null(free.scales)) {
		message("tm_facets(): the argument free.scales is deprecated. Specify this via the layer functions (e.g. fill.free in tm_polygons)")
		x + tm_options(free.scales = free.scales)
	} else {
		x
	}
}

#' @export
#' @rdname tm_facets
tm_facets_grid = function(rows = NULL,
						  columns = NULL,
						  pages = NULL,
						  ...) {
	args = list(...)
	args_called = names(rlang::call_match()[-1])
	tm = do.call("tm_facets", c(list(by = NULL, rows = rows, columns = columns, pages = pages, type = "grid"), args[setdiff(names(args), "type")]))
	tm[[1]]$calls = args_called
	tm
}


#' @export
#' @rdname tm_facets
#' @param ... passed on to `tm_facets()`
tm_facets_wrap = function(by = "VARS__",
						  nrow = NA,
						  ncol = NA,
						  byrow = TRUE,
						  ...) {
	args = list(...)
	args_called = names(rlang::call_match()[-1])
	tm = do.call("tm_facets", c(list(by = by, nrow = nrow, ncol = ncol, byrow = byrow, type = "wrap"), args[setdiff(names(args), "type")]))
	tm[[1]]$calls = args_called
	tm
}

#' @export
#' @rdname tm_facets
tm_facets_pagewise = function(by = "VARS__",
						  nrow = 1,
						  ncol = 1,
						  byrow = TRUE,
						  ...) {
	args = list(...)
	args_called = unique(c(names(rlang::call_match()[-1]), "nrow", "ncol"))

	tm = do.call("tm_facets", c(list(by = by, nrow = nrow, ncol = ncol, byrow = byrow, type = "page"), args[setdiff(names(args), "type")]))
	tm[[1]]$calls = args_called
	tm
}

#' @export
#' @rdname tm_facets
tm_facets_stack = function(by = "VARS__",
						   orientation = NA,
						  ...) {
	args = list(...)
	args_called = names(rlang::call_match()[-1])
	tm = do.call("tm_facets", c(list(by = by, orientation = orientation, type = "stack"), args[setdiff(names(args), "type")]))
	tm[[1]]$calls = args_called
	tm
}

#' @export
#' @rdname tm_facets
tm_facets_hstack = function(by = "VARS__",
						   ...) {
	do.call("tm_facets_stack", c(list(by = by, orientation = "horizontal"), list(...)))
}

#' @export
#' @rdname tm_facets
tm_facets_vstack = function(by = "VARS__",
							...) {
	do.call("tm_facets_stack", c(list(by = by, orientation = "vertical"), list(...)))
}

#' @export
#' @rdname tm_facets
tm_facets_flip = function(...) {
	do.call(tm_facets, list(...)) + tm_options(facet.flip = TRUE)
}
