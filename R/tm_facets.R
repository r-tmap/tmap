#' Facets
#' 
#' Specify facets. `tm_facets()` is the core function, but recommended is to use
#' `tm_facets_wrap()`, `tm_facets_stack()` or `tm_facets_grid()`.
#' The former two specify facets for one grouping variable (so one faceting dimension).
#' The difference is that wrap may place facets in multiple rows and columns whereas
#' `tm_facets_stack()` stacks the facets either horizontally or vertically.
#' `tm_facets_grid()` supports up to three faceting dimensions.
#'
#' @param by Group by variable (only for a facet wrap or facet stack)
#' @param rows Variable that specifies the rows (only for a facet grid)
#' @param columns Variable that specifies the columns (only for a facet grid)
#' @param pages Variable that specifies the pages (only for a facet grid)
#' @param as.layers show facets as layers?
#' @param nrows Number of rows
#' @param ncols Number of columns
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
#'   `FALSE` by default.
#' @param sync Logical. Should the navigation in view mode (zooming and panning)
#'   be synchronized? By default `TRUE` if the facets have the same bounding box.
#'   This is generally the case when rasters are plotted, or when `free.coords` is
#'   `FALSE`.
#' @param showNA If the `by` argument is specified, should missing values of the
#'   `by`-variable be shown in a facet? If two `by`-variables are specified,
#'   should missing values be shown in an additional row and column? 
#'   If `NA`, missing values only are shown if they exist. Similar to the
#'   `useNA` argument of [table()][base::table()], where `TRUE`, `FALSE`,
#'   and `NA` correspond to `"always"`, `"no"`, and `"ifany"` respectively.
#' @param textNA Text used for facets of missing values.
#' @param scale.factor Number that determines how the elements (e.g. font sizes,
#'   symbol sizes, line widths) of the small multiples are scaled in relation to
#'   the scaling factor of the shapes. The elements are scaled to the `scale.factor`th
#'   root of the scaling factor of the shapes. So, for `scale.factor=1`,
#'   they are scaled proportional to the scaling of the shapes. Since elements,
#'   especially text, are often too small to read, a higher value is recommended.
#'   By default, `scale.factor=2`.
#' @param type `"grid"`, `"wrap"` or `"stack"`
#' @param along deprecated Please use `tm_facets_page()`
#' @export
#' @rdname tm_facets
#' @name tm_facets
tm_facets = function(by = NULL,
					 rows = NULL,
					 columns = NULL,
					 pages = NULL,
					 as.layers = FALSE,
					 nrows = NA,
					 ncols = NA,
					 byrow = TRUE,
					 orientation = NA,
					 free.coords = NA,
					 drop.units = TRUE,
					 drop.empty.facets = TRUE,
					 drop.NA.facets = FALSE,
					 sync = TRUE,
					 showNA = NA,
					 textNA = "Mssing",
					 scale.factor=2,
					 type = NA, # grid, wrap or stack
					 along = NULL
					 ) {
	
	calls <- names(match.call(expand.dots = TRUE)[-1])
	
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
	


	tm_element_list(tm_element(
		type = type,
		by = by,
		rows = rows,
		columns = columns,
		as.layers = as.layers,
		pages = pages,
		nrows = nrows,
		ncols = ncols,
		orientation = orientation,
		free.coords = free.coords,
		drop.units = drop.units,
		drop.empty.facets = drop.empty.facets,
		drop.NA.facets = drop.NA.facets,
		sync = sync,
		showNA = showNA,
		textNA = textNA,
		scale.factor = scale.factor,
		calls = calls,
		subclass = "tm_facets"))
}

#' @export
#' @rdname tm_facets
#' @name tm_facets_grid
tm_facets_grid = function(rows = NULL,
						  columns = NULL,
						  pages = NULL,
						  ...) {
	args = list(...)
	calls = names(match.call(expand.dots = TRUE)[-1])
	tm = do.call("tm_facets", c(list(by = NULL, rows = rows, columns = columns, pages = pages, type = "grid"), args[setdiff(names(args), "type")]))
	tm[[1]]$calls = calls
	tm
}


#' @export
#' @rdname tm_facets
#' @param ... passed on to `tm_facets()`
#' @name tm_facets_wrap
tm_facets_wrap = function(by = "VARS__",
						  nrows = NA,
						  ncols = NA,
						  byrow = TRUE,
						  ...) {
	args = list(...)
	calls = names(match.call(expand.dots = TRUE)[-1])
	tm = do.call("tm_facets", c(list(by = by, nrows = nrows, ncols = ncols, byrow = byrow, type = "wrap"), args[setdiff(names(args), "type")]))
	tm[[1]]$calls = calls
	tm
}

#' @export
#' @rdname tm_facets
#' @name tm_facets_pagewise
tm_facets_pagewise = function(by = "VARS__",
						  nrows = 1,
						  ncols = 1,
						  byrow = TRUE,
						  ...) {
	args = list(...)
	calls = names(match.call(expand.dots = TRUE)[-1])
	tm = do.call("tm_facets", c(list(by = by, nrows = nrows, ncols = ncols, byrow = byrow, type = "page"), args[setdiff(names(args), "type")]))
	tm[[1]]$calls = calls
	tm
}

#' @export
#' @rdname tm_facets
#' @name tm_facets_stack
tm_facets_stack = function(by = "VARS__",
						   orientation = NA,
						  ...) {
	args = list(...)
	calls = names(match.call(expand.dots = TRUE)[-1])
	tm = do.call("tm_facets", c(list(by = by, orientation = orientation, type = "stack"), args[setdiff(names(args), "type")]))
	tm[[1]]$calls = calls
	tm
}

#' @export
#' @rdname tm_facets
#' @name tm_facets_hstack
tm_facets_hstack = function(by = "VARS__",
						   ...) {
	do.call("tm_facets_stack", c(list(by = by, orientation = "horizontal"), list(...)))
}

#' @export
#' @rdname tm_facets
#' @name tm_facets_vstack
tm_facets_vstack = function(by = "VARS__",
							...) {
	do.call("tm_facets_stack", c(list(by = by, orientation = "vertical"), list(...)))
}

#' @export
#' @rdname tm_facets
#' @name tm_facet_flip
tm_facets_flip = function() {
	tm_options(facet.flip = TRUE)
}
