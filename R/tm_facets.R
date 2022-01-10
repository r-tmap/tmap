#' @export
tm_facets = function(by = NULL,
					 rows = NULL,
					 columns = NULL,
					 pages = NULL,
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
					 type = NA # grid, wrap or stack
					 ) {
	
	calls <- names(match.call(expand.dots = TRUE)[-1])
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
tm_facets_hstack = function(by = "VARS__",
						   ...) {
	do.call("tm_facets_stack", c(list(by = by, orientation = "horizontal"), list(...)))
}

#' @export
tm_facets_vstack = function(by = "VARS__",
							...) {
	do.call("tm_facets_stack", c(list(by = by, orientation = "vertical"), list(...)))
}

tm_facet_flip = function() {
	tm_options(facet.flip = TRUE)
}
