#' @export
tm_facets = function(by = NULL,
					 rows = NULL,
					 columns = NULL,
					 pages = NULL,
					 nrows = NA,
					 ncols = NA,
					 byrow = TRUE,
					 free.coords = NA,
					 drop.units = TRUE,
					 drop.empty.facets = TRUE,
					 drop.NA.facets = FALSE,
					 sync = TRUE,
					 showNA = NA,
					 textNA = "Mssing",
					 scale.factor=2,
					 is.wrap = NA) {
	
	calls <- names(match.call(expand.dots = TRUE)[-1])
	if (!is.null(by)) {
		is.wrap = TRUE
		rows = NULL
		columns = NULL
		pages = NULL
	}
	if (!is.null(rows) || !is.null(columns) || !is.null(pages)) {
		is.wrap = FALSE
		by = NULL
	}

	tm_element_list(tm_element(
		is.wrap = is.wrap,
		by = by,
		rows = rows,
		columns = columns,
		pages = pages,
		nrows = nrows,
		ncols = ncols,
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
	tm = do.call("tm_facets", c(list(by = NULL, rows = rows, columns = columns, pages = pages, is.wrap = FALSE), args[setdiff(names(args), "is.wrap")]))
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
	tm = do.call("tm_facets", c(list(by = by, nrows = nrows, ncols = ncols, byrow = byrow, is.wrap = TRUE), args[setdiff(names(args), "is.wrap")]))
	tm[[1]]$calls = calls
	tm
}


tm_facet_flip = function() {
	tm_options(facet.flip = TRUE)
}
