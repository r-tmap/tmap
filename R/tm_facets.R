tm_facets = function(by = "VARS__",
					 rows = NULL,
					 columns = NULL,
					 pages = NULL,
					 nrows = NA,
					 ncols = NA,
					 free.coords = NA,
					 drop.units = TRUE,
					 drop.empty.facets = TRUE,
					 drop.NA.facets = FALSE,
					 sync = TRUE,
					 showNA = NA) {
	
	calls <- names(match.call(expand.dots = TRUE)[-1])
	is.wrap = is.null(rows) && is.null(columns) && is.null(pages)
	
	if (is.na(free.coords)) {
		if (is.wrap) {
			free.coords = rep((by != "VARS__"), 2)
		} else {
			free.coords = c((!is.null(rows) && (rows != "VARS__")), (!is.null(columns)) && (columns != "VARS__"))
		}	
	} else {
		free.coords = rep(free.coords, length.out = 2)
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
		calls = calls,
		subclass = "tm_facets"))
	
	
	
}

tm_facets_grid = function(rows = NULL,
						  columns = NULL,
						  pages = NULL,
						  ...) {
	args = list(...)
	calls = names(match.call(expand.dots = TRUE)[-1])
	tm = do.call("tm_facets", c(list(by = NULL, rows = rows, columns = columns, pages = pages), args[setdiff(names(args), "by")]))
	tm[[1]]$calls = calls
	tm
}

tm_facets_wrap = function(by = "VARS__",
						  nrows = NA,
						  ncols = NA,
						  ...) {
	args = list(...)
	calls = names(match.call(expand.dots = TRUE)[-1])
	tm = do.call("tm_facets", c(list(by = by, rows = NULL, columns = NULL, pages = NULL, nrows = nrows, ncols = ncols), args[setdiff(names(args), c("rows", "columns", "pages"))]))
	tm[[1]]$calls = calls
	tm
}
