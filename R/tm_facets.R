tm_facets_grid = function(rows = NULL,
						  columns = NULL,
						  pages = NULL) {
	
	tm_element_list(tm_element(
		is.wrap = FALSE,
		wrap = NULL,
		rows = rows,
		columns = columns,
		pages = pages,
		nrows = NA,
		ncols = NA,
		subclass = "tm_facets"))
}

tm_facets_wrap = function(by = "VARS__",
						  nrows = NA,
						  ncols = NA) {
	
	tm_element_list(tm_element(
		is.wrap = TRUE,
		wrap = by,
		rows = NULL,
		columns = NULL,
		pages = NULL,
		nrows = nrows,
		ncols = ncols,
		subclass = "tm_facets"))
}
