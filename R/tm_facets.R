tm_facets = function(by = NULL,
					 along = NULL) {
	
	tm_element_list(tm_element(
		by = by,
		along = along,
		ba = c(by, along),
		subclass = "tm_facets"))
}
