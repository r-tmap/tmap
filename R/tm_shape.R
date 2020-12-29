tm_shape = function(shp, 
					name = NULL,
					is.main = NA,
					crs = NULL,
					bbox = NULL,
					unit = NULL) {
	tm_element_list(tm_element(shp = shp,
							   is.main = is.main,
							   crs = crs,
							   bbox = bbox,
							   unit = unit,
							   shp_name = ifelse(is.null(name) == TRUE, deparse(substitute(shp))[1], name), 
							   subclass = "tm_shape"))
}
