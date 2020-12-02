tm_shape = function(shp, 
					name = NULL,
					is.main = NA,
					crs = NULL,
					bbox = NULL,
					unit = NULL) {
	lst = list(shp = shp,
			   is.main = is.main,
			   crs = crs,
			   bbox = bbox,
			   unit = unit,
			   shp_name = ifelse(is.null(name) == TRUE, deparse(substitute(shp))[1], name))
	tm_element_list(tm_element(lst, subclass = "tm_shape"))
}

tm_polygons = function(col = "blue") {
	tm_element_list(tm_element(as.list(environment()), subclass = "tm_polygons"))
}




.tmapShape = function(...) {
	UseMethod(".tmapShape")
}

#' @method .tmapShape stars
#' @export
.tmapShape.stars = function(shp, name, is.main, crs, bbox, unit, shp_name) {
	print("!!!stars")
}


#' @method .tmapShape sf
#' @export
.tmapShape.sf = function(shp, name, is.main, crs, bbox, unit, shp_name) {
	print("$$$sf")
	
}


# .tmapShape = function(features, data, crs, bbox) {
# 	structure(list(features = features, data = data, crs = crs, bbox = bbox), class = ".tmapShape")
# }
# 
# 
# 
# 
# tmapLayer = function(shapeTransGroup = NULL, 
# 					 shapeTransLayer = NULL,
# 					 aes,
# 					 aes.setup,
# 					 meta) {
# 	
# }
# 
