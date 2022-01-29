tm_basemap = function(server, alpha, zoom) {
	tm_element_list(tm_element(
		args = lapply(as.list(match.call()[-1]), eval, envir = parent.frame()),
		mapping.fun = "Basemap",
		subclass = c("tm_basemap", "tm_aux_layer")))
}

tm_grid = function() {
	tm_element_list(tm_element(
		args = list(),
		mapping.fun = "Grid",
		subclass = c("tm_grid", "tm_aux_layer")))
}
