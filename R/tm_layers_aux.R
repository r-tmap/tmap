tm_basemap = function(server) {
	tm_element_list(tm_element(
		args = list(server = server),
		mapping.fun = "Basemap",
		subclass = "tm_aux_layer", "tm_layer"))
}

tm_grid = function() {
	tm_element_list(tm_element(
		args = list(),
		mapping.fun = "Grid",
		subclass = "tm_aux_layer", "tm_layer"))
}
