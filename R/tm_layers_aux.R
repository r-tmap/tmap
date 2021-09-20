tm_basemap = function(server) {
	tm_element_list(tm_element(
		args = list(server = server),
		subclass = "tm_aux_layer", "tm_layer"))
}
