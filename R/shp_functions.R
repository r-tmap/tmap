assign_values = function(shp, dt, column) {
	shp[[1]][dt$tmapID__] = dt[[column]]
	shp
}

