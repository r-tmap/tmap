tmapMapRaster = function(tms, tml) {
	
}

tmapMapPolygon = function(tms, fill, fill.setup) {
	data.table(fill = tmapAesColorDiscrete(fill, fill.setup),
			   color = tmapAesColorDiscrete(color, color.setup))
	
}
tmapMapPoint = function(tms, tml) {
	
}
