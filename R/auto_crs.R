auto_crs = function(x, world_crs = "+proj=eck4") {
	# placeholder for new automatic-crs function implemented elsewhere

	# for the time being: use unprojected coordinates
	st_crs(x) # was 4326, but this may cause transformation

}
