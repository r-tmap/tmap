end_of_the_world <- function(earth.datum="longlat", proj=NA) {
	world_bb_co <- matrix(c(
		rep(-180, 181), seq(-179, 179), rep(180, 181), seq(179, -179),
		seq(-90, 90), rep(90, 359), seq(90, -90), rep(-90, 359)), ncol=2)
	
	world_bb_sp <- SpatialPolygons(list(Polygons(list(Polygon(coords=world_bb_co)), ID="world_bb")), proj4string=CRS(get_proj4(earth.datum)))
	
	if (is.na(proj)) world_bb_sp else set_projection(world_bb_sp, projection = proj)
}
