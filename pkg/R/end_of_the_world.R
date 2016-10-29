# Reproject the end of the world (-180, 180; -90, 90) to polygon. Needed 1) to return feasible bounding boxes (bb) and 2) to cut grid lines in world maps
end_of_the_world <- function(earth.datum="longlat", proj=NA, subdegrees=1, bbx=c(-180, 180, -90, 90), buffer=1e-6, suppress.warnings=TRUE) {
	
	CRS_datum <- get_proj4(earth.datum, as.CRS = TRUE)
	proj <- get_proj4(proj, as.CRS = TRUE)
	
	for (b in buffer*(10^(0:6))) {
		x1 <- bbx[1] + b
		x2 <- bbx[2] - b
		y1 <- bbx[3] + b
		y2 <- bbx[4] - b
		
		dx <- x2-x1
		dy <- y2-y1
		
		ny <- dy * subdegrees + 1
		nx <- dx * subdegrees + 1
		step <- 1/subdegrees
		
		world_bb_co <- matrix(c(
			rep(x1, ny), rep(seq(x1+step, x2-step, by=step), length.out=nx-2), rep(x2, ny), rep(seq(x2-step, x1+step, by=-step), length.out=nx-2),
			rep(seq(y1, y2, by=step), length.out=ny), rep(y2, nx-2), rep(seq(y2, y1, by=-step), length.out=ny), rep(y1, nx-2)), ncol=2)
		
		world_bb_sp <- SpatialPolygons(list(Polygons(list(Polygon(coords=world_bb_co)), ID="world_bb")), proj4string=CRS_datum)
		
		res <- if (is.na(proj)) {
			world_bb_sp 
		} else {
			tryCatch({
				spTransform(world_bb_sp, proj)
			}, error=function(e){
				NULL
			}, warning=function(w){
				NULL
			})
		}
		if (!is.null(res)) break
	}

	if (is.null(res)) stop("Unable to determine end of the world in projection ", proj, call. = FALSE)
	res
}
