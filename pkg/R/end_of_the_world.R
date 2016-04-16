end_of_the_world <- function(earth.datum="longlat", proj=NA, subdegrees=1, bbx=c(-180, 180, -90, 90), buffer=1e-6) {
	
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
		
		world_bb_sp <- SpatialPolygons(list(Polygons(list(Polygon(coords=world_bb_co)), ID="world_bb")), proj4string=CRS(get_proj4(earth.datum)))
		
		if (is.na(proj)) return(world_bb_sp) else {
			res <- tryCatch({
				set_projection(world_bb_sp, projection = proj)
			}, error=function(e){
				NULL
			}, warning=function(w){
				NULL
			})
			if (!is.null(res)) return(res)
		}
	}
	stop("Unable to determine end of the world in projection ", proj, call. = FALSE)
}

# end_of_the_world <- function(earth.datum="longlat", proj=NA, subdegrees=1) {
# 	ny <- 180 * subdegrees + 1
# 	nx <- 360 * subdegrees + 1
# 	
# 	step <- 1/subdegrees
# 	
# 	world_bb_co <- matrix(c(
# 		rep(-180, ny), rep(seq(-180+step, 180-step, by=step), length.out=nx-2), rep(180, ny), rep(seq(180-step, -180+step, by=-step), length.out=nx-2),
# 		rep(seq(-90, 90, by=step), length.out=ny), rep(90, nx-2), rep(seq(90, -90, by=-step), length.out=ny), rep(-90, nx-2)), ncol=2)
# 	
# 	world_bb_sp <- SpatialPolygons(list(Polygons(list(Polygon(coords=world_bb_co)), ID="world_bb")), proj4string=CRS(get_proj4(earth.datum)))
# 	
# 	if (is.na(proj)) world_bb_sp else set_projection(world_bb_sp, projection = proj)
# }
