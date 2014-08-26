get_lengths <- function(shp, digits=3) {
	round(SpatialLinesLengths(shp, longlat=FALSE)/1000, digits=digits)
}

# get angle from the coordinates of a line (only considers endpoints)
get_direction_angle <- function(co) {
	p1 <- co[1,]
	p2 <- co[nrow(co),]
	
	a <- atan2(p2[2] - p1[2], p2[1] - p1[1]) * 180 / pi
	if (a < 0) a <- a + 360
	a
}

# get angle in degrees that correspond to the direction NOZW
get_NOZW_angle <- function(direction) {
	switch(direction, 
		   N=90,
		   NO=45,
		   O=0,
		   ZO=315,
		   Z=270,
		   ZW=225,
		   W=180,
		   NW=135)
}

# return the absolute difference of two angles in degrees
angle_diff <- function(a1, a2) {
	y <- a2 / 180 * pi
	x <- a1 / 180 * pi
	abs(atan2(sin(x-y), cos(x-y)) * 180 / pi)
}


# 
set_directions <- function(lines, main_direction=TRUE, directions=directions, roads=roads) {
	co <- coordinates(lines)
	
	rns <- roads$number[match(lines$ID, roads$name)]
	
	co2 <- mapply(function(p, rn) {
		a_rw <- get_NOZW_angle(directions$directionR[directions$roadnumber==rn])
		if (!main_direction) a_rw <- a_rw + 180
		
		## set direction per polyline segment
		p2 <- lapply(p, function(l) {
			a1 <- get_direction_angle(l)
			adiff <- angle_diff(a1, a_rw)
			if (adiff > 90) l <- l[nrow(l):1,]
			l
		})
		
		## set order of polyline segments
		if (length(p2)>1) {
			line_centers <- lapply(p2, function(l) {
				colMeans(l)
			})
			
			line_centers <- do.call("rbind", line_centers)
			line_centers <- SpatialPointsDataFrame(line_centers, data.frame(ID=1:nrow(line_centers)))
			line_centers_co <- coordinates(line_centers)
			
			
			line_fit <- fit_polylines(line_centers, sep.dist = 1e6, verbose = FALSE)
			line_fit_co <- coordinates(line_fit)[[1]][[1]]
			line_fit_a <- get_direction_angle(line_fit_co)
			adiff <- angle_diff(line_fit_a, a_rw)
			if (adiff > 90) line_fit_co <- line_fit_co[nrow(line_fit_co):1, ]
			
			id_line_fit <- paste(round(line_fit_co[,1], 2), round(line_fit_co[,2], 2), sep="_")
			id_line_centers <- paste(round(line_centers_co[,1], 2), round(line_centers_co[,2], 2), sep="_")
			
			stopifnot(setequal(id_line_fit, id_line_centers))
			
			p2 <- p2[match(id_line_centers, id_line_fit)]
			
		}
		sapply(p2, Line)
	}, co, rns, SIMPLIFY=FALSE)
	
	polyLines <- mapply(Lines, co2, ID=lines$ID, SIMPLIFY=FALSE)
	
	shp <- SpatialLines(polyLines, proj4string=lines@proj4string)
	shp2 <- SpatialLinesDataFrame(shp, data=lines@data, match.ID=FALSE)
	shp2
}
