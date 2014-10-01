get_lengths <- function(shp, digits=3) {
	round(SpatialLinesLengths(shp, longlat=FALSE)/1000, digits=digits)
}

round_km <- function(x, digits=3, multiply=1e-3) {
	round(x*multiply, digits=digits)
}

# get angle from the coordinates of a line (only considers endpoints)
get_direction_angle <- function(co) {
	p1 <- co[1,]
	p2 <- co[nrow(co),]
	
	a <- atan2(p2[2] - p1[2], p2[1] - p1[1]) * 180 / pi
	if (a < 0) a <- a + 360
	a
}

get_cycle_direction <- function(co) {
	p1 <- co[1,]
	p2 <- co[floor(nrow(co)/2),]
	a1 <- get_direction_angle(co[c(1, floor(nrow(co)*.45)), ])
	a2 <- get_direction_angle(co[c(1, floor(nrow(co)*.55)), ])
	if ((a1 > a2) && (a1-a2 < 180)) return("CW") else return("CCW")
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
		   NW=135,
		   NULL)
}

# return the absolute difference of two angles in degrees
angle_diff <- function(a1, a2) {
	y <- a2 / 180 * pi
	x <- a1 / 180 * pi
	abs(atan2(sin(x-y), cos(x-y)) * 180 / pi)
}


# 
set_directions <- function(lines, main_direction=TRUE, directions=directions) {
	co <- coordinates(lines)
	
	rns <- lines$ID
	
	co2 <- mapply(function(p, rn) {
		cat("rijksweg ", rn, "\n")
		
		#if (rn=="A10") browser()
		
		dir_string <- directions$directionR[directions$roadname==rn]
		a_rw <- get_NOZW_angle(dir_string)
		
		if (is.null(a_rw)) {
			if (length(p)!=1) stop("rondweg heeft meerdere segmenten")
			p1 <- p[[1]]
			dir1 <- get_cycle_direction(p1)
			if (dir1 != dir_string) p1 <- p1[nrow(p1):1,]
			p2 <- list(p1)	
			
		} else {
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
			
		}
			
		
		sapply(p2, Line)
	}, co, rns, SIMPLIFY=FALSE)
	
	polyLines <- mapply(Lines, co2, ID=lines$ID, SIMPLIFY=FALSE)
	
	shp <- SpatialLines(polyLines, proj4string=lines@proj4string)
	shp2 <- SpatialLinesDataFrame(shp, data=lines@data, match.ID=FALSE)
	shp2
}


create_meter_list <- function(lines, dist, direction) {
	list1 <- mapply(function(co, id, dr) {
		co <- lapply(co, function(co2) {
			co2 <- as.data.frame(co2)
			names(co2) <- c("x", "y")
			co2$roadname <- id
			co2$direction <- dr
			co2$meter <- seq(0, by=dist, length.out=nrow(co2))
			co2$mark <- ""
			co2$mark[c(1, nrow(co2))] <- c("BEGIN", "EIND")
			co2
		})
		if (length(co)>1) {
			for (i in 2:length(co)) {
				co[[i]]$meter <- co[[i]]$meter + co[[i-1]]$meter[nrow(co[[i-1]])]
			}
		}
		co
	}, coordinates(lines), lines$ID, direction)
	
	list2 <- lapply(list1, function(d) {
		do.call("rbind", d)
	})
	names(list2) <- as.character(lines$ID)
	list2
}


write_point_info <- function(afr, drw, info) {
	res <- mapply(function(a, d) {
		for (i in 1:nrow(a)) {
			id <- which(a$x[i]==d$x & a$y[i]==d$y)
			stopifnot(length(id)>0)
			d$mark[id] <- writeMark(d$mark[id], info)
		}
		d
	}, afr, drw, SIMPLIFY=FALSE)
}


writeMark <- function(current, nw) if (current=="") nw else paste0(current, ", ", nw)


search_POB <- function(lines, drw) {
	res <- lapply(drw$ID, function(id) {
		lines_sel <- lines[lines$roadname==id, ]
		drw_sel <- drw[drw$ID==id,]
		
		linesList <- list()
		for (i in 1:length(lines_sel)) {
			lines_sel_co <- coordinates(lines_sel[i,])[[1]]
			lines_sel_shp <- lapply(lines_sel_co, SpatialPoints)
			# loop through linesitten
			lines_L <- lapply(lines_sel_shp, function(a)map_points_to_line(shp.points=a, shp.lines=drw_sel))
			lines_L <- lapply(lines_L, function(a) {
				d_first <- mean(a$d[1:2])
				d_last <- mean(a$d[(nrow(a)-1):nrow(a)])
				id <- ifelse(d_first < d_last, 1, nrow(a))
				a[id,]
			})
			linesList[[i]] <- do.call("rbind", lines_L)
		}
		linesList <- do.call("rbind", linesList)
		linesList$x <- 0
		linesList$y <- 0
		co <- coordinates(drw_sel)[[1]]
		for (i in 1:nrow(linesList)) {
			linesList$x[i] <- co[[linesList$id2[i]]][linesList$id3[i], 1]
			linesList$y[i] <- co[[linesList$id2[i]]][linesList$id3[i], 2]
		}
		linesList
	})
	names(res) <- drw$ID
	res
}


search_points <- function(pnts, drw) {
	res <- lapply(drw$ID, function(id) {
		pnts_sel <- pnts[pnts$roadname==id, ]
		drw_sel <- drw[drw$ID==id,]
		
		pntsList <- map_points_to_line(shp.points=pnts_sel, shp.lines=drw_sel)
		
		pntsList$x <- 0
		pntsList$y <- 0
		co <- coordinates(drw_sel)[[1]]
		for (i in 1:nrow(pntsList)) {
			pntsList$x[i] <- co[[pntsList$id2[i]]][pntsList$id3[i], 1]
			pntsList$y[i] <- co[[pntsList$id2[i]]][pntsList$id3[i], 2]
		}
		pntsList
	})
	names(res) <- drw$ID
	res	
}

compact_info <- function(info, edit=FALSE) {
	info <- info[info$mark!="", ]
	start <- substr(info$mark, 1, 3)
	lengths <- nchar(info$mark)
	if (edit) {
		info$mark <- ifelse(lengths>6,
					ifelse(start=="LUS", "Meerdere lussen", "Meerdere punten"), info$mark)
		info$mark <- factor(info$mark, levels=c("BEGIN", "EIND", "OPRIT", "AFRIT", "LUS", "Meerdere lussen", "Meerdere punten"))
	} 
	info
}




write_info <- function(drw, compact = TRUE, path) {
	for (i in 1:length(drw)) {
		d <- drw[[i]]
		if (compact) d <- compact_info(d)
		d$x <- sprintf("%.3f",d$x)
		d$y <- sprintf("%.3f",d$y)
		d$meter <- sprintf("%.3f",round_km(d$meter))
		direction <- d$direction[1]
		filename <- file.path(path, paste0("info_", names(drw)[i], "_", direction, ".csv"))
		write.table(d, file=filename, sep="\t", row.names=FALSE, quote=FALSE)
	}
	invisible()
}


plot_per_rw <- function(drw, info, scale=.1, path) {
	for (rn in drw$ID) {
		drw_sel <- drw[drw$ID==rn,]
		info_sel <- compact_info(info[[rn]], edit=TRUE)
		
		
		drive <- info_sel$direction[1]
		
		points <- SpatialPointsDataFrame(coords = info_sel[,1:2], data=info_sel, proj4string = CRS(get_projection(drw)))
		basen <- paste0(rn, "_", drive)
		filename <- file.path(path, paste0(basen, ".pdf"))
		
		b <- drw_sel@bbox
		dx <- b[1,2] - b[1,1]
		dy <- b[2,2] - b[2,1]
		
		if (dx>dy) {
			width <- 10
			height <- min(ceiling(10 * dy / dx) + 1, 10)
		} else {
			height <- 10
			width <- min(ceiling(10 * dx / dy) + 1, 10)
		}

		
		
		pdf(filename, width=width, height=height)
		print(tm_shape(drw_sel) +
			tm_lines(col="black") +
			tm_shape(points) +
			tm_bubbles(col= "mark", palette = "Paired") +
			tm_shape(points[points$mark!="LOOP", ]) +
			tm_text(text = "meter") +
			tm_layout(basen, scale = scale))
		dev.off()
	}
}

plot_google <- function(drw, info, rn) {
	require(RColorBrewer)
	require(plotKML)
	
	info_sel <- compact_info(info[[rn]], edit=TRUE)
	direction <- info_sel$direction[1]
	points <- SpatialPointsDataFrame(coords = info_sel[,1:2], data=info_sel, proj4string = CRS(tmap:::get_proj4_code("rd")))
	points <- set_projection(points, "longlat")
	
	baseNamePoints <- paste0(rn, "_", direction, "_points")
	baseNameLines <- paste0(rn, "_", direction, "_route")
	plotKML(points["mark"], folder.name=baseNamePoints, file.name=paste0("../applications/traffic_NLD/kml/", baseNamePoints, ".kml"))
	
	drw_sel <- set_projection(drw[drw$ID==rn,], "longlat")
	plotKML(drw_sel["ID"], colour="steelblue" , width=6, folder.name=baseNameLines, file.name=paste0("../applications/traffic_NLD/kml/", baseNameLines, ".kml"))
	
}
