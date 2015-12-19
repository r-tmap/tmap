#' @rdname double_line
#' @param offset offset from the original lines
#' @export
offset_line <- function(shp, offset) {
	if (offset<0) {
		double_line(shp, width=-offset*2, sides="left")
	} else {
		double_line(shp, width=offset*2, sides="right")
	}
}
	

#' Create a double line or offset line
#' 
#' Create a double line or offset line. The double line can be useful for visualizing two-way tracks or emulating objects such as railway tracks. The offset line can be useful to prevent overlapping of spatial lines.
#' 
#' @param shp SpatialLines(DataFrame)
#' @param width width between the left and righthand side
#' @param sides character value that specifies which sides are selected: \code{"both"}, \code{"left"}, or \code{"right"}. The default is \code{"both"}. For the other two options, see also the shortcut function \code{offset_line}.
#' @name double_line
#' @rdname double_line
#' @export
#' @import sp
#' @importFrom rgeos gBuffer gIntersection
double_line <- function(shp, width, sides="both") {
	margin=width/100
	hasData <- ("data" %in% slotNames(shp))
		
	linesCount <- sapply(shp@lines, length)
	IDs <- mapply(rep, 1:length(shp), linesCount)
	
	shp2 <- disaggregate(shp)
	shp2s <- split(shp2, f=factor(get_IDs(shp2)))
	
	
	shp3 <- gBuffer(shp2, width=width, capStyle="SQUARE", byid=TRUE)
	shp4 <- as(shp3, "SpatialLines")
	
	shp5 <- gBuffer(shp2,width= width+margin,capStyle="FLAT", byid=TRUE)
	
	shp4s <- split(shp4, f=factor(get_IDs(shp4)))
	shp5s <- split(shp5, f=factor(get_IDs(shp5)))
	
	shp6s <- mapply(gIntersection, shp4s, shp5s)
	
	shp7s <- mapply(FUN =  function(s2, s6) {
		s6s <- disaggregate(s6)
		
		co2 <- s2@lines[[1]]@Lines[[1]]@coords
		
		a2 <- sapply(1:(nrow(co2)-1), function(i) {
			get_direction_angle(co2[c(i,i+1),])
		})
		a2 <- c(a2, a2[length(a2)])
		
		dirs <- sapply(s6s@lines, function(L) {
			co6 <- L@Lines[[1]]@coords
			
			co6 <- co6[seq(1, nrow(co6), length.out=5),]
			
			a6 <- apply(co6, MARGIN = 1, FUN = function(coi) {
				id <- which.min(sqrt((coi[1] - co2[,1])^2 + (coi[2] - co2[,2])^2))
				angle_diff(get_direction_angle(rbind(co2[id,], coi)), a2[id])
			})
			sum(angle_diff(a6, 90, absolute = TRUE) < 90) >= 3 
		})
		
		id <- get_IDs(s2)
		
		if (any(dirs)) {
			lnsL <- do.call("c", lapply(s6s@lines[dirs], function(l) l@Lines))
			slL <- SpatialLines(list(Lines(lnsL, ID=id)), proj4string = s2@proj4string)
			sldfL <- SpatialLinesDataFrame(slL, s2@data) 
		} else {
			sldfL <- NULL
		}
		
		if (any(!dirs)) {
			lnsR <- do.call("c", lapply(s6s@lines[!dirs], function(l) l@Lines))
			slR <- SpatialLines(list(Lines(lnsR, ID=id)), proj4string = s2@proj4string)
			sldfR <- SpatialLinesDataFrame(slR, s2@data) 
		} else {
			sldfR <- NULL
		}
		
		list(L=sldfL, R=sldfR)
	}, shp2s, shp6s, SIMPLIFY = FALSE)
	
	
	selL <- !sapply(shp7s, function(s) is.null(s[["L"]]))
	selR <- !sapply(shp7s, function(s) is.null(s[["R"]]))
	
	shpL <- do.call("sbind", lapply(shp7s, "[[", "L"))
	shpR <- do.call("sbind", lapply(shp7s, "[[", "R"))
	
	llL <- lapply(1:length(shp), function(id) {
		if (!any(IDs[selL]==id)) return(NULL)
		Lines(do.call("c", lapply(shpL@lines[IDs[selL]==id], function(l) l@Lines)),ID = id)
	})
	selL2 <- !sapply(llL, is.null)
	shpL2 <- SpatialLines(llL[selL2], proj4string = shp@proj4string)
	shpL3 <- if (hasData) {
		SpatialLinesDataFrame(shpL2, data=shp@data[selL2,], match.ID = FALSE)
	} else shpL2
	
	llR <- lapply(1:length(shp), function(id) {
		if (!any(IDs[selR]==id)) return(NULL)
		Lines(do.call("c", lapply(shpR@lines[IDs[selR]==id], function(l) l@Lines)),ID = id)
	})
	selR2 <- !sapply(llR, is.null)
	shpR2 <- SpatialLines(llR[selR2], proj4string = shp@proj4string)
	shpR3 <- if (hasData) {
		SpatialLinesDataFrame(shpR2, data=shp@data[selR2,], match.ID = FALSE)
	} else shpR2
	
	selB <- selL | selR
	shpB <- sbind(shpL3, shpR3)
	IDb <- c(which(selL2), which(selR2))
	
	llB <- lapply(1:length(shp), function(id) {
		if (!any(IDb==id)) return(NULL)
		Lines(do.call("c", lapply(shpB@lines[IDb==id], function(l) l@Lines)),ID = id)
	})
	selB2 <- !sapply(llB, is.null)
	shpB2 <- SpatialLines(llB[selB2], proj4string = shp@proj4string)
	shpB3 <- if(hasData) {
		SpatialLinesDataFrame(shpB2, data=shp@data[selB2,], match.ID = FALSE)	
	} else shpB2
	
	if (sides=="left") {
		shpL3
	} else if (sides=="right") {
		shpR3
	} else {
		shpB3
	}
}


# get angle from the coordinates of a line (only considers endpoints)
get_direction_angle <- function(co) {
	p1 <- co[1,]
	p2 <- co[nrow(co),]
	
	a <- atan2(p2[2] - p1[2], p2[1] - p1[1]) * 180 / pi
	a %% 360
}


# calculate the difference between two angles
angle_diff <- function(a1, a2, absolute=FALSE) {
	y <- a2 / 180 * pi
	x <- a1 / 180 * pi
	a <- atan2(sin(x-y), cos(x-y)) * 180 / pi
	if (absolute) abs(a) else a
}

# get the endpoints per line, as well as the angle of the head and tail
end_points <- function(shp) {
	lapply(shp@lines, function(l) {
		lapply(l@Lines, function(ll) {
			k <- nrow(ll@coords)
			co <- ll@coords[c(1, k), ]
			co_head <- ll@coords[1:2, ]
			co_tail <- ll@coords[(k-1):k, ]
			
			dir_head <- get_direction_angle(co_head)
			dir_tail <- get_direction_angle(co_tail)
			
			if (all(co[1,]==co[2,])) {
				dir_diff <- angle_diff(dir_head, dir_tail, absolute = TRUE)
				opposite <- (dir_diff > 90)
				
				dir_diff2 <- angle_diff(dir_head, dir_tail, absolute = FALSE) + ifelse(opposite, 180, 0)
				
				dir_head <- (dir_head - dir_diff2/2) %% 360
				dir_tail <- (dir_head + ifelse(opposite, 180, 0)) %% 360
			}
			
			cbind(co, c(dir_head, dir_tail))
		})
	})
}


