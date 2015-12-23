#' @rdname double_line
#' @param offset offset from the original lines
#' @export
offset_line <- function(shp, offset) {
	isLeft <- offset<0
	double_line(shp, width=abs(offset*2), sides=ifelse(isLeft, "left", "right"))
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
	ns <- length(shp)
	hasData <- ("data" %in% slotNames(shp))
		
	# determine feature ids per line segment
	linesCount <- sapply(shp@lines, length)
	IDs <- mapply(rep, 1:ns, linesCount)

	# process width and sides arguments
	ws <- rep(width, length.out=ns)
	ws2 <- ws[IDs]
	margin=max(ws)/100
	ss <- rep(sides, length.out=ns)
	
	# let each line segment be seperate SL(DF)
	shp2 <- disaggregate(shp)
	shp2s <- split(shp2, f=factor(get_IDs(shp2)))

	# Per line segment, determine left- and right-handside part
	shp7s <- mapply(FUN =  function(s2, w) {
		if (w==0) {
			return(list(L=s2, R=s2))
		}
		
		s3 <- suppressWarnings(gBuffer(s2, width=w, capStyle="SQUARE"))
		s4 <- as(s3, "SpatialLines")
		
		s5 <- suppressWarnings(gBuffer(s2,width= w+margin,capStyle="FLAT"))
		
		s6 <- gIntersection(s4, s5)
		
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
	}, shp2s, ws2, SIMPLIFY = FALSE)
	
	# get non-NULL argument selection
	selL <- !sapply(shp7s, function(s) is.null(s[["L"]]))
	selR <- !sapply(shp7s, function(s) is.null(s[["R"]]))
	
	# stack them
	shpL <- do.call("sbind", lapply(shp7s, "[[", "L"))
	shpR <- do.call("sbind", lapply(shp7s, "[[", "R"))
	
	
	Lns <- mapply(function(id, s) {
		L <- if (s %in% c("left", "both")) {
			if (!any(IDs[selL]==id)) {
				NULL
			} else lapply(shpL@lines[IDs[selL]==id], function(l) l@Lines)
		} else NULL
		R <- if (s %in% c("right", "both")) {
			if (!any(IDs[selR]==id)) {
				NULL
			} else lapply(shpR@lines[IDs[selR]==id], function(l) l@Lines)
		} else NULL
		
		if (is.null(L) && is.null(R)) {
			NULL
		} else Lines(do.call("c", c(L, R)),ID = id)	
	}, 1:ns, ss, SIMPLIFY=FALSE)
	
	Lns_sel <- !sapply(Lns, is.null)
	
	if (!any(Lns_sel)) stop("Unable to create offset lines")
	
	shp8 <- SpatialLines(Lns[Lns_sel], proj4string = shp@proj4string)
	shp9 <- if (hasData) {
		SpatialLinesDataFrame(shp8, data=shp@data[Lns_sel,], match.ID = FALSE)
	} else shp8
	
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


