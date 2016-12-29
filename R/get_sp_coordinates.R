get_sp_coordinates <- function(shp, gpl, gt, bbx) {
	if (inherits(shp, "SpatialLines")) {
		
		if (gt$shape.line.center.type[1]=="segment") {
			ns <- length(shp)
			shp_lst <- one_line_per_lines(shp)
			shp <- shp_lst$shp
			attr(shp, "bbox") <- bbx
			id <- shp_lst$id
			
			aes <- intersect(names(gpl), c("line.col", "line.lwd", "text", "text.size", "text.color", "text.xmod", "text.ymod", "text_sel", "symbol.size", "symbol.col", "symbol.xmod", "symbol.ymod"))
			
			gpl[aes] <- lapply(gpl[aes], function(a) {
				if (length(a)==ns) {
					a[id]
				} else a
			})
		}
		
		if (gt$shape.line.center.type[2]=="midpoint") {
			co <- lines_midpoints(shp)@coords
			
			if (gt$shape.line.center.type[1]=="feature") {
				lC <- gCentroid(shp, byid=TRUE)@coords
				lCX <- lC[,1]
				lCY <- lC[,2]
				lens <- sapply(shp@lines, function(lns)length(lns@Lines))
				
				X <- co[,1]
				Y <- co[,2]
				ID <- do.call("c", mapply(rep, 1:length(lens), lens, SIMPLIFY=FALSE))
				Xs <- split(X, ID)
				Ys <- split(Y, ID)
				
				co <- t(mapply(function(x1, y1, x2, y2) {
					minid <- which.min(sqrt((x1-x2)^2 + (y1-y2)^2))
					c(x2[minid], y2[minid])
				}, lCX, lCY, Xs, Ys, SIMPLIFY=TRUE))
			}
		} else {
			co <- gCentroid(shp, byid=TRUE)@coords
		}
		
	} else {
		co <- coordinates(shp) # prefered over gCentroid since coordinates correspond to first (normally largest) polygon of each object
	}
	list(co=co, gpl=gpl, shp=shp)
}