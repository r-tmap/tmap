one_line_per_lines <- function(shp) {
	k <- length(shp)
	lns <- sapply(shp@lines, function(l)length(l@Lines))
	ID <- do.call("c", mapply(rep, 1:length(lns), lns, SIMPLIFY=FALSE))
	
	res <- lapply(shp@lines, function(l) {
		L <- lapply(1:length(l@Lines), function(i) {
			l2 <- l@Lines[[i]]
			ID <- paste(l@ID, i)
			Lines(l2, ID=ID)
		})
	})
	res <- do.call("c", res)
	shp2 <- SpatialLines(res, proj4string = shp@proj4string)
	list(shp=SpatialLinesDataFrame(shp2, data=shp@data[ID, , drop=FALSE], match.ID=FALSE),
		 id=ID)
}



