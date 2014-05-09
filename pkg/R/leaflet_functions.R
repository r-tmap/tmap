col2hex <- function(x) apply(col2rgb(x), MARGIN=2, FUN=function(y)do.call(rgb, c(as.list(y), list(maxColorValue=255))))

zoomlevel <- function(bbox) {
	x <- max(bbox[,2] - bbox[,1])
	z <- min(max(8-round(sqrt(x)*.3), 1), 18)
	z
}

get_polyID <- function(p)substr(p, 1, nchar(p)-5)

get_polygon_info <- function(shp, fill) {
	res <- lapply(shp@polygons, function(p){
		npoly <- length(p@Polygons)
		
		co <- lapply(p@Polygons, function(pp)pp@coords)
		
		co <- lapply(co, function(co2)rbind(co2, c(NA,NA)))
		co <- do.call("rbind", co)
		ids <- paste(p@ID, formatC(1:npoly, width=4, flag="0"), sep="_")
		list(co=co, ids=ids)
	})
	fill <- col2hex(fill)
	cols <- mapply(function(x, col){
		rep.int(col, length(x$ids))
	}, res, fill, SIMPLIFY=FALSE)
	
	coords <- do.call(rbind, lapply(res, function(x)x$co))
	coords <- coords[1:(nrow(coords)-1), ]
	ids <- unlist(lapply(res, function(x)x$ids))
	opts <- lapply(unlist(cols), function(x)list(fillColor=x))
	list(coords=coords, ids=ids, opts=opts)
}