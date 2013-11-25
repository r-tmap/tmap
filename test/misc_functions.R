getAreas <- function(shp) {
	sapply(slot(shp, "polygons"),
		   function(x) {
		   	sum(sapply(slot(x, "Polygons"), slot, "area")*
		   		(.5-sapply(slot(x, "Polygons"), slot, "hole"))*2)
		   })
}

getIDs <- function(shp) sapply(shp@polygons, function(x)x@ID)

numOfPoly <- function(shp) sapply(shp@polygons, function(x)length(x@Polygons))

middle <- function(x) sum(range(x))/2

getCenterCoor <- function(shp) {
	matrix(unlist(
		
		lapply(shp@polygons, 
			   function(p) {
			   	m <- matrix(unlist(lapply(p@Polygons, 
			   							  function(pol) c(middle(pol@coords[,1]),
			   							  				middle(pol@coords[,2]),
			   							  				pol@area))), ncol=3,byrow=TRUE)
			   	c(x=weighted.mean(m[,1], m[,3]), y=weighted.mean(m[,2], m[,3]))
			   })), ncol=2,byrow=TRUE)
	
}

flowMap <- function(shp, from, to, id="STATCODE") {
	from <- c("GM0957", "GM1955", "GM0265")
	to <- c("GM0905")
	if (!is.numeric(from)) from <- match(from, shp[[id]])
	if (!is.numeric(to)) to <- match(to, shp[[id]])
	
	n <- length(shp@polygons)
	col <- rep(NA, n)
	col[from] <- "red"
	col[to] <- "blue"
	
	
	par(mai=c(0,0,0,0))
	plot(shp, col=col)
	
	cartoMap(shp, col=col, plot.bg = NA, plot.borders = TRUE)
	
	p <- getCenterCoor(shp)
	
	x1 <- p[from, 1]
	x2 <- p[to, 1]
	y1 <- p[from, 2]
	y2 <- p[to, 2]
	
	for (i in 1:length(x1)) {
		for (j in 1:length(x2)) {
			lines(x=c(x1[i], x2[j]), y=c(y1[i], y2[j]))
		}
	}
	
}
