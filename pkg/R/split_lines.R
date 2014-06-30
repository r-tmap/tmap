#' Split lines in segments of equal length.
#' 
#' Split lines in segments of equal length.
#' 
#' @param shp The shape object that contains the lines
#' @param dist Distance per segment
#' @param byid 
split_lines <- function(shp, dist=1000, byid=TRUE, simplify=TRUE) {

	 
	
}


library(sp)
Sl = SpatialLines(list(Lines(list(Line(cbind(c(1,2,3),c(3,2,2)))),
							 ID="a")))
cSl <- coordinates(Sl)
cSl
in_nrows <- lapply(cSl, function(x) sapply(x, nrow))
outn <- sapply(in_nrows, function(y) sum(y-1))
res <- vector(mode="list", length=outn)
i <- 1
for (j in seq(along=cSl)) {
	for (k in seq(along=cSl[[j]])) {
		for (l in 1:(nrow(cSl[[j]][[k]])-1)) {
			res[[i]] <- cSl[[j]][[k]][l:(l+1),]
			i <- i + 1
		}
	}
}
res1 <- vector(mode="list", length=outn)
for (i in seq(along=res))
	res1[[i]] <- Lines(list(Line(res[[i]])), as.character(i))
outSL <- SpatialLines(res1)