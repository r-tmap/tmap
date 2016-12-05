data(World)


library(rmapshaper)

w2 <- ms_simplify(World, keep=.2, keep_shapes=TRUE, explode=TRUE)


qtm(World, fill="HPI", borders.lwd=2)

qtm(w2, fill="HPI", borders.lwd=2)

x <- ms_innerlines(World)

data(NLD_muni)
NLD_muni2 <- ms_simplify(NLD_muni, keep=.1, keep_shapes=TRUE, explode=TRUE)

qtm(NLD_muni2, fill="origin_non_west")


NLD_prov2 <- unionSpatialPolygons(NLD_muni, IDs = NLD_muni$province)
NLD_prov2 <- rgeos::gUnaryUnion(NLD_muni, NLD_muni$province)

str(NLD_prov2@data)

simplify_map <- function(shp, fact = 0.1, drop.small.units = TRUE , drop.small.subunits = TRUE, ...) {
	keep_shapes <- !drop.small.units
	explode <- keep_shapes && !drop.small.subunits
	
	if (explode) {
		shp$UNIT__NR <- 1L:length(shp)
	}
	
	x <- ms_simplify(shp, keep=fact, keep_shapes=keep_shapes, explode=explode, ...)
	
	if (explode) {
		x <- union_map(x, by="UNIT__NR")
	}
	
	x@data[, c("rmapshaperid", "UNIT__NR")] <- list()
	
	
	
}

