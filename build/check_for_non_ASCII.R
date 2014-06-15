check_nonASCII <- function() {
	data(World)
	data(Europe)
	data(NLD_prov)
	data(NLD_muni)
	data(NLD_ageGroups)
	data(rivers)
	data(cities)
	data(airports)
	
	nms <- c("World", "Europe", "NLD_prov", "NLD_muni", "NLD_ageGroups", "rivers", "cities", "airports")
	
	lapply(nms, function(nm) {
		d <- get(nm)
		if (inherits(d, "Spatial")) d <- d@data
		lapply(d, function(col) {
			x <- grep("I_WAS_NOT_ASCII", iconv(as.character(col), "latin1", "ASCII", sub="I_WAS_NOT_ASCII"))	
		})
	})
	
	
}

check_nonASCII()



x <- grep("I_WAS_NOT_ASCII", iconv(airports$name[268], "latin1", "ASCII", sub="I_WAS_NOT_ASCII"))