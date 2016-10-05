.onLoad <- function(...) {
	options(tmap.style="white", tmap.mode="plot")
	internet <- working_internet()
	assign(".internet", internet, envir = .TMAP_CACHE)
	assign(".underCoverage", NULL, envir = .TMAP_CACHE)
	assign(".overCoverage", NULL, envir = .TMAP_CACHE)
	assign(".last_map", NULL, envir = .TMAP_CACHE)
	assign(".last_map_new", NULL, envir = .TMAP_CACHE)
} 

.TMAP_CACHE <- new.env(FALSE, parent=globalenv())
