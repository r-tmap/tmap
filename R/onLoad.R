.onLoad <- function(...) {
	options(tmap.style="white", tmap.mode="plot", tmap.design.mode = FALSE)
	internet <- working_internet()
	assign("internet", internet, envir = .TMAP_CACHE)
	assign("underCoverage", NULL, envir = .TMAP_CACHE)
	assign("overCoverage", NULL, envir = .TMAP_CACHE)
	assign("last_map", NULL, envir = .TMAP_CACHE)
	assign("last_map_new", NULL, envir = .TMAP_CACHE)
	assign("tmapOptions", .defaultTmapOptions, envir = .TMAP_CACHE)
	assign("tmapStyles", .defaultTmapStyles, envir = .TMAP_CACHE)
	assign("tmapFormats", .defaultTmapFormats, envir = .TMAP_CACHE)
	tips_order <- determine_tips_order()
	
	assign("tmapTipsIds", tips_order, envir = .TMAP_CACHE)
	assign("tmapTipsId", 1, envir = .TMAP_CACHE)
	
} 

.onAttach <- function(libname, pkgname) {
	packageStartupMessage("Breaking News: tmap 3.x is retiring. Please test v4, e.g. with\nremotes::install_github('r-tmap/tmap')")
}

.TMAP_CACHE <- new.env(FALSE, parent=globalenv())

.crs_longlat <- sf::st_crs(4326)
.crs_merc <- sf::st_crs(3857)
