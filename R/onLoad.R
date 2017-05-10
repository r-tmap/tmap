.onLoad <- function(...) {
	options(tmap.unit="metric", tmap.style="white", tmap.mode="plot", tmap.limits=c(facets.plot=64, facets.view=4))
	internet <- working_internet()
	assign(".internet", internet, envir = .TMAP_CACHE)
	assign(".underCoverage", NULL, envir = .TMAP_CACHE)
	assign(".overCoverage", NULL, envir = .TMAP_CACHE)
	assign(".last_map", NULL, envir = .TMAP_CACHE)
	assign(".last_map_new", NULL, envir = .TMAP_CACHE)
} 

.TMAP_CACHE <- new.env(FALSE, parent=globalenv())

.CRS_longlat <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0", doCheckCRSArgs = FALSE)
.CRS_merc <- CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs", doCheckCRSArgs = FALSE)

get_proj4_version <- function() {
	PROJ4_version <- rgdal::getPROJ4VersionInfo()
	vid <- gregexpr("PJ_VERSION: ", PROJ4_version, fixed = TRUE)[[1]][1] + 12
	as.integer(substr(PROJ4_version, vid, nchar(PROJ4_version)-1))
}
