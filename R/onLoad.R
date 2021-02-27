.onLoad <- function(...) {
	options(tmap.style="white", tmap.mode="plot", tmap.design.mode = FALSE)
	assign("tmapOptions", .defaultTmapOptions, envir = .TMAP)
	assign("tmapStyles", .defaultTmapStyles, envir = .TMAP)
	assign("tmapFormats", .defaultTmapFormats, envir = .TMAP)
} 

.TMAP <- new.env(FALSE, parent=globalenv())
.TMAP_LEAFLET <- new.env(FALSE, parent=globalenv())
.TMAP_GRID <- new.env(FALSE, parent=globalenv())
