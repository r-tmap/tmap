# envir <- environment()
.onLoad <- function(...) {
	options(tmap.style="white", tmap.mode="plot", tmap.design.mode = FALSE)
	assign("tmapOptions", .defaultTmapOptions, envir = .TMAP)
	
	
	# makeActiveBinding("tmap_pals", function() {
	# 	remove_non_letters = function(x) gsub("[-, _, \\,, (, ), \\ , \\.]",  "", x)
	# 	hcl_pals = grDevices::hcl.pals()
	# 	base_pals = grDevices::palette.pals()
	# 	pals = ls(asNamespace("pals"))[substr(ls(asNamespace("pals")), 1, 4) != "pal."]
	# 	list(base_hcl = structure(as.list(hcl_pals), names = remove_non_letters(hcl_pals)),
	# 		 base_pal = structure(as.list(base_pals), names = remove_non_letters(base_pals)),
	# 		 pals = structure(as.list(pals), names = pals))
	# }, env = envir)
	
	#assign("tmapStyles", .defaultTmapStyles, envir = .TMAP)
	#assign("tmapFormats", .defaultTmapFormats, envir = .TMAP)
} 

.TMAP <- new.env(FALSE, parent=globalenv())
.TMAP_LEAFLET <- new.env(FALSE, parent=globalenv())
.TMAP_GRID <- new.env(FALSE, parent=globalenv())
