# envir = environment()
.onLoad = function(...) {
	options(tmap.style = "white", tmap.mode = "plot", tmap.design.mode = FALSE,
			tmap.devel.mode = FALSE)
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

	assign("last_map", NULL, envir = .TMAP)
	assign("last_map_new", NULL, envir = .TMAP)


	# flag for old v3 code
	assign("v3", FALSE, envir = .TMAP)


	assign("tmapStyles", .defaultTmapStyles, envir = .TMAP)

	.TMAP$round_to = as.vector(sapply((-9):9, function(i) {
			sapply(c(1, 2.5, 5), function(j) {
				j*10^i
			})
	})) # needed for pretty ticks for continuous scale with trans enabled (like log scale)

}

#' @export
#' @keywords internal
#' @rdname tmap_internal
.TMAP = new.env(FALSE, parent = globalenv())

#' @export
#' @keywords internal
#' @rdname tmap_internal
.TMAP_LEAFLET = new.env(FALSE, parent = globalenv())

#' @export
#' @keywords internal
#' @rdname tmap_internal
.TMAP_GRID = new.env(FALSE, parent = globalenv())
