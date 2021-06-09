tmap_pal_name_compress = function(x) tolower(gsub("[-, \\,, (, ), \\ ]",  "", x))


#' List of supported color palettes
#' 
#' List of supported color palettes
#' 
#' @export
tmap_pals = local({
	hcl_pals = grDevices::hcl.pals()
	base_pals = grDevices::palette.pals()
	pals = ls(asNamespace("pals"))[!(substr(ls(asNamespace("pals")), 1, 4) %in% c("pal.", "get."))]
	list(base_hcl = structure(as.list(hcl_pals), names = tmap_pal_name_compress(hcl_pals)),
		 base_pal = structure(as.list(base_pals), names = tmap_pal_name_compress(base_pals)),
		 pals = structure(as.list(pals), names = pals))
})	



tmap_is_palette = function(x) {
	tmap_pal_names = do.call(c, c(lapply(tmap_pals, names), list(use.names = FALSE)))
	tmap_pal_name_compress(x) %in% tmap_pal_names
}


tmapGetPalette = function(x, n) {
	# todo: "Set 2" occurs in hcl and palette
	
	x = tmap_pal_name_compress(x)
	
	if (x %in% names(tmap_pals$base_hcl)) {
		grDevices::hcl.colors(n = n, palette = x)
	} else if (x %in% tmap_pals$base_pal) {
		grDevices::palette.colors(n = n, palette = x)
	} else if (x %in% tmap_pals$pals) {
		if (!requireNamespace("pals")) stop("Package pals required.", call. = FALSE)
		getFromNamespace(x, ns = "pals")(n)
	} else {
		stop("Unknown x")
	}
}