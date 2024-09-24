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
	assign("tmapFormats", .defaultTmapFormats, envir = .TMAP)

	.TMAP$round_to = as.vector(sapply((-9):9, function(i) {
			sapply(c(1, 2.5, 5), function(j) {
				j*10^i
			})
	})) # needed for pretty ticks for continuous scale with trans enabled (like log scale)

	if (packageVersion("cols4all") < "0.8") {
		cols4all = list(area7 = c("#FF9D9A", "#77AADD", "#F1CE63", "#2CA02C", "#B07AA1", "#9EDAE5", "#CC6677"),
						area8 = c("#CC6677", "#AEC7E8", "#44BB99", "#B07AA1", "#BBCC33", "#FFAABB", "#B6992D", "#98DF8A"),
						area9 = c("#EE8866", "#88CCEE", "#2CA02C", "#B07AA1", "#F1CE63", "#FFAABB", "#6699CC", "#44BB99", "#CC6677"),
						area7d = c("#72190E", "#332288", "#225555", "#997700", "#437DBF", "#994F88", "#666633"),
						area8d = c("#663333", "#1F77B4", "#225555", "#994F88", "#997700", "#332288", "#666633", "#661100"),
						area9d = c("#72190E", "#1965B0", "#225555", "#994F88", "#997700", "#332288", "#666633", "#663333", "#437DBF"),
						line7 = c("#1F77B4", "#2CA02C", "#E73F74", "#6699CC", "#994F88", "#117733", "#D37295"),
						line8 = c("#DC050C", "#1F77B4", "#117733", "#994F88", "#999933", "#D37295", "#6699CC", "#E73F74"),
						line9 = c("#EE3377", "#1F77B4", "#117733", "#CF1C90", "#999933", "#994455", "#6699CC", "#D37295", "#DC050C"),
						friendly5 = c("#CC6677", "#F1CE63", "#117733", "#99DDFF", "#9467BD"),
						friendly7 = c("#E65518", "#F2B701",  "#009988", "#88CCEE", "#9467BD","#225522", "#882255"),
						friendly9 = c("#E73F74", "#F1CE63", "#99DDFF", "#9467BD", "#009988", "#882255", "#225522", "#4B4B8F", "#999933"),
						friendly11 = c("#E73F74", "#F1CE63", "#77AADD", "#9467BD", "#AAAA00", "#FF9D9A", "#99DDFF", "#B07AA1", "#225522", "#882255", "#4B4B8F"),
						friendly13 = c("#E73F74", "#F1CE63", "#77AADD", "#009988", "#9467BD", "#FF9D9A", "#99DDFF", "#AAAA00", "#225522", "#882255", "#997700", "#4B4B8F", "#8C564B"))
		cols4all::c4a_load(cols4all::c4a_data(cols4all, types = "cat", series = "cols4all"), overwrite = TRUE)
	}


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
