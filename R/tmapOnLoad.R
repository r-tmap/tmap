.onLoad = function(...) {
	options(tmap.style = "white", tmap.mode = "plot", mode_pool = NULL, tmap.design.mode = FALSE,
			tmap.devel.mode = FALSE)

	# Resolve the leaflet marker icon on the user's machine rather than at
	# package build time. Calling marker_icon() at top level freezes the
	# build server's leaflet path into the binary (e.g. D:/RCompile/...).
	if (requireNamespace("leaflet", quietly = TRUE)) {
		opts = .defaultTmapOptions
		opts$value.const$shape.markers = marker_icon()
		utils::assignInMyNamespace(".defaultTmapOptions", opts)
	}

	assign("defaultTmapOptions", .defaultTmapOptions, envir = .TMAP)
	assign("tmapOptions", .defaultTmapOptions, envir = .TMAP)

	assign("last_map", NULL, envir = .TMAP)
	assign("last_map_new", NULL, envir = .TMAP)
	# flag for old v3 code
	assign("v3", FALSE, envir = .TMAP)
	assign("tips", sample(.tips), envir = .TMAP)
	assign("tip_nr", 1L, envir = .TMAP)
	assign("tmapStyles", .defaultTmapStyles, envir = .TMAP)
	.TMAP$round_to = as.vector(sapply((-9):9, function(i) {
		sapply(c(1, 2.5, 5), function(j) {
			j*10^i
		})
	})) # needed for pretty ticks for continuous scale with trans enabled (like log scale)
	.TMAP$mode_last = "view"
	.TMAP_GRID$maptiles_urls = character(0) # needed for caching
	fill_providers()
}

.tips <- c(
	"tmap works with {.href [map variables](https://r-tmap.github.io/tmap/articles/basics_vv)}.",
	"tmap can be {.href [extended](https://r-tmap.github.io/tmap/articles/adv_extensions)} in several ways.",
	"{.href [Charts](https://r-tmap.github.io/tmap/articles/basics_charts)} can be added next to legends.",
	"Layout {.href [styles](https://r-tmap.github.io/tmap/articles/adv_options#setting-options-and-styles)} can be created.",
	"Automatic {.href [map projection](https://r-tmap.github.io/tmap/articles/foundations_crs)} recommendations can be set via {.code tm_crs(crs = {.str auto})}.",
	"Layout can be adjusted to look identical as {.href [ggplot2](https://r-tmap.github.io/tmap/articles/versus_ggplot2#mimicking-ggplot2-layout)}.",
	"In {.str plot} mode, a special design-mode can be enabled via {.code tmap_design_mode()}."
)


#' @rdname tmap_providers
#' @export
.tmap_providers <- new.env(FALSE, parent=globalenv())


#' @export
#' @rdname tmap_internal
.TMAP = new.env(FALSE, parent = globalenv())

#' @export
#' @rdname tmap_internal
.TMAP_LEAFLET = new.env(FALSE, parent = globalenv())

#' @export
#' @rdname tmap_internal
.TMAP_GRID = new.env(FALSE, parent = globalenv())


fill_providers = function() {
	rm(list = ls(envir = .tmap_providers), envir = .tmap_providers)
	y = tmap_providers(as.list = TRUE)
	list2env(y, envir = .tmap_providers)
}

