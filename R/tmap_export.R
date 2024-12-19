#' Export tmap to the format of the used graphics mode
#'
#' * `tmap_grob()` returns a [`grob`][grid::grob()] object (`"plot" mode`)
#' * `tmap_leaflet()` a [`leaflet`][leaflet::leaflet()] object (`"view"` mode).
#'
#' @param x a tmap object.
#' @param asp,scale the desired aspect ratio and scale of the map. Only applicable for `"plot"` mode.
#' @param show show the map?
#' @inheritDotParams print.tmap
#' @return
#' * `tmap_grob()` returns a [`grob`][grid::grob()] object (`"plot"` mode)
#' * `tmap_leaflet()` a [`leaflet`][leaflet::leaflet()] object (`"view"` mode).
#'   In case small multiples are shown, a list is returned.
#' @export
#' @examples
#' map = tm_shape(World) + tm_polygons()
#' tmap_leaflet(map, show = TRUE)
tmap_leaflet = function(x,
						#mode = "view",
						show = FALSE,
						#add.titles = TRUE,
						#in.shiny = FALSE,
						...) {
	current_mode = getOption("tmap.mode")
	on.exit({
		options(tmap.mode = current_mode)
	})
	options(tmap.mode = "view")
	print.tmap(x, show = show, ...)
}

#' @rdname tmap_leaflet
#' @export
tmap_grob = function(x,
					 asp = NA,
					 scale = 1,
					 show = FALSE,
					 ...) {
	current_mode = getOption("tmap.mode")
	on.exit({
		options(tmap.mode = current_mode)
	})
	options(tmap.mode = "plot")
	print.tmap(x + tm_options(asp = asp, scale = scale), show = show, ...)
}
