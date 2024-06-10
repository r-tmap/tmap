#' Export tmap to the format of the used graphics mode
#' 
#' * `tmap_grid()` returns a [`grob`][grid::grob()] object (`"plot" mode`)
#' * `tmap_leaflet()` a [`leaflet`][leaflet::leaflet()] object (`"view"` mode).
#' 
#' @param x a tmap object.
#' @param show show the map?
#' @inheritDotParams print.tmap
#' @return 
#' * `tmap_grid()` returns a [`grob`][grid::grob()] object (`"plot"` mode)
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

#' @name tmap_grid
#' @rdname tmap_leaflet
#' @export
tmap_grid = function(x,
					 show = FALSE,
					 ...) {
	current_mode = getOption("tmap.mode")
	on.exit({
		options(tmap.mode = current_mode)
	})
	options(tmap.mode = "plot")
	print.tmap(x, show = show, ...)
}
