#' @rdname tmap_leaflet
#' @name tmap_grid
#' @export
tmap_grid = function(x,
					 show = FALSE,
					 ...) {
	current_mode = getOption("tmap.mode")
	on.exit({
		options(tmap.mode = current_mode)
	})
	options(tmap.mode = "plot")
	print.tmap(x, show=show, ...)
}

#' Export tmap to the format of the used graphics mode.
#' 
#' Export tmap to the format of the used graphics mode. `tmap_grid` returns a \code{\link[grid::grob]{grob}} object (\code{"plot\" mode}) and `tmap_leaflet` a \code{\link{leaflet::leaflet}} object (\code{"view"} mode).
#' 
#' @param x description
#' @param show show the map?
#' @param ... arguments passed on the \code{\link{print.tmap}}
#' @rdname tmap_leaflet
#' @name tmap_leaflet
#' @return `tmap_grid` returns a \code{\link[grid::grob]{grob}} object (\code{"plot\" mode}) and `tmap_leaflet` a \code{\link{leaflet::leaflet}} object (\code{"view"} mode). In case small multiples are shown, a list is returned.
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
	print.tmap(x, show=show, ...)
}
