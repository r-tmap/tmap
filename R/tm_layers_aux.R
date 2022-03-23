#' Map layer: basemap / overlay tiles
#' 
#' Map layer that draws tiles from a tile server. The function \code{tm_basemap} draws the tile layer as basemap, i.e. as bottom layer. In contrast,  \code{tm_tiles} draws the tile layer as overlay layer, where the stacking order corresponds with the order in which this layer is called, just like other map layers.
#' 
#' @param server name of the provider or an URL. The list of available providers can be obtained with \code{providers} (tip: in RStudio, type \code{providers$} to see the options). See \url{https://leaflet-extras.github.io/leaflet-providers/preview/} for a preview of those. When a URL is provided, it should be in template format, e.g. \code{"https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png"}. Use \code{NULL} in \code{tm_basemap} to disable basemaps.
#' @param alpha transparency level
#' @param zoom zoom level (only used in plot mode)
#' @param zindex zindex of the pane in view mode. By default, it is set to the layer number plus 400. By default, the tmap layers will therefore be placed in the custom panes \code{"tmap401"}, \code{"tmap402"}, etc., except for the base tile layers, which are placed in the standard \code{"tile"}. This parameter determines both the name of the pane and the z-index, which determines the pane order from bottom to top. For instance, if \code{zindex} is set to 500, the pane will be named \code{"tmap500"}.
#' @param group Name of the group to which this layer belongs. Not implemented yet.
#' @export
#' @rdname tm_basemap
#' @name tm_basemap
#' @example ./examples/tm_basemap.R
tm_basemap = function(server = NULL, alpha = NULL, zoom = NULL, zindex = 0, group = NA) {
	tm_element_list(tm_element(
		args = list(server = server, alpha = alpha, zoom = zoom, type = "basemap"),
		mapping.fun = "Tiles",
		zindex = zindex,
		group = group,
		subclass = c("tm_basemap", "tm_aux_layer")))
}

#' @export
#' @rdname tm_basemap
#' @name tm_tiles
tm_tiles = function(server = NULL, alpha = NULL, zoom = NULL, zindex = NA, group = NA) {
	tm_element_list(tm_element(
		args = list(server = server, alpha = alpha, zoom = zoom, type = "overlay"),
		mapping.fun = "Tiles",
		zindex = zindex,
		group = group,
		subclass = c("tm_tiles", "tm_aux_layer")))
}




#' @import leaflet
#' @export
leaflet::providers

tm_grid = function() {
	tm_element_list(tm_element(
		args = list(),
		mapping.fun = "Grid",
		subclass = c("tm_grid", "tm_aux_layer")))
}
