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


#' @name tm_graticules
#' @rdname tm_grid
#' @export
tm_graticules <- function(x=NA,
						  y=NA,
						  n.x=NA,
						  n.y=NA,
						  projection = 4326,
						  labels.format = list(suffix = intToUtf8(176)),
						  labels.cardinal = TRUE,
						  ...) {
	do.call(tm_grid, c(list(x = x, y = y, n.x = n.x, n.y = n.y, projection = projection, labels.format = labels.format, labels.cardinal = labels.cardinal), list(...)))
}


#' Coordinate grid / graticule lines
#' 
#' Creates a \code{\link{tmap-element}} that draws coordinate grid lines. It serves as a layer that can be drawn anywhere between other layers. By default, \code{tm_grid} draws horizontal and vertical lines acording to the coordinate system of the (master) shape object. Latitude and longitude graticules are drawn with \code{tm_graticules}.
#' 
#' @param x x coordinates for vertical grid lines. If \code{NA}, it is specified with a pretty scale and \code{n.x}.
#' @param y y coordinates for horizontal grid lines. If \code{NA}, it is specified with a pretty scale and \code{n.y}.
#' @param n.x preferred number of grid lines for the x axis. For the labels, a \code{\link{pretty}} sequence is used, so the number of actual labels may be different than \code{n.x}.
#' @param n.y preferred number of grid lines for the y axis. For the labels, a \code{\link{pretty}} sequence is used, so the number of actual labels may be different than \code{n.y}.
#' @param projection projection character. If specified, the grid lines are projected accordingly. Many world maps are projected, but still have latitude longitude (epsg 4326) grid lines.
#' @param col color of the grid lines.
#' @param lwd line width of the grid lines
#' @param alpha alpha transparency of the grid lines. Number between 0 and 1. By default, the alpha transparency of \code{col} is taken. 
#' @param labels.show show tick labels. Either one value for both \code{x} and \code{y} axis, or a vector two: the first for \code{x} and latter for \code{y}.
#' @param labels.size font size of the tick labels
#' @param labels.col font color of the tick labels
#' @param labels.rot Rotation angles of the labels. Vector of two values: the first is the rotation angle (in degrees) of the tick labels on the x axis and the second is the rotation angle of the tick labels on the y axis. Only \code{0}, \code{90}, \code{180}, and \code{270} are valid values.
#' @param labels.format list of formatting options for the grid labels. Parameters are:
#' \describe{
#' \item{fun}{Function to specify the labels. It should take a numeric vector, and should return a character vector of the same size. By default it is not specified. If specified, the list items \code{scientific}, \code{format}, and \code{digits} (see below) are not used.}
#' \item{scientific}{Should the labels be formatted scientifically? If so, square brackets are used, and the \code{format} of the numbers is \code{"g"}. Otherwise, \code{format="f"}, and \code{text.separator}, \code{text.less.than}, and \code{text.or.more} are used. Also, the numbers are automatically  rounded to millions or billions if applicable.}
#' \item{format}{By default, \code{"f"}, i.e. the standard notation \code{xxx.xxx}, is used. If \code{scientific=TRUE} then \code{"g"}, which means that numbers are formatted scientifically, i.e. \code{n.dddE+nn} if needed to save space.}
#' \item{digits}{Number of digits after the decimal point if \code{format="f"}, and the number of significant digits otherwise.}
#' \item{...}{Other arguments passed on to \code{\link[base:formatC]{formatC}}}
#' }
#' @param labels.cardinal add the four cardinal directions (N, E, S, W) to the labels, instead of using negative coordiantes for west and south (so it assumes that the coordinates are positive in the north-east direction).
#' @param labels.margin.x margin between tick labels of x axis and the frame. Note that when \code{labels.inside.frame == FALSE} and \code{ticks == TRUE}, the ticks will be adjusted accordingly.
#' @param labels.margin.y margin between tick labels of y axis and the frame. Note that when \code{labels.inside.frame == FALSE} and \code{ticks == TRUE}, the ticks will be adjusted accordingly.
#' @param labels.space.x space that is used for the labels and ticks for the x-axis when \code{labels.inside.frame == FALSE}. By default, it is determined automatically using the widths and heights of the tick labels. The unit of this parameter is text line height.
#' @param labels.space.y space that is used for the labels and ticks for the y-axis when \code{labels.inside.frame == FALSE}. By default, it is determined automatically using the widths and heights of the tick labels. The unit of this parameter is text line height.
#' @param labels.inside.frame Show labels inside the frame? By default \code{FALSE}
#' @param ticks If \code{labels.inside.frame = FALSE}, should ticks can be drawn between the labels and the frame? Either one value for both \code{x} and \code{y} axis, or a vector two: the first for \code{x} and latter for \code{y}.
#' @param lines If \code{labels.inside.frame = FALSE}, should grid lines can be drawn?
#' @param ndiscr number of points to discretize a parallel or meridian (only applicable for curved grid lines)
#' @param zindex zindex of the pane in view mode. By default, it is set to the layer number plus 400. By default, the tmap layers will therefore be placed in the custom panes \code{"tmap401"}, \code{"tmap402"}, etc., except for the base tile layers, which are placed in the standard \code{"tile"}. This parameter determines both the name of the pane and the z-index, which determines the pane order from bottom to top. For instance, if \code{zindex} is set to 500, the pane will be named \code{"tmap500"}.
#' @param group not used
#' @param ... arguments passed on to \code{tm_grid}
#' @export
#' @example ./examples/tm_grid.R
tm_grid = function(x=NA,
				   y=NA,
				   n.x=NA,
				   n.y=NA,
				   projection=NA,
				   col=NA,
				   lwd=1,
				   alpha=NA,
				   labels.show=TRUE,
				   labels.pos=c("left", "bottom"),
				   labels.size=.6,
				   labels.col=NA,
				   labels.rot = c(0, 0),
				   labels.format = list(big.mark = ","),
				   labels.cardinal = FALSE,
				   labels.margin.x=0,
				   labels.margin.y=0,
				   labels.space.x=NA,
				   labels.space.y=NA,
				   labels.inside.frame=FALSE,
				   ticks = labels.show & !labels.inside.frame,
				   lines = TRUE,
				   ndiscr = 100,
				   zindex = NA,
				   group = NA) {
	tm_element_list(tm_element(
		args = c(list(show = TRUE), as.list(environment())),
		mapping.fun = "Grid",
		zindex = zindex,
		group = group,
		subclass = c("tm_grid", "tm_aux_layer")))
}

tm_graticules
