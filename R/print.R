#' Draw thematic map
#' 
#' Draw thematic map. If the tmap mode is set to \code{"plot"} (see \code{\link{tmap_mode}}), the map is plot in the current graphics device. If the mode is set to \code{"view"}, the map is shown interactively as an htmlwidget.
#' 
#' @param x tmap object. A tmap object is created with \code{\link{qtm}} or by stacking \code{\link{tmap-element}}s.
#' @param vp \code{\link[grid:viewport]{viewport}} to draw the plot in. This is particularly useful for insets.
#' @param return.asp Logical that determines whether the aspect ratio of the map is returned. In that case, \code{\link[grid:grid.newpage]{grid.newpage()}} will be called, but without plotting of the map. This is used by \code{\link{tmap_save}} to determine the aspect ratio of the map.
#' @param mode the mode of tmap: \code{"plot"} (static) or \code{"view"} (interactive). See \code{\link{tmap_mode}} for details.
#' @param show logical that determines whether to show to map. Obviously \code{TRUE} by default, but \code{show=FALSE} can be useful for just obtaining the returned objects.
#' @param knit should \code{\link[knitr:knit_print]{knit_print}} be enabled, or the normal \code{\link[base:print]{print}} function?
#' @param options options passed on to knitprint
#' @param ... not used
#' @return If \code{mode=="plot"}, then a list is returned with the processed shapes and the metadata. If \code{mode=="view"}, a \code{\link[leaflet:leaflet]{leaflet}} object is returned (see also \code{\link{tmap_leaflet}})
#' @import tmaptools
#' @import sf
#' @import stars
#' @importFrom units set_units as_units
#' @import RColorBrewer
#' @importFrom viridisLite viridis
#' @import grid
#' @import methods
#' @importFrom graphics par
#' @importFrom classInt classIntervals findCols
#' @importFrom grDevices col2rgb colorRampPalette dev.off dev.set dev.cur is.raster png rgb dev.size
#' @importFrom stats na.omit dnorm fft quantile rnorm runif 
#' @importFrom grDevices xy.coords colors
#' @importFrom utils capture.output data download.file head setTxtProgressBar tail txtProgressBar
#' @importFrom leafem addStarsImage addStarsRGB addMouseCoordinates
#' @import leaflet
#' @importFrom htmlwidgets appendContent onRender
#' @importFrom htmltools tags HTML htmlEscape
#' @import leafsync
#' @importFrom utils packageVersion
#' @importFrom rlang missing_arg expr
#' @export
#' @method print tmap
print.tmap <- function(x, vp=NULL, return.asp=FALSE, mode=getOption("tmap.mode"), show=TRUE, knit=FALSE, options=NULL, ...) {
	print_tmap(x=x, vp=vp, return.asp=return.asp, mode=mode, show=show, knit=knit, options=options, ...)
}

#' @rdname print.tmap
#' @rawNamespace
#' if(getRversion() >= "3.6.0") {
#'   S3method(knitr::knit_print, tmap)
#' } else {
#'   export(knit_print.tmap)
#' }
knit_print.tmap <- function(x, ..., options=NULL) {
	# @importFrom knitr knit_print
	print_tmap(x, knit=TRUE, options=options, ...)
}

#' Create a leaflet widget from a tmap object
#' 
#' Create a leaflet widget from a tmap object. An interactive map (see \code{\link{tmap_mode}}) is an automatically generated leaflet widget. With this function, this leaflet widget is obtained, which can then be changed or extended by using leaflet's own methods.
#'  
#' @param x tmap object. A tmap object is created with \code{\link{qtm}} or by stacking \code{\link{tmap-element}}s.
#' @param mode the mode of tmap, which is set to \code{"view"} in order to obtain the leaflet object. See \code{\link{tmap_mode}} for details.
#' @param show should the leaflet map be shown? \code{FALSE} by default
#' @param add.titles add titles to leaflet object
#' @param in.shiny is the leaflet output going to be used in shiny? If so, two features are not supported and therefore disabled: facets and colored backgrounds.
#' @param ... arguments passed on to \code{\link{print.tmap}}
#' @return \code{\link[leaflet:leaflet]{leaflet}} object
#' @example ./examples/tmap_leaflet.R
#' @seealso \code{\link{tmapOutput}} for tmap in Shiny, \code{\link{tmap_mode}}, \code{\link{tm_view}}, \code{\link{print.tmap}}
#' @export
tmap_leaflet <- function(x, mode="view", show = FALSE, add.titles = TRUE, in.shiny = FALSE, ...) {
	print.tmap(x, mode=mode, show=show, interactive_titles = add.titles, in.shiny = in.shiny, ...)
}
