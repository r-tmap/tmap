#' Wrapper functions for using tmap in shiny
#' 
#' Use \code{tmapOutput} to create a UI element, and \code{renderTmap} to render the tmap widget
#' 
#' Two features from tmap are not (yet) supported in Shiny: small multiples (facets) and colored backgrounds (argument bg.color of \code{\link{tm_layout}}). Workarounds for small multiples: create multiple independent maps or specify as.layers = TRUE in \code{\link{tm_facets}}.
#'
#' @param expr A tmap object. A tmap object is created with \code{\link{qtm}} or by stacking \code{\link{tmap-element}}s.
#' @param env The environment in which to evaluate expr
#' @param quoted Is expr a quoted expression (with quote())? This is useful if you want to save an expression in a variable
#' @param outputId Output variable to read from
#' @param width,height the width and height of the map
#' @name tmapOutput
#' @rdname tmapOutput
#' @export
tmapOutput <- function (outputId, width = "100%", height = 400) {
	htmlwidgets::shinyWidgetOutput(outputId, "leaflet", width,
								   height, "leaflet")
}

#' @name renderTmap
#' @rdname tmapOutput
#' @export
renderTmap <- function(expr, env = parent.frame(), quoted = FALSE) {
	expr <- tmap_leaflet(expr, in.shiny = TRUE)
	if (!quoted)
		expr = substitute(expr)
	htmlwidgets::shinyRenderWidget(expr, leafletOutput, env,
								   quoted = TRUE)
}

tm_proxy <- function(mapId) {
	# leaflet::leafletProxy(mapId)
	print("proxy")
	structure(list(tm_proxy = leaflet::leafletProxy(mapId)), class = "tmap")
}

tm_clear_polygons <- function() {
	print("clear_poly")
	structure(list(tm_clear = list(type = "polygons")), class = "tmap")
}
