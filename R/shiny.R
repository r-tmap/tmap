#' Wrapper functions for using tmap in shiny
#' 
#' Use \code{tmapOutput} to create a UI element, and \code{renderTmap} to render the tmap map. To update the map (more specifically, to add and remove layers) use \code{tmapProxy}. Adding layers is as usual, removing layers can be done with the function \code{tm_remove_layer}.
#' 
#' Two features from tmap are not (yet) supported in Shiny: small multiples (facets) and colored backgrounds (argument bg.color of \code{\link{tm_layout}}). Workarounds for small multiples: create multiple independent maps or specify as.layers = TRUE in \code{\link{tm_facets}}.
#' 
#' @param expr A tmap object. A tmap object is created with \code{\link{qtm}} or by stacking \code{\link{tmap-element}}s.
#' @param env The environment in which to evaluate expr
#' @param quoted Is expr a quoted expression (with quote())? This is useful if you want to save an expression in a variable
#' @param outputId Output variable to read from
#' @param width,height the width and height of the map
#' @param mapId single-element character vector indicating the output ID of the map to modify (if invoked from a Shiny module, the namespace will be added automatically)
#' @param session the Shiny session object to which the map belongs; usually the default value will suffice
#' @param x the tmap object that specifies the added and removed layers.
#' @param zindex the z index of the pane in which the layer is contained that is going to be removed. It is recommended to specify the zindex for this layer when creating the map (inside \code{renderTmap}).
#' @importFrom htmlwidgets shinyWidgetOutput
#' @rdname renderTmap
#' @example ./examples/tmapOutput.R 
#' @export
renderTmap <- function(expr, env = parent.frame(), quoted = FALSE) {
	if (!quoted)
		expr = substitute(expr)
	expr = substitute(getFromNamespace("tmap_leaflet", "tmap")(expr, in.shiny = TRUE))
	htmlwidgets::shinyRenderWidget(expr, leafletOutput, env,
								   quoted = TRUE)
}


#' @name tmapOutput
#' @rdname renderTmap
#' @export
tmapOutput <- function(outputId, width = "100%", height = 400) {
	leafletOutput(outputId, width = width, height = height)
}

#' @name tmapProxy
#' @rdname renderTmap
#' @export
tmapProxy <- function(mapId, session = shiny::getDefaultReactiveDomain(), x) {
	print.tmap(x, mode="view", show=FALSE, interactive_titles = TRUE, in.shiny = TRUE, lf = leaflet::leafletProxy(mapId, session))
}


#' @name tm_remove_layer
#' @rdname renderTmap
#' @export
tm_remove_layer <- function(zindex) {
	structure(list(tm_remove_layer = list(zindex = zindex)), class = "tmap")
}

