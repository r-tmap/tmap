#' Shiny integration for tmap
#'
#' @description
#' * `tmapOutput()` creates a UI element
#' * `renderTmap()` renders a tmap map
#' * `tmapProxy()` updates a tmap map in `view` mode
#'
#' Adding layers is as usual via the map layer functions like [tm_polygons()].
#' Removing layers can be done, removing with `tm_remove_layer()`.
#'
#' @details
#' Two features from tmap are not (yet) supported in Shiny: small multiples (facets) and colored backgrounds (argument `bg.color` of [tm_layout()]).
#' Workarounds for small multiples: create multiple independent maps or specify `as.layers = TRUE` in [tm_facets()].
#'
#' @param expr A tmap object. A tmap object is created with [qtm()] or by stacking [`tmap-element`]s.
#' @param mode tmap mode, see [tmap_mode()] If not defined, the current mode is used
#' @param env The environment in which to evaluate expr
#' @param quoted Is `expr` a quoted expression (with `quote()`)? This is useful if you want to save an expression in a variable
#' @param outputId Output variable to read from
#' @param width,height the width and height of the map
#' @param mapId single-element character vector indicating the output ID of the map to modify (if invoked from a Shiny module, the namespace will be added automatically)
#' @param session the Shiny session object to which the map belongs; usually the default value will suffice
#' @param x the tmap object that specifies the added and removed layers.
#' @param zindex The stacking number of the layer to be removed. It is recommended to specify the `zindex` for this layer when creating the map (inside `renderTmap()`).
#' @param execOnResize If `TRUE` (default), when the plot is resized, the map is regenerated. When set to `FALSE` the map is rescaled: the aspect ratio is kept, but the layout will be less desirable.
#' @param ... passed on to the mode-specific methods
#' @importFrom htmlwidgets shinyWidgetOutput
#' @example ./examples/tmapOutput.R
#' @seealso `r .doc_see_also_shiny()`
#' @export
renderTmap <- function(expr, env = parent.frame(), quoted = FALSE, execOnResize = TRUE, mode = NA) {
	if (!quoted) expr <- substitute(expr)            # capture once, in this frame
	renderTmapGS(tmap_graphics_class(mode),
				 expr = expr, env = env, execOnResize = execOnResize)
}

#' @rdname renderTmap
#' @export
tmapOutput <- function(outputId, width = "100%", height = 400, mode = NA) {
	tmapOutputGS(tmap_graphics_class(mode),
				 outputId = outputId, width = width, height = height)
}



#' @rdname renderTmap
#' @export
tmapProxy <- function(mapId, session = shiny::getDefaultReactiveDomain(), x, mode = NA) {
	tmapProxyGS(tmap_graphics_class(mode),
				mapId = mapId, session = session, tmobj = x)   # `x` (tmap obj) -> tmobj
}


#' @rdname renderTmap
#' @export
tm_remove_layer <- function(zindex) {
	tm_element_list(tm_element(zindex = zindex,
							   subclass = c("tm_proxy", "tm_remove_layer")))
}

