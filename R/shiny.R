#' Wrapper functions for using **tmap** in **shiny**
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
#' @param zindex the z index of the pane in which the layer is contained that is going to be removed. It is recommended to specify the `zindex` for this layer when creating the map (inside `renderTmap()`).
#' @param execOnResize If `TRUE` (default), when the plot is resized, the map is regenerated. When set to `FALSE` the map is rescaled: the aspect ratio is kept, but the layout will be less desirable.
#' @importFrom htmlwidgets shinyWidgetOutput
#' @example ./examples/tmapOutput.R
#' @export
renderTmap <- function(expr, env = parent.frame(), quoted = FALSE, execOnResize = TRUE, mode = NA) {
	if (is.na(mode)) mode = getOption("tmap.mode")
	gs = tmap_graphics_name(mode)


	if (gs == "Grid") {
		expr = substitute(getFromNamespace("print.tmap", "tmap")(expr))
		shiny::renderPlot(expr, env = env, quoted = TRUE, execOnResize = execOnResize)
	} else if (gs == "Leaflet") {
		if (!quoted)
			expr = substitute(expr)
		expr = substitute(getFromNamespace("print.tmap", "tmap")(expr, in.shiny = TRUE))
		htmlwidgets::shinyRenderWidget(expr, leafletOutput, env,
									   quoted = TRUE)



		# fun = paste0("renderTmap", gs)
		# do.call(fun, list(expr = expr, env = env, quoted = quoted, execOnResize = execOnResize))
	} else {
		stop('renderTmap does not work yet for modes other than "plot" and "view"')
	}

}


#' @rdname renderTmap
#' @export
tmapOutput <- function(outputId, width = "100%", height = 400, mode = NA) {
	if (is.na(mode)) mode = getOption("tmap.mode")
	gs = tmap_graphics_name(mode)
	if (gs == "Grid") {
		shiny::plotOutput(outputId = outputId, width = width, height = height)
	} else {
		fun = paste0("tmapOutput", gs)
		do.call(fun, list(outputId = outputId, width = width, height = height))
	}
}


#' @rdname renderTmap
#' @export
tmapProxy <- function(mapId, session = shiny::getDefaultReactiveDomain(), x, mode = NA) {
	if (is.na(mode)) mode = getOption("tmap.mode")
	gs = tmap_graphics_name(mode)

	if (gs == "Grid") {
		message("tmapProxy not working for plot mode (yet)")
		print.tmap(x, show = TRUE)
	} else {
		#fun = paste0("tmapProxy", gs)
		#do.call(fun, list(mapId = mapId, session = session, x = x))
		print.tmap(x, lf = leaflet::leafletProxy(mapId, session), show = FALSE, in.shiny = TRUE, proxy = TRUE)
	}

}


#' @rdname renderTmap
#' @export
tm_remove_layer <- function(zindex) {
	tm_element_list(tm_element(zindex = zindex,
							   subclass = c("tm_proxy", "tm_remove_layer")))
}

