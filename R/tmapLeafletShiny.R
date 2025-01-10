# not working yet, see #1024
renderTmapLeaflet = function(expr, env, quoted, execOnResize) {
	if (!quoted)
		expr = substitute(expr)
	expr = substitute(getFromNamespace("print.tmap", "tmap")(expr, in.shiny = TRUE))
	htmlwidgets::shinyRenderWidget(expr, leafletOutput, env,
								   quoted = TRUE)
}

tmapOutputLeaflet = function(outputId, width, height) {
	leaflet::leafletOutput(outputId, width = width, height = height)
}


tmapProxyLeaflet = function(mapId, session, x) {
	print.tmap(x, lf = leaflet::leafletProxy(mapId, session), show = FALSE, in.shiny = TRUE, proxy = TRUE)
}
