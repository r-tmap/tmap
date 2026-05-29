#' @exportS3Method
renderTmapGS.Leaflet <- function(x, expr, env, ...) {
	expr <- bquote(getFromNamespace("print.tmap", "tmap")(.(expr), in.shiny = TRUE))
	htmlwidgets::shinyRenderWidget(expr, leaflet::leafletOutput, env, quoted = TRUE)
}

#' @exportS3Method
tmapOutputGS.Leaflet <- function(x, outputId, width, height, ...) {
	leaflet::leafletOutput(outputId = outputId, width = width, height = height)
}

#' @exportS3Method
tmapProxyGS.Leaflet <- function(x, mapId, session, tmobj, ...) {
	print.tmap(tmobj,
			   lf = leaflet::leafletProxy(mapId, session),
			   show = FALSE, in.shiny = TRUE, proxy = TRUE)
}
