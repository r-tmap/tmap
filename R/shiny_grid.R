#' @exportS3Method
renderTmapGS.Grid <- function(x, expr, env, execOnResize, ...) {
	expr <- bquote(getFromNamespace("print.tmap", "tmap")(.(expr)))
	shiny::renderPlot(expr, env = env, quoted = TRUE, execOnResize = execOnResize)
}

#' @exportS3Method
tmapOutputGS.Grid <- function(x, outputId, width, height, ...) {
	shiny::plotOutput(outputId = outputId, width = width, height = height)
}

#' @exportS3Method
tmapProxyGS.Grid <- function(x, mapId, session, tmobj, ...) {
	message("tmapProxy not working for plot mode (yet)")
	print.tmap(tmobj, show = TRUE)
}
