#' Options for the interactive tmap viewer
#' 
#' Set the options for the interactive tmap viewer
#' 
#' @param alpha transparency parameter applied to whole map
#' @param popup.all.data should only the aesthetic variables be shown in the popup windows, or all variables?
#' @param na color for missing values. The default value of \code{NULL} means transparent. It overrides the \code{na} value of the \code{aes.color} in \code{\link{tm_layout}}.
#' @param basemaps vector of one or more names of baselayer maps. See \url{http://leaflet-extras.github.io/leaflet-providers/preview}.
#' @param bubble.size.fixed should bubble sizes be fixed while zooming?
#' @param dot.size.fixed should dot sizes be fixed while zooming?
#' 
#' @export
tm_view_options <- function(alpha=.7,
							popup.all.data=FALSE,
							na=NA,
							basemaps=c("OpenStreetMap", "Esri.WorldImagery", "Thunderforest.Landscape", "OpenTopoMap"),
							bubble.size.fixed=FALSE,
							dot.size.fixed=TRUE
							) {
	g <- list(tm_view=c(as.list(environment()), list(call=names(match.call(expand.dots = TRUE)[-1]))))
	class(g) <- "tm"
	g
}

