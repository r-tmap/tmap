#' Add manual legend
#' 
#' Creates a \code{\link{tmap-element}} that adds a manual legend.
#' 
#' @param type type of legend. One of \code{"fill"}, \code{"symbol"}, \code{"text"}, or \code{"line"}
#' @param labels legend labels
#' @param col legend colors
#' @param size legend symbol sizes (if \code{type=="symbol"})
#' @param shape legend symbol shapes (if \code{type=="symbol"})
#' @param lwd legend line widths (if \code{type=="line"})
#' @param lty legend line types (if \code{type=="line"})
#' @param text legend texts (if \code{type=="text"})
#' @param alpha legend fill transparancy
#' @param border.col legend border col (if \code{type} is \code{"fill"} or \code{"symbol"})
#' @param border.lwd legend border width (if \code{type} is \code{"fill"} or \code{"symbol"})
#' @param border.alpha legend border alpha (if \code{type} is \code{"fill"} or \code{"symbol"})
#' @param title legend title
#' @param is.portrait is legend portrait (\code{TRUE}) or landscape (\code{FALSE})?
#' @param legend.format options to format the legend, see \code{\link{tm_layout}}
#' @param reverse are the legend items reversed (by default \code{FALSE})?
#' @param z legend stack position
#' @export
#' @seealso \code{\link{tm_symbols}} for an example
tm_add_legend <- function(type = c("fill", "symbol", "text", "line"), 
						  labels=NULL, 
						  col=NULL, 
						  size=NULL, 
						  shape=NULL,
						  lwd=NULL,
						  lty=NULL,
						  text=NULL, 
						  alpha=NA,
						  border.col="black",
						  border.lwd=1,
						  border.alpha=NA,
						  title="", 
						  is.portrait=TRUE, 
						  legend.format=list(),
						  reverse=FALSE,
						  z=NA) {
	type <- match.arg(type)
	g <- list(tm_add_legend=c(as.list(environment()), list(are.dots=FALSE, call=names(match.call(expand.dots = TRUE)[-1]))))
	class(g) <- "tmap"
	g
}
