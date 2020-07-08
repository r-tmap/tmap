#' Add manual legend
#' 
#' Creates a \code{\link{tmap-element}} that adds a manual legend.
#' 
#' @param type type of legend. One of \code{"fill"}, \code{"symbol"}, \code{"text"}, \code{"line"}, or \code{"title"}. The last option only displays a title.
#' @param labels legend labels
#' @param col legend colors
#' @param size legend symbol sizes (if \code{type=="symbol"}). See example how to replicate the sizes of symbols created with \code{\link{tm_symbols}}. If not specified, the symbols will have the same size as when calling \code{\link{tm_symbols}} without specifying the \code{size} argument.
#' @param shape legend symbol shapes (if \code{type=="symbol"})
#' @param lwd legend line widths (if \code{type=="line"})
#' @param lty legend line types (if \code{type=="line"})
#' @param text legend texts (if \code{type=="text"})
#' @param alpha legend fill transparency
#' @param border.col legend border col (if \code{type} is \code{"fill"} or \code{"symbol"})
#' @param border.lwd legend border width (if \code{type} is \code{"fill"} or \code{"symbol"})
#' @param border.alpha legend border alpha (if \code{type} is \code{"fill"} or \code{"symbol"})
#' @param title legend title
#' @param is.portrait is legend portrait (\code{TRUE}) or landscape (\code{FALSE})?
#' @param legend.format options to format the legend, see \code{\link{tm_symbols}} (the description of the argument \code{legend.format}) for details. Note that many of these arguments are not applicable for \code{tm_add_legend} since \code{labels} should be a character vector. However, some options could still be handy, e.g. \code{list(text.align = "right")}.
#' @param reverse are the legend items reversed (by default \code{FALSE})?
#' @param z legend stack position
#' @param zindex zindex of the pane in view mode to which the legend belongs (if any). 
#' @param group name of the group to which this layer belongs in view mode. Each group can be selected or deselected in the layer control item. By default \code{NULL}, which means that the legend will not be shown in the layer control item.
#' @export
#' @example ./examples/tm_add_legend.R
#' @seealso \code{\link{tm_symbols}} for another example
tm_add_legend <- function(type = c("fill", "symbol", "text", "line", "title"), 
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
						  z=NA,
						  zindex = NA,
						  group=NULL) {
	type <- match.arg(type)
	g <- list(tm_add_legend=c(as.list(environment()), list(are.dots=FALSE, call=names(match.call(expand.dots = TRUE)[-1]))))
	class(g) <- "tmap"
	g
}
