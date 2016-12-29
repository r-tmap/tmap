#' ggplot2 theme for proportional symbols
#' 
#' ggplot2 theme for proportional symbols. By default, this theme only shows the plotting area, so without titles, axes, and legend
#' 
#' @param base_size base size
#' @param base_family base family
#' @param plot.axes should the axes be shown?
#' @param plot.legend should the legend(s) be shown?
#' @export
theme_ps <- function(base_size = 12, base_family = "", plot.axes=FALSE, plot.legend=FALSE) {
	if (!requireNamespace("ggplot2", quietly = TRUE)) {
		stop("ggplot2 package needed for this function to work. Please install it.",
			 call. = FALSE)
	} else {
		replace <- function(e1, e2) {
			e1[names(e2)] <- e2
			e1	
		}
		
		tps <- replace(ggplot2::theme_minimal(base_size = base_size, base_family = base_family),
			ggplot2::theme(panel.background = ggplot2::element_blank(), 
				  panel.border = ggplot2::element_blank(), 
				  panel.grid = ggplot2::element_blank(),
				  strip.background = ggplot2::element_blank(), 
				  plot.background = ggplot2::element_blank(), 
				  plot.title = ggplot2::element_blank(),
				  strip.text = ggplot2::element_blank(),
				  panel.spacing = unit(0,"null"),
				  plot.margin = rep(unit(0,"null"),4)))
		
		if (!plot.legend) {
			tps <- replace(tps, ggplot2::theme(legend.position = "none"))
		}
		
		if (plot.axes) {
			tps
		} else {
			replace(tps, 
					ggplot2::theme(axis.text = ggplot2::element_blank(),
					  axis.title = ggplot2::element_blank(),
					  axis.ticks = ggplot2::element_line(), 
					  axis.ticks.length = unit(0, "lines")))
		}
	}
}
