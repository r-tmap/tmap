#' ggplot2 theme for proportional symbols
#' 
#' ggplot2 theme for proportional symbols. By default, this theme only shows the plotting area, so without titles, axes, and legend
#' 
#' @param base_size
#' @param base_family
#' @param plot.axes
#' @param plot.legend
#' @export
theme_ps <- function(base_size = 12, base_family = "", plot.axes=FALSE, plot.legend=FALSE) {
	if (!requireNamespace("ggplot2", quietly = TRUE)) {
		stop("ggplot2 package needed for this function to work. Please install it.",
			 call. = FALSE)
	} else {
		tps <- theme_minimal(base_size = base_size, base_family = base_family) %+replace% 
			theme(panel.background = element_blank(), 
				  panel.border = element_blank(), 
				  panel.grid = element_blank(),
				  strip.background = element_blank(), 
				  plot.background = element_blank(), 
				  plot.title = element_blank(),
				  strip.text = element_blank(),
				  panel.margin = unit(0,"null"),
				  plot.margin = rep(unit(0,"null"),4))
		
		if (!plot.legend) {
			tps <- tps %+replace% theme(legend.position = "none")
		}
		
		if (plot.axes) {
			tps
		} else {
			tps	%+replace%
				theme(axis.text = element_blank(),
					  axis.title = element_blank(),
					  axis.ticks = element_line(), 
					  axis.ticks.length = unit(0, "lines"))
		}
	}
}
