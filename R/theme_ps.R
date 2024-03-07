#' ggplot2 theme for proportional symbols
#' 
#' ggplot2 theme for proportional symbols. By default, this theme only shows the
#' plotting area, so without titles, axes, and legend.
#' 
#' @param base_size base size
#' @param base_family base family
#' @param plot.axes should the axes be shown?
#' @param plot.legend should the legend(s) be shown?
#' @export
theme_ps <- function(base_size = 12, base_family = "", plot.axes = FALSE, plot.legend = FALSE) {
  rlang::check_installed("ggplot2", reason = "to create plot themes.")
  replace <- function(e1, e2) {
    e1[names(e2)] <- e2
    e1
  }
  tps <- replace(
    ggplot2::theme_minimal(base_size = base_size, base_family = base_family),
    ggplot2::theme(
      panel.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      plot.background = ggplot2::element_blank(),
      plot.title = ggplot2::element_blank(),
      strip.text = ggplot2::element_blank(),
      panel.spacing = unit(0, "null"),
      plot.margin = rep(unit(0, "null"), 4)
    )
  )
  if (!plot.legend) {
    tps <- replace(tps, ggplot2::theme(legend.position = "none"))
  }
  if (plot.axes) {
    tps
  } else {
    replace(
      tps,
      ggplot2::theme(
        axis.text = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_line(),
        axis.ticks.length = unit(0, "lines")
      )
    )
  }
}


theme_chart <- function(base_size = 12 * scale, base_family = "", plot.axes = FALSE, plot.axis.x = FALSE, plot.axis.y = FALSE, plot.grid.x = FALSE, plot.grid.y = TRUE, plot.legend = FALSE, scale = 1, text.color = "#000000", text.size = 1) {
	rlang::check_installed("ggplot2", reason = "to create plot themes.")
	replace <- function(e1, e2) {
		e1[names(e2)] <- e2
		e1
	}
	
	tps <- replace(
		ggplot2::theme_minimal(base_size = base_size, base_family = base_family),
		ggplot2::theme(
			text = ggplot2::element_text(color = text.color, size = base_size * text.size),
			axis.text = ggplot2::element_text(color = text.color, size = base_size * text.size),
			panel.background = ggplot2::element_blank(),
			panel.border = ggplot2::element_blank(),
			panel.grid = ggplot2::element_blank(),
			strip.background = ggplot2::element_blank(),
			plot.background = ggplot2::element_blank(),
			plot.title = ggplot2::element_blank(),
			strip.text = ggplot2::element_blank(),
			panel.spacing = unit(0, "null"),
			plot.margin = unit(rep(.25, 4), "lines")
		)
	)
	if (!plot.legend) {
		tps <- replace(tps, ggplot2::theme(legend.position = "none"))
	}
	
	if (plot.axes) {
		plot.axis.x = TRUE
		plot.axis.y = TRUE
	}
	
	
	if (plot.grid.x) {
		tps = replace(
			tps,
			ggplot2::theme(
				panel.grid.major.x = ggplot2::element_line(colour = "#888888", linewidth = lwd_to_mm(scale))	
			)
		)
	}
	if (plot.grid.y) {
		tps = replace(
			tps,
			ggplot2::theme(
				panel.grid.major.y = ggplot2::element_line(colour = "#888888", linewidth = lwd_to_mm(scale))	
			)
		)
	}
	
	if (plot.axis.x) {
		tps = replace(
			tps,
			ggplot2::theme(
				#axis.text.x = ggplot2::element_blank(),
				axis.title.x = ggplot2::element_blank(),
				axis.ticks.x = ggplot2::element_line(),
				axis.ticks.length.x = unit(0, "lines")
			)
		)
	} else {
		tps = replace(
			tps,
			ggplot2::theme(
				axis.text.x = ggplot2::element_blank(),
				axis.title.x = ggplot2::element_blank(),
				axis.ticks.x = ggplot2::element_line(),
				axis.ticks.length.x = unit(0, "lines")
			)
		)
	}
	
	if (plot.axis.y) {
		tps = replace(
			tps,
			ggplot2::theme(
				#axis.text.y = ggplot2::element_blank(),
				axis.title.y = ggplot2::element_blank(),
				axis.ticks.y = ggplot2::element_line(),
				axis.ticks.length.y = unit(0, "lines")
			)
		)
	} else {
		tps = replace(
			tps,
			ggplot2::theme(
				axis.text.y = ggplot2::element_blank(),
				axis.title.y = ggplot2::element_blank(),
				axis.ticks.y = ggplot2::element_line(),
				axis.ticks.length.y = unit(0, "lines")
			)
		)
	}
		
	tps
}
