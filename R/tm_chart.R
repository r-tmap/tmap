#' Legend charts
#' 
#' Legend charts are small charts that are added to the map, usually in addition to legends.
#' 
#' Note that these charts are different from charts drawn inside the map. Those are called glyphs (to be implemented). 
#' 
#' @param breaks The breaks of the bins (for histograms)
#' @param plot.axis.x,plot.axis.y Should the x axis and y axis be plot?
#' @param extra.ggplot2 Extra ggplot2 code
#' @param position Position of the chart. See [tm_pos()] for details
#' @param width in number of text lines (height of it)
#' @param height in number of text lines
#' @param stack stack with other map components?
#' @param z stacking order
#' @param group.frame group.frame
#' @param resize.as.group resize.as.group
#' @example examples/tm_chart.R
#' @name tm_chart
#' @export
tm_chart_histogram = function(breaks,
							  plot.axis.x,
							  plot.axis.y,
							  extra.ggplot2,
							  position,
							  width,
							  height,
							  stack,
							  z,
							  group.frame,
							  resize.as.group) {
	args = lapply(as.list(match.call()[-1]), eval, envir = parent.frame())
	
	if (!("z" %in% (names(args)))) args$z = as.integer(NA)
	args$show = TRUE
	args$type = "histogram"
	args$summary = "binned"
	structure(args, class = c("tm_chart_histogram", "tm_chart", "tm_component", "list"))
}

#' @rdname tm_chart
#' @export
tm_chart_bar = function( plot.axis.x,
							  plot.axis.y,
							  extra.ggplot2,
							  position,
							  width,
							  height,
							  stack,
							  z,
							  group.frame,
							  resize.as.group) {
	args = lapply(as.list(match.call()[-1]), eval, envir = parent.frame())
	
	if (!("z" %in% (names(args)))) args$z = as.integer(NA)
	args$show = TRUE
	args$type = "bar"
	args$summary = "binned"
	structure(args, class = c("tm_chart_bar", "tm_chart", "tm_component", "list"))
}


#' @rdname tm_chart
#' @export
tm_chart_donut = function(position,
							  width,
							  height,
							  stack,
							  z,
							  group.frame,
							  resize.as.group) {
	args = lapply(as.list(match.call()[-1]), eval, envir = parent.frame())
	
	if (!("z" %in% (names(args)))) args$z = as.integer(NA)
	args$show = TRUE
	args$type = "donut"
	args$summary = "binned"
	structure(args, class = c("tm_chart_donut", "tm_chart", "tm_component", "list"))
}

#' @rdname tm_chart
#' @export
tm_chart_violin = function(position,
						   width,
						   height,
						   stack,
						   z,
						   group.frame,
						   resize.as.group) {
	args = lapply(as.list(match.call()[-1]), eval, envir = parent.frame())
	
	if (!("z" %in% (names(args)))) args$z = as.integer(NA)
	args$show = TRUE
	args$type = "violin"
	args$summary = "raw_nna"
	structure(args, class = c("tm_chart_violin", "tm_chart", "tm_component", "list"))
}

#' @rdname tm_chart
#' @export
tm_chart_box = function(position,
						   width,
						   height,
						   stack,
						   z,
						   group.frame,
						   resize.as.group) {
	args = lapply(as.list(match.call()[-1]), eval, envir = parent.frame())
	
	if (!("z" %in% (names(args)))) args$z = as.integer(NA)
	args$show = TRUE
	args$type = "box"
	args$summary = "raw_nna"
	structure(args, class = c("tm_chart_box", "tm_chart", "tm_component", "list"))
}

#' @rdname tm_chart
#' @export
tm_chart_none = function() {
	structure(list(show = FALSE, summary = "none"), class = c("tm_chart_none", "tm_chart", "tm_component", "list"))	
}



#' @rdname tm_chart
#' @export
tm_chart_heatmap = function(position,
						width,
						height,
						stack,
						z,
						group.frame,
						resize.as.group) {
	args = lapply(as.list(match.call()[-1]), eval, envir = parent.frame())
	
	if (!("z" %in% (names(args)))) args$z = as.integer(NA)
	args$show = TRUE
	args$type = "heatmap"
	args$summary = "binned2D"
	structure(args, class = c("tm_chart_heatmap", "tm_chart", "tm_component", "list"))
}
