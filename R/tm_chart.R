#' Legend charts
#'
#' Legend charts are small charts that are added to the map, usually in addition to legends.
#'
#' Note that these charts are different from charts drawn inside the map. Those are called glyphs (to be implemented).
#'
#' @param breaks The breaks of the bins (for histograms)
#' @param plot.axis.x,plot.axis.y Should the x axis and y axis be plot?
#' @param extra.ggplot2 Extra ggplot2 code
#' @param position Position of the chart. An object created with `tm_pos_in()` or `tm_pos_out()`. Or, as a shortcut, a vector of two values, specifying the x and y coordinates. The first is `"left"`, `"center"` or `"right"` (or upper case, meaning tighter to the map frame), the second `"top"`, `"center"` or `"bottom"`. Numeric values are also supported, where 0, 0 means left bottom and 1, 1 right top. See also \href{https://r-tmap.github.io/tmap/articles/adv_positions}{vignette about positioning}.
#' @param width in number of text lines (height of it)
#' @param height in number of text lines
#' @param stack stack with other map components, either `"vertical"` or `"horizontal"`.
#' @param z stacking order
#' @param group.frame group.frame
#' @param resize_as_group resize_as_group
#' @example examples/tm_chart.R
#' @seealso \href{https://r-tmap.github.io/tmap/articles/basics_charts}{Vignette about charts}
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
							  resize_as_group) {

	args = lapply(as.list(rlang::call_match()[-1]), eval, envir = parent.frame())

	if (!("z" %in% (names(args)))) args$z = NA_integer_
	args$show = TRUE
	args$type = "histogram"
	args$summary = "binned"
	structure(args, class = c("tm_chart_histogram", "tm_chart", "tm_component", "list"))
}

#' @rdname tm_chart
#' @export
tm_chart_bar = function(plot.axis.x,
						plot.axis.y,
						extra.ggplot2,
						position,
						width,
						height,
						stack,
						z,
						group.frame,
						resize_as_group) {
	args = lapply(as.list(rlang::call_match()[-1]), eval, envir = parent.frame())

	if (!("z" %in% (names(args)))) args$z = NA_integer_
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
						  resize_as_group) {
	args = lapply(as.list(rlang::call_match()[-1]), eval, envir = parent.frame())

	if (!("z" %in% (names(args)))) args$z = NA_integer_
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
						   resize_as_group) {
	args = lapply(as.list(rlang::call_match()[-1]), eval, envir = parent.frame())

	if (!("z" %in% (names(args)))) args$z = NA_integer_
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
					    resize_as_group) {
	args = lapply(as.list(rlang::call_match()[-1]), eval, envir = parent.frame())

	if (!("z" %in% (names(args)))) args$z = NA_integer_
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
							resize_as_group) {
	args = lapply(as.list(rlang::call_match()[-1]), eval, envir = parent.frame())

	if (!("z" %in% (names(args)))) args$z = NA_integer_
	args$show = TRUE
	args$type = "heatmap"
	args$summary = "binned2D"
	structure(args, class = c("tm_chart_heatmap", "tm_chart", "tm_component", "list"))
}
