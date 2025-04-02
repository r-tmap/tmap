#' Legend charts
#'
#' Legend charts are small charts that are added to the map, usually in addition to legends.
#'
#' Note that these charts are different from charts drawn inside the map. Those are called glyphs (to be implemented).
#'
#' @param breaks The breaks of the bins (for histograms)
#' @param plot.axis.x,plot.axis.y Should the x axis and y axis be plot?
#' @param extra.ggplot2 Extra ggplot2 code
#' @inheritParams tm_title
#' @example examples/tm_chart.R
#' @seealso \href{https://r-tmap.github.io/tmap/articles/basics_charts}{Vignette about charts}
#' @name tm_chart
#' @export
tm_chart_histogram = function(breaks,
							  plot.axis.x,
							  plot.axis.y,
							  extra.ggplot2,
							  position,
							  group_id,
							  width,
							  height,
							  stack,
							  z,
							  ...) {

	args = lapply(as.list(rlang::call_match()[-1]), eval, envir = parent.frame())
	args = warning_group_args(args)

	args$z = args$z %||% NA_integer_
	args$group_id = args$group_id %||% NA_character_

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
						group_id,
						width,
						height,
						stack,
						z,
						...) {
	args = lapply(as.list(rlang::call_match()[-1]), eval, envir = parent.frame())
	args = warning_group_args(args)

	args$z = args$z %||% NA_integer_
	args$group_id = args$group_id %||% NA_character_

	args$show = TRUE
	args$type = "bar"
	args$summary = "binned"
	structure(args, class = c("tm_chart_bar", "tm_chart", "tm_component", "list"))
}


#' @rdname tm_chart
#' @export
tm_chart_donut = function(position,
						  group_id,
						  width,
						  height,
						  stack,
						  z,
						  ...) {
	args = lapply(as.list(rlang::call_match()[-1]), eval, envir = parent.frame())
	args = warning_group_args(args)


	args$z = args$z %||% NA_integer_
	args$group_id = args$group_id %||% NA_character_

	args$show = TRUE
	args$type = "donut"
	args$summary = "binned"
	structure(args, class = c("tm_chart_donut", "tm_chart", "tm_component", "list"))
}

#' @rdname tm_chart
#' @export
tm_chart_violin = function(position,
						   group_id,
						   width,
						   height,
						   stack,
						   z,
						   ...) {
	args = lapply(as.list(rlang::call_match()[-1]), eval, envir = parent.frame())
	args = warning_group_args(args)

	args$z = args$z %||% NA_integer_
	args$group_id = args$group_id %||% NA_character_

	args$show = TRUE
	args$type = "violin"
	args$summary = "raw_nna"
	structure(args, class = c("tm_chart_violin", "tm_chart", "tm_component", "list"))
}

#' @rdname tm_chart
#' @export
tm_chart_box = function(position,
						group_id,
					    width,
					    height,
					    stack,
					    z,
						...) {
	args = lapply(as.list(rlang::call_match()[-1]), eval, envir = parent.frame())
	args = warning_group_args(args)

	args$z = args$z %||% NA_integer_
	args$group_id = args$group_id %||% NA_character_

	args$show = TRUE
	args$type = "box"
	args$summary = "raw_nna"
	structure(args, class = c("tm_chart_box", "tm_chart", "tm_component", "list"))
}

#' @rdname tm_chart
#' @export
tm_chart_none = function() {
	structure(list(show = FALSE, group_id = NA_character_, z = NA_integer_, summary = "none"), class = c("tm_chart_none", "tm_chart", "tm_component", "list"))
}



#' @rdname tm_chart
#' @export
tm_chart_heatmap = function(position,
							group_id,
							width,
							height,
							stack,
							z,
							...){
	args = lapply(as.list(rlang::call_match()[-1]), eval, envir = parent.frame())
	args = warning_group_args(args)

	args$z = args$z %||% NA_integer_
	args$group_id = args$group_id %||% NA_character_

	args$show = TRUE
	args$type = "heatmap"
	args$summary = "binned2D"
	structure(args, class = c("tm_chart_heatmap", "tm_chart", "tm_component", "list"))
}
