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
	structure(args, class = c("tm_chart_histogram", "tm_chart", "tm_component", "list"))
}

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
	structure(args, class = c("tm_chart_bar", "tm_chart", "tm_component", "list"))
}


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
	structure(args, class = c("tm_chart_donut", "tm_chart", "tm_component", "list"))
}


tm_chart_none = function() {
	structure(list(show = FALSE), class = c("tm_chart_histogram", "tm_chart", "tm_component", "list"))	
}
