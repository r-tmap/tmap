tm_chart_histogram = function(position,
						width,
						height,
						stack,
						z,
						group.frame,
						resize.as.group) {
	args = lapply(as.list(match.call()[-1]), eval, envir = parent.frame())
	
	if (!("z" %in% (names(args)))) args$z = as.integer(NA)
	args$show = TRUE
	structure(args, class = c("tm_chart_histogram", "tm_chart", "tm_component", "list"))
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
	structure(args, class = c("tm_chart_donut", "tm_chart", "tm_component", "list"))
}


tm_chart_none = function() {
	structure(list(show = FALSE), class = c("tm_chart_histogram", "tm_chart", "tm_component", "list"))	
}
