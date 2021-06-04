tm_legend = function(title  = NA,
					 show = TRUE,
					 format = list(),
					 is.portrait = TRUE,
					 reverse = FALSE,
					 z = NA) {
	structure(c(list(FUN = "tmapLegend"), as.list(environment())), class = "tm_legend")
}

tm_legend_portrait = function(...) {
	tm_legend(...)
}

tm_legend_landscape = function(is.portrait = FALSE, ...) {
	args = c(list(...), list(is.portrait = is.portrait))
	do.call(tm_legend, args)
}

