pchs = stats::setNames(c(seq(0L, 25L, 1L), seq(100L, 109L, 1L)),
					   c(c('open-rect', 'open-circle', 'open-triangle', 'simple-plus', 
					     'simple-cross', 'open-diamond', 'open-down-triangle', 'cross-rect', 
					     'simple-star', 'plus-diamond', 'plus-circle', 'hexagram', 'plus-rect', 
					     'cross-circle', 'triangle-rect', 'solid-rect', 'solid-circle-md', 
					     'solid-triangle', 'solid-diamond', 'solid-circle-bg', 'solid-circle-sm', 'circle', 
					     'rect', 'diamond', 'triangle', 'down-triangle'
					   ),
					     c('rect', 'circle', 'triangle', 'plus', 'cross', 'diamond', 'star', 'stadium', 'line', 'polygon')
					   ))


get_pch_names = function(x) {
	if (is.numeric(x)) {
		if (!(all(x %in% pchs))) stop("Unknown symbol values", call. = FALSE)
		names(pchs)[match(x, pchs)]
	} else {
		if (!all(x %in% names(pchs))) stop("Unknown symbol values", call. = FALSE)
		x
	}
}

get_pch_number = function(x) {
	if (is.numeric(x)) {
		if (!(all(x %in% pchs))) stop("Unknown symbol values", call. = FALSE)
		x
	} else {
		if (!all(x %in% names(pchs))) stop("Unknown symbol values", call. = FALSE)
		unname(pchs[match(x, names(pchs))])
	}
}
