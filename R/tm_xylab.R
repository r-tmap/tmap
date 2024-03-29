#' Map: x and y labels
#' 
#' The x and y labels for maps
#' 
#' @param text text of the title
#' @param size font size of the title
#' @param color color
#' @param rotation rotation in degrees
#' @param space space between label and map in number of line heights
#' @param fontface font face 
#' @param fontfamily font family
#' @param side side: `"top"` or `"bottom"` for `tm_xlab` and `"left"` or `"right"` for `tm_ylab`
#' @export
#' @export
#' @rdname tm_xlab
#' @name tm_xlab
tm_xlab = function(text, size, color, rotation, space, fontface, fontfamily, side) {
	args = lapply(as.list(match.call()[-1]), eval, envir = parent.frame())
	args$show = TRUE
	names(args) = paste("xlab", names(args), sep = ".")
	do.call(tm_options, args)
}

#' @export
#' @rdname tm_xlab
#' @name tm_ylab
tm_ylab = function(text, size, color, rotation, space, fontface, fontfamily, side) {
	args = lapply(as.list(match.call()[-1]), eval, envir = parent.frame())
	args$show = TRUE
	names(args) = paste("ylab", names(args), sep = ".")
	do.call(tm_options, args)
}
