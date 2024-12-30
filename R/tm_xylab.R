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
#' @param alpha alpha transparency of the text
#' @param side side: `"top"` or `"bottom"` for `tm_xlab` and `"left"` or `"right"` for `tm_ylab`
#' @export
#' @export
tm_xlab = function(text, size, color, rotation, space, fontface, fontfamily, alpha, side) {
	args = lapply(as.list(rlang::call_match()[-1]), eval, envir = parent.frame())
	args$show = TRUE
	names(args) = paste("xlab", names(args), sep = ".")
	do.call(tm_options, args)
}

#' @export
#' @rdname tm_xlab
tm_ylab = function(text, size, color, rotation, space, fontface, fontfamily, alpha, side) {
	args = lapply(as.list(rlang::call_match()[-1]), eval, envir = parent.frame())
	args$show = TRUE
	names(args) = paste("ylab", names(args), sep = ".")
	do.call(tm_options, args)
}
