#' Draw thematic map
#' 
#' Draw thematic map.
#' 
#' @param x tmap object. 
#' @param ... not used
#' @export
#' @method print tmap
print.tmap = function(x, ...) {
	x2 = step1_rearrange(x)
	x3 = step2_data(x2)
	x4 = step3_trans(x3)
	step4_plot(x4)
}
