#' Map layer: RGB
#' 
#' @param col,col.scale,col.legend,col.free Visual variable that determines the col color.
#' @export
tm_rgb = function(col = tm_mv(1:3),
				  col.scale = tm_scale_rgb(),
				  col.legend = tm_legend(),
				  col.free = NA) {
	do.call(tm_raster, args = list(col = col, col.scale = col.scale, col.legend = col.legend, col.free = col.free))
}