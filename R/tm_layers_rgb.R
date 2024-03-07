#' Map layer: RGB
#' 
#' @param col,col.scale,col.legend,col.chart,col.free Visual variable that determines
#'   the col color.
#' @export
tm_rgb = function(col = tm_mv(1:3),
				  col.scale = tm_scale_rgb(),
				  col.legend = tm_legend(),
				  col.chart = tm_chart_none(),
				  col.free = NA) {
	do.call(tm_raster, args = list(col = col, col.scale = col.scale, col.legend = col.legend, col.chart = col.chart, col.free = col.free))
}
