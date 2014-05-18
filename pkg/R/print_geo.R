#' Print geo object
#' 
#' Print geo object
#' 
#' @param x geo object
#' @param ... not used
#' @import sp
#' @import RColorBrewer
#' @import grid
#' @import gridBase
#' @import classInt
#' @export
#' @method print geo
print.geo <- function(x, ...) {
	result <- process_geo(x)
	gmeta <- result$gmeta
	gps <- result$gps
	nx <- result$nx

	
	# backup par settings
	#opar <- par("mai", "xaxs", "yaxs")
	opar <- par(no.readonly=TRUE)
	
	par(mai=c(0,0,0,0), oma=c(0,0,0,0))
	plot.new()
	#grid.newpage()
	
	gridplot(gmeta$geo_grid$nrow, gmeta$geo_grid$ncol, "plot_all", nx, gps$shps, gps$multiples)
	do.call("par", opar)
	
}
