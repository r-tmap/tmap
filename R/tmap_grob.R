#' Export to grob object
#' 
#' Export a tmap plot object to a grob object (from the grid package).
#' 
#' @param tm tmap object
#' @export
#' @return A grob object when one page is generated, or a list of grob objects when multiple pages are generated.
#' @example ./examples/tmap_grob.R
tmap_grob = function(tm) {
	.tmapOptions <- get("tmapOptions", envir = .TMAP_CACHE)
	show.messages <-.tmapOptions$show.messages
	
	
	x = print(tm, show = FALSE)
	asp = x$gps$plot1$tm_layout$shape.dasp
	if (length(x$grb) == 1L) {
		grb = x$grb[[1]]
		if (show.messages) message("Note that the aspect ratio for which the grob has been generated is ", format(asp, digits = 3))
	} else {
		grb = x$grb
		if (show.messages) message("A list of grobs will be returned, one for each page. Note that the aspect ratio for which each grob has been generated is ", format(asp, digits = 3))
	}
	grb
}
