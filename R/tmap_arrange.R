#' Arrange small multiples in grid layout
#' 
#' Arrange small multilples in a grid layout. Normally, small multiples are created by specifying multiple variables for one aesthetic or by specifying the by argument (see \code{\link{tm_facets}}). This function can be used to arrange custom small multiples in a grid layout.
#' 
#' The global option \code{tmap.limits} controlls the limit of the number of facets that are plotted. By default, \code{tmap_options(tmap.limits=c(facets.view=4, facets.plot=64))}. The maximum number of interactive facets is set to four since otherwise it may become very slow.
#' 
#' @param ... \code{\link{tmap}} objects. The number of multiples that can be plot is limited (see details).
#' @param ncol number of columns
#' @param nrow number of rows
#' @param sync logical. Should the navigation in view mode (zooming and panning) be synchronized? By default \code{FALSE}.
#' @param asp aspect ratio. If will overwrite the \code{asp} argument from \code{\link{tm_layout}}, unless set to \code{NULL}
#' @param outer.margins outer.margins, numeric vector four or a single value. If defines the outer margins for each multiple. If will overwrite the \code{outer.margins} argument from \code{\link{tm_layout}}, unless set to \code{NULL}.
#' @example ./examples/tmap_arrange.R
#' @export
tmap_arrange <- function(..., ncol=NA, nrow=NA, sync=FALSE, asp=0, outer.margins=.02) {
	tms <- list(...)
	tms_len <- length(tms)
	
	nx <- limit_nx(tms_len)
	if (nx != tms_len) tms <- tms[1:nx]
	

	if (is.na(ncol) && is.na(nrow)) {
		devsize <- graphics::par("din")
		dasp <- devsize[1] / devsize[2]
		
		## determine 'overall' aspect ratio by overlaying the maps		
		tmp <- tempfile(fileext = ".png")
		tasps <- sapply(tms, function(tm) {
			png( tmp, width=700, height=700, res = 100)
			asp <- print_tmap(tm, return.asp = TRUE, mode = "plot")
			dev.off()
			asp
		})
		hs <- sapply(tasps, function(tasp) ifelse(tasp>1, 1, 1/tasp))
		ws <- sapply(tasps, function(tasp) ifelse(tasp>1, tasp, 1))
		iasp <- max(ws) / max(hs)
		
		asp_ratio <- iasp / dasp
		
		nrowcol <- get_arrangement(nx = nx, asp_ratio = asp_ratio)
		nrow <- nrowcol[1]
		ncol <- nrowcol[2]
		
		
	} else {
		if (is.na(ncol)) ncol <- ceiling(nx / nrow)
		if (is.na(nrow)) nrow <- ceiling(nx / ncol)
	}
	m <- ncol * nrow
	
	interactive <- (getOption("tmap.mode")=="view")
	
	if (interactive) {
		lfs <- lapply(tms, function(tm) {
			tmap_leaflet(tm)
		})
		lfmv <- do.call(mapview::latticeView, c(lfs, list(ncol=ncol, sync=sync)))
		add_leaflet_titles(lfmv)
	} else {
		grid.newpage()
		vp <- viewport(layout=grid.layout(nrow=nrow, ncol=ncol), name = "tmap_arrangement")
		pushViewport(vp)
		
		if (!is.null(asp) || !is.null(outer.margins)) {
			layout_args <- list(asp=asp, outer.margins=outer.margins)
			layout_args <- layout_args[!sapply(layout_args, is.null)]
			tml <- do.call(tm_layout, layout_args)
		} else {
			tml <- NULL
		}
		
		nc <- 1
		nr <- 1
		for (i in 1:nx) {
			tm <- tms[[i]]
			if (!is.null(tml)) tm <- tm + tml
			print_tmap(tm, vp = viewport(layout.pos.col = nc, layout.pos.row = nr))
			nc <- nc + 1
			if (nc > ncol) {
				nc <- 1
				nr <- nr + 1
			}
		}
	}
}
