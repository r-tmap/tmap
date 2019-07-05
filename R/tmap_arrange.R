#' Arrange small multiples in grid layout
#' 
#' Arrange small multiples in a grid layout. Normally, small multiples are created by specifying multiple variables for one aesthetic or by specifying the by argument (see \code{\link{tm_facets}}). This function can be used to arrange custom small multiples in a grid layout.
#' 
#' The global option \code{tmap.limits} controls the limit of the number of facets that are plotted. By default, \code{tmap_options(tmap.limits=c(facets.view=4, facets.plot=64))}. The maximum number of interactive facets is set to four since otherwise it may become very slow.
#' 
#' @param ... \code{\link{tmap}} objects or one list of \code{\link{tmap}} objects. The number of multiples that can be plot is limited (see details).
#' @param ncol number of columns
#' @param nrow number of rows
#' @param widths vector of column widths. It should add up to 1 and the length should be equal to \code{ncol}
#' @param heights vector of row heights. It should add up to 1 and the length should be equal to \code{nrow}
#' @param sync logical. Should the navigation in view mode (zooming and panning) be synchronized? By default \code{FALSE}.
#' @param asp aspect ratio. The aspect ratio of each map. Normally, this is controlled by the \code{asp} argument from \code{\link{tm_layout}} (also a tmap option). This argument will overwrite it, unless set to \code{NULL}. The default value for \code{asp} is 0, which means that the aspect ratio is adjusted to the size of the device divided by the number of columns and rows. When \code{asp} is set to \code{NA}, which is also the default value for \code{\link{tm_layout}}, the aspect ratio will be adjusted to the used shapes.
#' @param outer.margins outer.margins, numeric vector four or a single value. If defines the outer margins for each multiple. If will overwrite the \code{outer.margins} argument from \code{\link{tm_layout}}, unless set to \code{NULL}.
#' @param x a \code{tmap_arrange} object (returned from \code{tmap_arrange})
#' @param knit should \code{\link[knitr:knit_print]{knit_print}} be enabled, or the normal \code{\link[base:print]{print}} function?
#' @param options options passed on to knitprint
#' @example ./examples/tmap_arrange.R
#' @export
tmap_arrange <- function(..., ncol=NA, nrow=NA, widths=NA, heights = NA, sync=FALSE, asp=0, outer.margins=.02) {
	tms <- list(...)
	if (!inherits(tms[[1]], "tmap")) {
		if (!is.list(tms[[1]])) stop("The first argument of tmap_arrange is neither a tmap object nor a list.")
		tms <- tms[[1]]
	}
	istmap <- vapply(tms, FUN = inherits, FUN.VALUE = logical(1), what = "tmap")
	if (!all(istmap)) stop("Not all arguments are tmap objects.")
	opts <- list(ncol=ncol, nrow=nrow, widths=widths, heights=heights, sync=sync, asp=asp, outer.margins=outer.margins)
	attr(tms, "opts") <- opts
	class(tms) <- c("tmap_arrange", class(tms))
	tms
}

#' @rdname tmap_arrange
#' @rawNamespace
#' if(getRversion() >= "3.6.0") {
#'   S3method(knitr::knit_print, tmap_arrange)
#' } else {
#'   export(knit_print.tmap_arrange)
#' }
knit_print.tmap_arrange <- function(x, ..., options = NULL) {
	print_tmap_arrange(x, knit=TRUE, ..., options = options)
}

#' @export
#' @rdname tmap_arrange
#' @method print tmap_arrange
print.tmap_arrange <- function(x, knit = FALSE, ..., options = NULL) {
	print_tmap_arrange(x, knit=knit, ..., options = options)
}	




print_tmap_arrange <- function(tms, knit = FALSE, show = TRUE, add.titles = TRUE, ..., options = options) {
	args <- list(...)

	opts <- attr(tms, "opts")
		
	tms_len <- length(tms)
	
	nx <- limit_nx(tms_len)
	if (nx != tms_len) tms <- tms[1:nx]
	
	if (is.na(opts$ncol) && is.na(opts$nrow)) {
		devsize <- dev.size()
		dasp <- devsize[1] / devsize[2]
		
		## determine 'overall' aspect ratio by overlaying the maps		
		#tmp <- tempfile(fileext = ".png")
		#png( tmp, width=700, height=700, res = 100)
		
		curdev <- dev.cur()
		tasps <- vapply(tms, function(tm) {
			asp <- get_asp_ratio(tm, width = 700, height = 700, res = 100)	
			#asp <- print_tmap(tm, return.asp = TRUE, mode = "plot")
			# dev.off()
			asp
		}, numeric(1))
		dev.set(curdev)
		
		#dev.off()
		#dev.off()
		hs <- vapply(tasps, function(tasp) ifelse(tasp>1, 1, 1/tasp), numeric(1))
		ws <- vapply(tasps, function(tasp) ifelse(tasp>1, tasp, 1), numeric(1))
		iasp <- max(ws) / max(hs)
		
		asp_ratio <- iasp / dasp
		
		nrowcol <- get_arrangement(nx = nx, asp_ratio = asp_ratio)
		nrow <- nrowcol[1]
		ncol <- nrowcol[2]
		
		
	} else {
		ncol <- if (is.na(opts$ncol)) ceiling(nx / opts$nrow) else opts$ncol
		nrow <- if (is.na(opts$nrow)) ceiling(nx / opts$ncol) else opts$nrow
	}
	
	widths <- opts$widths
	heights = opts$heights
	if (is.na(widths[1])) widths <- rep(1/ncol, ncol)
	if (is.na(heights[1])) heights <- rep(1/nrow, nrow)
	
	m <- ncol * nrow

	interactive <- (getOption("tmap.mode")=="view")
	
	if (interactive) {
		lfs <- lapply(tms, function(tm) {
			tmap_leaflet(tm, add.titles = FALSE)
		})
		lfmv <- do.call(leafsync::latticeView, c(lfs, list(ncol=ncol, sync=ifelse(opts$sync, "all", "none"))))
		#class(lfmv) <- c("tmap_arrange_view", class(lfmv))
		#return(add_leaflet_titles(lfmv))
		
		if (add.titles) lfmv <- add_leaflet_titles(lfmv)
		
		if (show) {
			if (knit) {
				kp <- get("knit_print", asNamespace("knitr"))
				return(do.call(kp, c(list(x=lfmv), args, list(options=options))))
			} else {
				return(print(lfmv))
			}
		} else lfmv
		
		
	} else {
		grid.newpage()
		vp <- viewport(layout=grid.layout(nrow=nrow, ncol=ncol, widths = widths, heights=heights), name = "tmap_arrangement")
		pushViewport(vp)
		
		if (!is.null(opts$asp) || !is.null(opts$outer.margins)) {
			layout_args <- list(asp=opts$asp, outer.margins=opts$outer.margins)
			layout_args <- layout_args[!vapply(layout_args, is.null, logical(1))]
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
	#dev.off()
	#invisible(tms)
}


