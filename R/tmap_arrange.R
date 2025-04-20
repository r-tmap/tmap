#' Arrange small multiples in grid layout
#'
#' Arrange small multiples in a grid layout. Normally, small multiples are created
#' by specifying multiple variables for one aesthetic or by specifying the by argument
#' (see [tm_facets()]). This function can be used to arrange custom small multiples in a grid layout.
#'
#' The global option `tmap.limits` controls the limit of the number of facets that are plotted.
#' By default, `tmap_options(tmap.limits = c(facets.view=4, facets.plot=64))`.
#' The maximum number of interactive facets is set to four since otherwise it may become very slow.
#'
#' @param ... [`tmap`] objects or one list of [`tmap`] objects.
#'   The number of multiples that can be plot is limited (see details).
#' @param ncol number of columns
#' @param nrow number of rows
#' @param widths vector of column widths. It should add up to 1 and the length
#'   should be equal to `ncol`.
#' @param heights vector of row heights. It should add up to 1 and the length
#'   should be equal to `nrow`.
#' @param sync logical. Should the navigation in view mode (zooming and panning)
#'   be synchronized? By default `FALSE`.
#' @param asp aspect ratio. The aspect ratio of each map. Normally, this is
#'   controlled by the `asp` argument from [tm_layout()] (also a tmap option).
#'   This argument will overwrite it, unless set to `NULL`.
#'   The default value for `asp` is 0, which means that the aspect ratio is
#'   adjusted to the size of the device divided by the number of columns and rows.
#'   When `asp` is set to `NA`, which is also the default value for `tm_layout()`,
#'   the aspect ratio will be adjusted to the used shapes.
#' @param outer.margins outer.margins, numeric vector four or a single value.
#'   If defines the outer margins for each multiple. If will overwrite the
#'   `outer.margins` argument from [tm_layout()], unless set to `NULL`.
#' @param x a `tmap_arrange` object (returned from `tmap_arrange()`).
#' @param knit should [knitr::knit_print()] be enabled, or the normal [base::print()] function?
#' @param options options passed on to [knitr::knit_print()]
#' @example ./examples/tmap_arrange.R
#' @export
tmap_arrange <- function(..., ncol = NA, nrow = NA, widths = NA, heights = NA, sync = FALSE, asp = 0, outer.margins = .02) {
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
#' @exportS3Method knitr::knit_print
knit_print.tmap_arrange <- function(x, ..., options = NULL) {
	print_tmap_arrange(x, knit=TRUE, ..., options = options)
}

#' @export
#' @rdname tmap_arrange
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

		curdev <- grDevices::dev.cur()
		rmc = tmap_options(raster.max_cells = 36)

		on.exit(tmap_options(rmc))

		tasps <- suppressMessages({
			vapply(tms, function(tm) {
				asp <- get_asp_ratio(tm, width = 700, height = 700, res = 100)
				#asp <- print_tmap(tm, return.asp = TRUE, mode = "plot")
				# dev.off()
				asp
			}, numeric(1))
		})

		grDevices::dev.set(curdev)
		tmap_options(rmc)

		#dev.off()
		#dev.off()
		hs <- vapply(tasps, function(tasp) ifelse(tasp>1, 1, 1/tasp), numeric(1))
		ws <- vapply(tasps, function(tasp) ifelse(tasp>1, tasp, 1), numeric(1))
		iasp <- max(ws) / max(hs)

		asp_ratio <- iasp / dasp

		nrowcol <- process_get_arrangement(nx = nx, asp_ratio = asp_ratio)
		nrow <- nrowcol[1]
		ncol <- nrowcol[2]


	} else {
		ncol <- if (is.na(opts$ncol)) ceiling(nx / opts$nrow) else opts$ncol
		nrow <- if (is.na(opts$nrow)) ceiling(nx / opts$ncol) else opts$nrow
	}


	interactive <- (getOption("tmap.mode")=="view")
	gs = tmap_graphics_name()

	fun = paste0("tmap", gs, "Arrange")

	do.call(fun, list(tms = tms, nx = nx, ncol = ncol, nrow = nrow, opts = opts, knit = knit, show = show, args = args, options = options))

}





process_get_arrangement <- function(nx, asp_ratio) {
	#           asp ~ nrow
	#       |--------------
	#   1   |
	# ~ncol |       nx
	#       |
	ncol_init <- sqrt(nx/asp_ratio)
	nrow_init <- nx / ncol_init

	# rounding:
	nrow_ceiling <- min(ceiling(nrow_init), nx)
	ncol_ceiling <- min(ceiling(ncol_init), nx)

	# find minimal change
	nrow_xtra <- abs(nrow_ceiling - nrow_init) * ncol_init
	ncol_xtra <- abs(ncol_ceiling - ncol_init) * nrow_init

	# calculaet the other, and subtract 1 when possible
	if (nrow_xtra < ncol_xtra) {
		nrow <- nrow_ceiling
		ncol <- ceiling(nx / nrow)
		if ((nrow-1) * ncol >= nx) nrow <- nrow - 1
	} else {
		ncol <- ncol_ceiling
		nrow <- ceiling(nx / ncol)
		if ((ncol-1) * nrow >= nx) ncol <- ncol - 1
	}
	c(nrow=nrow, ncol=ncol)
}

