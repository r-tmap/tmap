#' Save tmap
#' 
#' Save tmap to a file, such as png, jpg, or pdf.
#'
#' @param tm tmap object
#' @param filename filename including extension, and optionally the path. The extensions pdf, eps, svg, wmf (Windows only), png, jpg, bmp, or tiff are supported. Use \code{\link{itmap}} to create an interactive svg.
#' @param width width. Defaults to the width of current plotting window. Units are set with the argument \code{units}.
#' @param height height. Defaults to the height of current plotting window. Units are set with the argument \code{units}.
#' @param units units for width and height when either one is explicitly specified (in, cm, or mm)
#' @param dpi dots per inch. Only applicable for raster graphics.
#' @param outer.margins overrides the outer.margins argument of \code{\link{tm_layout}} (unless set to \code{NA})
#' @param asp overrides the asp argument of \code{\link{tm_layout}} (unless set to \code{NA})
#' @param scale overrides the scale argument of \code{\link{tm_layout}} (unless set to \code{NA})
#' @param ... arguments passed on to device functions
#' @examples 
#' \dontrun{
#' data(NLD_muni, NLD_prov)
#' (tm_shape(NLD_muni) +
#' 	 tm_fill(col="population", convert2density=TRUE, 
#' 	   style="kmeans", title="Population (per km2)", legend.hist=FALSE) +
#' 	 tm_borders("black", alpha=.5) + 
#' tm_shape(NLD_prov) +
#' 	 tm_borders("grey25", lwd=2) +
#' tm_format_NLD(inner.margins = c(.02, .15, .06, .15)) + 
#' tm_scale_bar(position = c("left", "bottom")) +
#' tm_compass(position=c("right", "bottom")) + 
#' tm_style_classic()) %>% save_tmap()
#' }
#' @export
save_tmap <- function(tm, filename=shp_name(tm), width=par("din")[1], height=par("din")[2], units = c("in", "cm", "mm"),
					  dpi=300, outer.margins=0, asp=0, scale=NA, ...) {
	eps <- ps <- function(..., width, height) grDevices::postscript(..., 
																	width = width, height = height, onefile = FALSE, horizontal = FALSE, 
																	paper = "special")
	tex <- function(..., width, height) grDevices::pictex(..., 
														  width = width, height = height)
	pdf <- function(..., version = "1.4") grDevices::pdf(..., 
														 version = version)
	svg <- function(...) grDevices::svg(...)
	wmf <- function(..., width, height) grDevices::win.metafile(..., 
																width = width, height = height)
	emf <- function(..., width, height) grDevices::win.metafile(..., 
																width = width, height = height)
	png <- function(..., width, height) grDevices::png(..., width = width, 
													   height = height, res = dpi, units = "in")
	jpg <- jpeg <- function(..., width, height) grDevices::jpeg(..., 
																width = width, height = height, res = dpi, units = "in")
	bmp <- function(..., width, height) grDevices::bmp(..., width = width, 
													   height = height, res = dpi, units = "in")
	tiff <- function(..., width, height) grDevices::tiff(..., 
														 width = width, height = height, res = dpi, units = "in")
	shp_name <- function(tm) {
		paste(tm[[1]]$shp_name, ".pdf", sep = "")
	}
	default_device <- function(filename) {
		pieces <- strsplit(filename, "\\.")[[1]]
		ext <- tolower(pieces[length(pieces)])
		match.fun(ext)
	}
	units <- match.arg(units)
	convert_to_inches <- function(x, units) {
		x <- switch(units, `in` = x, cm = x/2.54, mm = x/2.54/10)
	}
	convert_from_inches <- function(x, units) {
		x <- switch(units, `in` = x, cm = x * 2.54, mm = x * 
						2.54 * 10)
	}
	if (!missing(width)) {
		width <- convert_to_inches(width, units)
	}
	if (!missing(height)) {
		height <- convert_to_inches(height, units)
	}
	if (missing(width) || missing(height)) {
		message("Saving ", prettyNum(convert_from_inches(width, units), digits = 3), " x ", prettyNum(convert_from_inches(height, units), digits = 3), " ", units, " image")
	}
	device <- default_device(filename)
	
	device(file = filename, width = width, height = height, ...)
	on.exit(capture.output(dev.off()))
	args <- list(outer.margins=outer.margins, asp=asp)
	if (!is.na(scale)) args$scale <- scale
	print(tm + do.call("tm_layout", args))
	invisible()
}
