#' Save a thematic map
#' 
#' This function saves a thematic map (from \code{\link{cartoMap}}, \code{\link{cartoMap}} or \code{\link{cartoMap}}) to pdf, png, jpg, ...
#' 
#' @param e expression that generates the plot (note: if a pdf file is specificied, and multiple plots are generated, for instance by chosing multiple variables while keeping \code{mfcol=mfrow=1}, the plots are created on different pages)
#' @param file filename, including the extension (one of pdf, png, jpg, bmp, and tiff)
#' @param width width in inches
#' @param height height in inches
#' @param res the resolution of the plot, i.e. dpi (dots per inch). Only applicable for bitmap formats (all but pdf)
#' @param ... arguments passed on to specific device (e.g. pdf)
#' @export
save_map <- function(e, file, width=7, height=7, res=200, ...) {
	# quick & dirty (should be done with regular expression)
	extension <- substr(file, nchar(file)-2, nchar(file))
	if (extension=="jpg") extension <- "jpeg"
	if (extension=="iff") extension <- "tiff"
	
	if (extension=="pdf") {
		pdf(file=file, width=width, height=height, ...)
	} else {
		do.call(extension, c(list(file=file, width=width, height=height, units="in", res=res), list(...)))
	}
	e
	dev.off()
}