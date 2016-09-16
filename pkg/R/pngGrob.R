#' Render a png image
#' 
#' Render a png image (from file path or url) to a grob, which can be used for proportional symbol maps
#' 
#' @param file character value containing the ile path or url
#' @return \code{\link[grid:rasterGrob]{rasterGrob}}
#' @export
#' @seealso \code{\link{tm_symbols}}
pngGrob <- function(file) {
	if (!requireNamespace("png", quietly = TRUE)) {
		stop("png package needed for this function to work. Please install it.",
			 call. = FALSE)
	} else {
		pu <- is_path_or_url(file)
		if (is.na(pu)) {
			stop(file, " is neither a valid path nor url", call.=FALSE)
		}
		
		if (!pu) {
			tmpfile <- tempfile(fileext=".png")
			download.file(file, destfile=tmpfile, mode="wb")
			file <- tmpfile
		}
		
		x <- png::readPNG(file)
		rasterGrob(x)
	}
}

is_path_or_url <- function(file) {
	if (file.exists(file)) {
		TRUE
	} else {
		con.url <- suppressWarnings(try({
			u <- url(file, open='rb')
			close(u)
		}, silent=TRUE))
		try.error <- inherits(con.url, "try-error")
		if (try.error) NA else FALSE
	}
}
