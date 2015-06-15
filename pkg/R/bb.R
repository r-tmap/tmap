#' Bounding box generator
#' 
#' Retrieves, modifies, or generates a bounding box. Existing bounding boxes can be retrieved (like \code{\link[sp:bbox]{bbox}}). They can be easily modified in two ways: either by using extension factor (\code{ext}), or by changing the x and y limits. New bounding boxes can be created by setting x and y limits in absolute values.
#' 
#' @param x Either a shape (from class \code{\link[sp:Spatial]{Spatial}} or \code{\link[raster:Raster]{Raster}}) or a bounding box (either 2 by 2 matrix or an \code{\link[raster:Extent]{Extent}} object)
#' @param ext Extension factor of the bounding box. If 1, the bounding box is unchanged. Values smaller than 1 reduces the bounding box, and values larger than 1 enlarges the bounding box. If specified, it overrules \code{xlim}, \code{ylim}, and \code{relative}.
#' @param xlim limits of the x-axis. These are either absolute or relative (depending on the argument \code{relative}).
#' @param ylim limits of the y-axis. See \code{xlim}.
#' @param relative boolean that determines whether relative values are used for \code{xlim} and \code{ylim} or absolute.
#' @param projection projection string (see \code{\link{set_projection}}) to transform the bounding box to. Only applicable if \code{x} is a shape. 
#' @import sp
#' @import raster
#' @export
bb <- function(x=NA, ext=NULL, xlim=NULL, ylim=NULL, relative = FALSE, projection=NULL) {
	if (inherits(x, "Extent")) {
		b <- bbox(x)		
	} else if (inherits(x, c("Spatial", "Raster"))) {
		b <- bbox(x)		
		if (!missing(projection)) {
			sp_rect <- as(extent(b), "SpatialPolygons")
			sp_rect <- set_projection(sp_rect, current.projection = proj4string(x))
			sp_rect_prj <- spTransform(sp_rect, CRSobj = get_proj4_code(projection))
			b <- sp_rect_prj@bbox
		}
	} else if (is.matrix(x) && length(x)==4) {
		b <- x
	} else if (is.vector(x) && length(x)==4) {
		b <- matrix(x, ncol=2)
	} else if (!is.na(x)[1]) {
		warning("Incorrect x argument")	
	} else if (missing(xlim) || missing(xlim)) {
		stop("Argument x is missing. Please specify x, or both xlim and ylim.")	
	} else {
		b <- matrix(NA, ncol=2,nrow=2)
	}
	
	dimnames(b) <- list(c("x", "y"), c("min", "max"))
	
	if (!missing(ext)) {
		xtra <- (ext-1)/2
		xlim <- c(-xtra, 1+xtra)
		ylim <- c(-xtra, 1+xtra)
		relative <- TRUE
	}
	
	if (relative) {
		steps <- b[, 2] - b[, 1]
		xlim <- if (is.null(xlim)) {
			b[1, ]
		} else {
			b[1,1] + xlim * steps[1]
		}
		ylim <- if (is.null(ylim)) {
			b[2, ]
		} else {
			b[2,1] + ylim * steps[2]
		}
	} else {
		if (is.null(xlim)) xlim <- b[1, ]
		if (is.null(ylim)) ylim <- b[2, ]
	}
	matrix(c(xlim, ylim), ncol = 2, byrow=TRUE, 
		   dimnames=list(c("x", "y"), c("min", "max")))
}
