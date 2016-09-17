#' Crop shape object
#' 
#' Crop a shape object (from class \code{\link[sp:Spatial]{Spatial-class}} or \code{\link[raster:Raster-class]{Raster}}).
#' 
#' This function is a wrapper around \code{\link[raster:crop]{crop}} from the raster package. It does two things in addition: \code{\link[sp:SpatialGrid]{SpatialGrid}} objects are allowed, and if \code{y} is a shape object with a different projection, it is temporarily reprojected in order to obtain a bounding box by which \code{x} is cropped.
#' 
#' @param x shape object, i.e. an object from class \code{\link[sp:Spatial]{Spatial-class}} or \code{\link[raster:Raster-class]{Raster}}
#' @param y bounding box (2 by 2 matrix), an \code{\link[raster:extent]{extent}}, or a shape object from which the bounding box is extracted (unless \code{polygon} is \code{TRUE} and \code{x} is a \code{SpatialPolygons} object).
#' @param polygon should \code{x} be cropped by the polygon defined by \code{y}. If \code{FALSE} (default), \code{x} is cropped by the bounding box of \code{x}. Polygon cropping only works when \code{x} is a spatial object and \code{y} is a \code{SpatialPolygons} object.
#' @param ... arguments passed on to \code{\link[raster:crop]{crop}}
#' @export
#' @seealso \code{\link{bb}}
#' @example ../examples/crop_shape.R
crop_shape <- function(x, y, polygon = FALSE,...) {
	xname <- deparse(substitute(x))
	yname <- deparse(substitute(y))

	if (!inherits(x, c("Spatial", "Raster"))) stop(xname, " is not a spatial/raster object.", call.=FALSE)
	
	px <- get_projection(x)
	
	polycut <- polygon && inherits(y, "SpatialPolygons") && !inherits(x, c("Raster", "SpatialGrid"))
	
	if (inherits(y, c("Spatial", "Raster"))) {
		py <- get_projection(y)
		
		# align projections
		if (!is.na(px) && !is.na(py)) {
			if (px!=py) {
				y <- set_projection(y, projection = px)
			}
		}
		if (!polycut) {
			y <- bb(y, as.extent = TRUE)
		}
	} else {
		if (!((is.matrix(y) || is.vector(y)) && length(y)==4) &&
			!inherits(y, "Extent")) {
			stop(yname, " is not a bounding box or an Extent object.", call.=FALSE)
		}
	}
	
	# sp objects are cast rasters for fast crop method
	sp2r2sp <- inherits(x, "SpatialGrid") && !polycut
	
	hasData <- ("data" %in% slotNames(x))
	
	if (sp2r2sp) x <- brick(x)
	
	if (polycut) {
    yunion <- gUnaryUnion(y)
    
    if (inherits(x, "SpatialPoints")) {
      ids <- over(x, yunion)
      x2 <- x[!is.na(ids), ]
    } else {
      x2 <- gIntersection(x, yunion, byid = TRUE, id=as.character(1e9 + 1:length(x)))
      if (hasData) {
        ids <- as.integer(get_IDs(x2))
        if (inherits(x, "SpatialPolygons")) {
          x2 <- SpatialPolygonsDataFrame(x2, x@data[ids-1e9, ], match.ID = FALSE)
        } else if (inherits(x, "SpatialLines")) {
          x2 <- SpatialLinesDataFrame(x2, x@data[ids-1e9, ], match.ID = FALSE)
        }
      }
    }
	} else {
	  # bounding box crop
	  x2 <- crop(x, y, ...)
	  
	  if (sp2r2sp) {
	    if (hasData) data <- get_raster_data(x2)
	    x2 <- as(x2, "SpatialGrid")
	    if (hasData) x2 <- SpatialGridDataFrame(x2, data=data)
	  }
	}

	x2
}
