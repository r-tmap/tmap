#' Convert shape data
#' 
#' Convert numeric data from one polygon shape to another. It uses an intersection matrix, which stores the intersection ratios of the two shape objects per polygon (see \code{\link{intersection_shapes}}).
#' 
#' @param shp.from the shape object, i.e. a \code{\link[sp:SpatialPolygonsDataFrame]{SpatialPolygons(DataFrame)}}, to be converted. It should contain data.
#' @param shp.to the shape object, i.e. a \code{\link[sp:SpatialPolygonsDataFrame]{SpatialPolygons(DataFrame)}}, to be converted into.
#' @param variables.from names of the numeric variables of \code{shp.from} to be converted. If missing, all numeric variables are taken.
#' @param variables.to variable names to be used. Should be the same number of variable names as \code{variables.from}
#' @import rgeos
#' @return Shape object \code{shp.to} with converted data from \code{shp.from}.
#' @note  Running time may be pretty slow.
#' @examples
#'\dontrun{
#' data(NLD_prov)
#' data(NLD_muni)
#' 
#' ## Compare to the original province population data
#' qtm(NLD_muni, fill="population", convert2density = TRUE)
#' qtm(NLD_prov, fill="population", convert2density = TRUE)
#' 
#' ## For illustration, the population of provinces is derived from the municipality data
#' NLD_prov <- convert_shape_data(NLD_muni, NLD_prov, variables.from = "population")
#' qtm(NLD_prov, fill="population.data", convert2density = TRUE)
#' 
#' ## Now, the population of province level is spread equally over the municipalities
#' NLD_muni <- convert_shape_data(NLD_prov, NLD_muni, variables.from = "population")
#' qtm(NLD_muni, fill="population.data", convert2density = TRUE)
#' }
#' @export
convert_shape_data <- function(shp.from, shp.to, variables.from=NULL, variables.to=NULL) {
	x <- intersection_shapes(shp.from, shp.to)

	data.from <- shp.from@data
	if (missing(variables.from)) variables.from <- names(data.from)
	if (missing(variables.to)) variables.to <- variables.from
	
	isnum <- sapply(data.from[,variables.from, drop=FALSE], is.numeric)
	
	if (any(isnum)) {
		ynum <- as.data.frame(t(x) %*% as.matrix(data.from[, variables.from[isnum], drop=FALSE])) 
	} else {
		ynum <- NULL		
	}
	
	if (any(!isnum)) {
		xabs <- x * approx_areas(shp.from)
		maxx <- apply(xabs, MARGIN=2, which.max)
		
		ycat <- data.from[maxx, variables.from[!isnum]]
	} else {
		ycat <- NULL
	}

	y <- if (is.null(ynum)) {
		ycat
	} else if (is.null(ycat)) {
		ynum
	} else cbind(ynum, ycat)

	y <- y[, variables.from, drop=FALSE] 
	names(y) <- variables.to
	shp.to <- append_data(shp.to, y, fixed.order=TRUE)
	shp.to
}

#' Calculate intersection matrix of two shape objects
#' 
#' The value of row i and column j in the intersection matrix corresponds to the proportion of the area of the i'th polygon of the first shape object that intersects the j'th polygon of the second shape object
#' 
#' @param shp.from the first shape object
#' @param shp.to the second shape object
#' @param id.from name of the data variable of \code{shp.from} that contains identification names of the polygons of \code{shp.from}. These are used as row names of the intersection matrix.
#' @param id.to name of the data variable of \code{shp.to} that contains identification names of the polygons of \code{shp.to}. These are used as column names of the intersection matrix.
#' @param absolute should the intersection matrix contain intersection area sizes rather than proportions?
#' @import rgeos
#' @return Intersection matrix with the number of rows equal to the number of polygons of \code{shp.from} and the number of columns equal to the number of polygons of \code{shp.to}.
#' @note  Running time may be pretty slow.
#' @examples
#'\dontrun{
#' data(NLD_prov)
#' data(NLD_muni)
#' 
#' m <- intersection_shapes(NLD_muni, NLD_prov, id.from="name", id.to="name")
#' 
#' }
#' @export
intersection_shapes <- function(shp.from, shp.to, id.from=NULL, id.to=NULL, absolute=FALSE) {
	polys.to <- shp.to@polygons
	polys.from <- shp.from@polygons
	l <- lapply(polys.to, function(p.to) {
		sapply(polys.from, function(p.from) {
			gInt <- gIntersection(SpatialPolygons(list(p.to)), SpatialPolygons(list(p.from)))
			if (is.null(gInt)) return(0) else {
				if (inherits(gInt, "SpatialPolygons")) {
					area.from <- p.from@area
					area.int <- gInt@polygons[[1]]@area
					if (absolute) {
						return(area.int)
					} else return(area.int/area.from)
				}
			}
			return(0)
		})
	})
	if (!missing(id.from)) if (!(id.from %in% names(shp.from))) {
		warning("id.from is not a data variable of shp.from")
		id.from <- NULL
	}
	if (!missing(id.to)) if (!(id.to %in% names(shp.to))) {
		warning("id.to is not a data variable of shp.to")
		id.to <- NULL
	}
	
	m <- do.call("cbind", l)
	
	rownames(m) <- if (is.null(id.from)) {
		get_IDs(shp.from)
	} else {
		shp.from[[id.from]]
	}
	colnames(m) <- if (is.null(id.to)) {
		get_IDs(shp.to)
	} else {
		shp.to[[id.to]]
	}	
	m
}
