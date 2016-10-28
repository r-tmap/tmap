#' Set and get the map projection
#' 
#' The function \code{set_projection} sets the projection of a shape file. It is
#' a convenient wrapper of \code{\link[sp:spTransform]{spTransform}} and
#' \code{\link[raster:projectRaster]{projectRaster}} with shortcuts for commonly
#' used projections. The projection can also be set directly in the plot call
#' with \code{\link{tm_shape}}. This function is also used to set the current
#' projection information if this is missing. The function \code{get_projection}
#' is used to get the projection information.
#' 
#' For raster objects, the projection method is based on the type of data. For numeric layers, the bilinear method is used, and for categorical layers the nearest neighbor. See \code{\link[raster:projectRaster]{projectRaster}} for details.
#' 
#' @param shp shape object of class \code{\link[sp:Spatial]{Spatial}} or
#'   \code{\link[raster:Raster-class]{Raster}} (see details).
#' @param projection new projection. Either a \code{\link[sp:CRS]{CRS}} object or a character value. If it is a character, it can either be a \code{PROJ.4} character string or a shortcut. See \code{\link{get_proj4}} for a list of shortcut values. This argument is only used to transform the \code{shp}. Use \code{current.projection} to specify the current projection of \code{shp}.
#' @param current.projection the current projection of \code{shp}. See \code{projection} for possible formats. Only use this if the current projection is missing or wrong.
#' @param overwrite.current.projection logical that determines whether the current projection is overwritten if it already has a projection that is different.
#' @param as.CRS should a CRS object be returned instead of a PROJ.4 character string? Default is \code{FALSE}.
#' @name set_projection
#' @rdname set_projection
#' @import sp
#' @importFrom raster projectRaster
#' @importFrom rgdal getPROJ4VersionInfo
#' @return \code{set_projection} returns a (transformed) shape object with updated projection information. \code{get_projection} returns the \code{PROJ.4} character string of \code{shp}.
#' @export
set_projection <- function(shp, projection=NA, current.projection=NA, overwrite.current.projection=FALSE) {
	shp.name <- deparse(substitute(shp))
	
	shp.CRS <- get_projection(shp, as.CRS = TRUE)
	current.CRS <- get_proj4(current.projection, as.CRS=TRUE)
	proj.CRS <- get_proj4(projection, as.CRS=TRUE)

	if (is.na(shp.CRS)) {
		if (is.na(current.CRS)) {
			stop("Currect projection of shape object unknown. Please specify the argument current.projection. The value \"longlat\", which stands for Longitude-latitude (WGS84), is most commonly used.")
		} else {
			if (inherits(shp, "Spatial")) {
				shp@proj4string <- current.CRS
			} else {
				shp@crs <- current.CRS
			}
			#current.projection <- current.proj4
		}
	} else {
		if (!is.na(current.CRS)) {
			if (identical(current.CRS, shp.CRS)) {
				warning("Current projection of ", shp.name, " already known.", call. = FALSE)
			} else {
				if (overwrite.current.projection) {
					warning("Current projection of ", shp.name, " differs from ", CRSargs(current.CRS), ", but is overwritten.", call. = FALSE)
					if (inherits(shp, "Spatial")) {
						shp@proj4string <- current.CRS
					} else {
						shp@crs <- current.CRS
					}
					
				} else {
					stop(shp.name, " already has projection: ", CRSargs(shp.CRS), ". This is different from the specified current projection ", CRSargs(current.CRS), ". If the specified projection is correct, use overwrite.current.projection=TRUE.", call. = FALSE)
				}
			} 
		} else {
			current.CRS <- shp.CRS
		}
	}
	

	if (!is.na(proj.CRS)) {
		PROJ4_version_nr <- get_proj4_version()
		
		if (length(grep("+proj=wintri", CRSargs(current.CRS), fixed = TRUE)) && PROJ4_version_nr < 491) {
			stop("Unable to reproject a shape from the Winkel Tripel projection with PROJ.4 version < 4.9.1")
		}	
		
		cls <- class(shp)
		
		if (inherits(shp, c("SpatialGrid", "SpatialPixels"))) {
			shp <- brick(shp)
			recast <- TRUE
		} else {
			recast <- FALSE
		}
		
		if (inherits(shp, "Raster")) {
			#raster_data <- get_raster_data(shp)
			has_color_table <- (length(colortable(shp))>0)
			
			# get factor levels (to be restored later)
			lvls <- get_raster_levels(shp, 1:nlayers(shp))
			# override factor levels with colortable values
			if (has_color_table) {
				lvls <- list(colortable(shp))
#				raster_data <- data.frame(PIXEL__COLOR=getValues(shp[[1]])+1L)
			}
			isnum <- sapply(lvls, is.null)
			new_ext <- suppressWarnings(projectExtent(shp, crs = proj.CRS))
			if (any(isnum) && !all(isnum)) {
				shp_num <- raster::subset(shp, subset=which(isnum))
				shp_cat <- raster::subset(shp, subset=which(!isnum))
				shp_num2 <- suppressWarnings(projectRaster(shp_num, to=new_ext, crs=proj.CRS, method="bilinear"))
				shp_cat2 <- suppressWarnings(projectRaster(shp_cat, to=new_ext, crs=proj.CRS, method="ngb"))
				
				# restore order
				o <- order(c(which(isnum), which(!isnum)))
				rlayers <- c(lapply(1:nlayers(shp_num), function(i) raster(shp_num2, layer=i)),
							 lapply(1:nlayers(shp_cat), function(i) raster(shp_cat2, layer=i)))[o]
				shp <- do.call("brick", rlayers)
			} else if (all(isnum)) {
				shp <- suppressWarnings(projectRaster(shp, to=new_ext, crs=proj.CRS, method="bilinear"))
			} else {
				shp <- suppressWarnings(projectRaster(shp, to=new_ext, crs=proj.CRS, method="ngb"))
			}
			
			# new_raster_data <- as.data.frame(mapply(function(d, l) {
			# 	if (!is.null(l) && !is.factor(d)) factor(d, levels=1:length(l), labels=l) else d
			# }, get_raster_data(shp), lvls, SIMPLIFY=FALSE))
			
			if (any(!isnum)) {
				shp@data@isfactor <- !isnum
				dfs <- mapply(function(nm, lv) {
					df <- data.frame(ID=1:length(lv), levels=factor(lv, levels=lv))
					if (cls=="RasterBrick") names(df)[2] <- nm
					df
				}, names(which(!isnum)), lvls[!isnum], SIMPLIFY=FALSE)
				shp@data@attributes <- dfs
			}
		} else {
			shp <- spTransform(shp, proj.CRS)
		}
		if (recast) {
			shp <- as(shp, cls)
			names(shp) <- names(isnum)
		}
	}
		
	shp
}

get_proj4_version <- function() {
	PROJ4_version <- rgdal::getPROJ4VersionInfo()
	vid <- gregexpr("PJ_VERSION: ", PROJ4_version, fixed = TRUE)[[1]][1] + 12
	as.integer(substr(PROJ4_version, vid, nchar(PROJ4_version)-1))
}
	

#' @name get_projection
#' @rdname set_projection
#' @import sp
#' @export
get_projection <- function(shp, as.CRS=FALSE) {
	if (as.CRS) {
		if (inherits(shp, "Spatial")) {
			attr(shp, "proj4string")
		} else if (inherits(shp, "Raster")) {
			attr(shp, "crs")
		} else {
			stop("shp is neither a Spatial nor a Raster object")
		}
	} else {
		proj4string(shp)
	}
}
