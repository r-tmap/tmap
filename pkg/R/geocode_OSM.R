#' Geocodes a location using OpenStreetMap Nominatim
#' 
#' Geocodes a location (based on a search query) to coordinates and a bounding box. Similar to geocode from the ggmap package. It uses OpenStreetMap Nominatim. For processing large amount of queries, please read the usage policy (\url{http://wiki.openstreetmap.org/wiki/Nominatim_usage_policy}).
#' 
#' @param q a character (vector) that specifies a search query. For instance \code{"India"} or \code{"CBS Weg 11, Heerlen, Netherlands"}.
#' @param projection projection in which the coordinates and bounding box are returned. Either a \code{PROJ.4} character string or a shortcut. See \code{\link{get_proj4}} for a list of shortcut values.
#' @param return.first.only Only return the first result
#' @param details provide output details, other than the point coordinates and bounding box
#' @param as.data.frame Return the output as a \code{data.frame}. If \code{FALSE}, a list is returned with at least two items: \code{"coords"}, a vector containing the coordinates, and \code{"bbox"}, the corresponding bounding box. By default false, unless \code{q} contains multiple queries
#' @param as.SPDF Return the output as \code{\link[sp:SpatialPointsDataFrame]{SpatialPointsDataFrame}}. If \code{TRUE}, \code{return.first.only} will be set to \code{TRUE}.
#' @param server OpenStreetMap Nominatim server name. Could also be a local OSM Nominatim server.
#' @return If \code{as.SPDF} then a \code{\link[sp:SpatialPointsDataFrame]{SpatialPointsDataFrame}} is returned. Else, if \code{as.data.frame}, then a \code{data.frame} is returned, else a list.
#' @export
#' @importFrom XML xmlChildren xmlRoot xmlAttrs xmlTreeParse xmlValue
#' @example ../examples/geocode_OSM.R
#' @seealso \code{\link{rev_geocode_OSM}}, \code{\link{bb}}
geocode_OSM <- function(q, projection="longlat", return.first.only=TRUE, details=FALSE, as.data.frame=NA, as.SPDF=FALSE, server="http://nominatim.openstreetmap.org") {
	n <- length(q)
	q2 <- gsub(" ", "+", enc2utf8(q), fixed = TRUE)
	addr <- paste0(server, "/search?q=", q2, "&format=xml&polygon=0&addressdetails=0")
	
	if (is.na(as.data.frame)) as.data.frame <- (n>1)
	if (as.SPDF) {
		as.data.frame <- TRUE
		return.first.only <- TRUE
	}

	
	output2 <- lapply(1:n, function(k) {
		tmpfile <- tempfile()
		suppressWarnings(download.file(addr[k], destfile = tmpfile, mode= "wb", quiet = TRUE))
		
		doc <- xmlTreeParse(tmpfile, encoding="UTF-8")
		unlink(tmpfile)
		
		res <- xmlChildren(xmlRoot(doc)) 
		
		if (length(res)==0) {
			warning(paste("No results found for \"", q[k], "\".", sep="")) #if (n==1) 
			return(NULL)
		}
		
		idx <- if (return.first.only) 1 else 1:length(res)
		
		sn_names <- c("place_id", "osm_type", "osm_id", "place_rank", "display_name", "class", "type", "importance", "icon")
		output <- lapply(idx, function(i) {
			search_result <- xmlAttrs(res[[i]])

			search_result_id <- search_result[sn_names]
			names(search_result_id) <- sn_names # in case of missings
			Encoding(search_result_id) <- "UTF-8"
			
			search_result_loc <- as.numeric(search_result[c("lat", "lon")])
			names(search_result_loc) <- c("lat", "lon")
			
			search_result_bb <- as.numeric(unlist(strsplit(search_result["boundingbox"], ",")))
			
			if (projection=="longlat") {
				names(search_result_bb) <- c("lat_min", "lat_max", "lon_min", "lon_max")
				b <- bb(xlim=search_result_bb[3:4], ylim=search_result_bb[1:2])
				
				coords <- search_result_loc[c("lon", "lat")]
				names(coords) <- c("x", "y")
				
			} else {
				b <- bb(xlim=search_result_bb[3:4], ylim=search_result_bb[1:2], current.projection = get_proj4("longlat"), projection=get_proj4(projection))
				
				search_result_bb <- b[c(2,4,1,3)]
				names(search_result_bb) <- c("y_min", "y_max", "x_min", "x_max")
				
				
				p <- SpatialPoints(matrix(search_result_loc[2:1], nrow=1), proj4string=CRS(get_proj4("longlat")))
				p <- set_projection(p, projection=projection)
				coords <- as.vector(attr(p, "coords"))
				names(coords) <- c("x", "y")
				
				search_result_loc <- as.list(coords)
				names(search_result_loc) <- c("x", "y")
			}

			res <- if (as.data.frame) {
				c(list(query=q[k]),
				  search_result_loc,
				  search_result_bb)
			} else {
				c(list(query=q[k], 
					   coords=coords,
					   bbox=b))
			}
			
			if (details) res <- c(res, search_result_id)
			if (as.data.frame) res <- as.data.frame(res, stringsAsFactors=FALSE)
			res
		})
	})
	
	output3 <- do.call(c, output2)
	
	if (as.data.frame) {
		df <- do.call(rbind, output3)
		
		if (as.SPDF) {
			if (projection=="longlat") {
				spdf <- SpatialPointsDataFrame(df[, c("lon", "lat")], proj4string=CRS(get_proj4("longlat")), 
											   data = df,
											   match.ID = FALSE)
			} else {
				spdf <- SpatialPointsDataFrame(df[, c("x", "y")], proj4string=CRS(get_proj4(projection)), 
											   data = df,
											   match.ID = FALSE)
			}
			spdf
		} else {
			df
		}
	} else {
		if (length(output3)==1) {
			output3[[1]]
		} else output3
	}
}


#' Reverse geocodes a location using OpenStreetMap Nominatim
#' 
#' Reverse geocodes a location (based on spatial coordinates) to an address. It uses OpenStreetMap Nominatim. For processing large amount of queries, please read the usage policy (\url{http://wiki.openstreetmap.org/wiki/Nominatim_usage_policy}).
#' 
#' @param x x coordinate(s), or a \code{\link[sp:SpatialPoints]{SpatialPoints}} object
#' @param y y coordinate(s)
#' @param zoom zoom level
#' @param projection projection in which the coordinates \code{x} and \code{y} are provided
#' @param as.data.frame return as data.frame (\code{TRUE}) or list (\code{FALSE}). By default a list, unless multiple coordinates are provided.
#' @param server OpenStreetMap Nominatim server name. Could also be a local OSM Nominatim server.
#' @export
#' @importFrom XML xmlChildren xmlRoot xmlAttrs xmlTreeParse xmlValue
#' @return A data frmame with all atributes that are contained in the search result
#' @example ../examples/rev_geocode_OSM.R
#' @seealso \code{\link{geocode_OSM}}
rev_geocode_OSM <- function(x, y=NULL, zoom=NULL, projection="longlat", as.data.frame=NA, server="http://nominatim.openstreetmap.org") {
	if (inherits(x, "SpatialPoints")) {
		
		isproj <- is.projected(x)
		
		if (is.na(isproj)) {
			warning("Projection of SpatialPoints object unknown. Assuming ", projection)
			if (projection!="longlat") x <- set_projection(x, current.projection = projection, projection="longlat")
		} else {
			if (isproj) {
				x <- set_projection(x, projection = "longlat")
			}
		}
		n <- length(x)
		co <- coordinates(x)
		lon <- x <- co[,1]
		lat <- y <- co[,2]
	} else {
		if (length(x) > 1 || length(y) > 1) {
			n <- max(length(x), length(y))
			x <- rep(x, length.out=n)
			y <- rep(y, length.out=n)
		}
		if (projection=="longlat") {
			lon <- x
			lat <- y
		} else {
			projection <- get_proj4(projection)
			single_point <- SpatialPoints(matrix(c(x,y), ncol=2), proj4string=CRS(projection))
			coords <- attr(set_projection(single_point, projection = get_proj4("longlat")), "coords")
			lon <- coords[,1]
			lat <- coords[,2]
		}
	}
	
	if (is.na(as.data.frame)) as.data.frame <- (n>1)
	
	if (missing(zoom)) {
		qzoom <- ""
		strzoom <- ""
	} else {
		qzoom <- paste0("&zoom=", zoom)
		strzoom <- paste0(", zoom = ", zoom)
	}
	
	addr <- paste0(server, "/reverse?format=xml&lat=", lat, "&lon=", lon, qzoom, "&addressdetails=1")
	

	dfs <- lapply(1:n, function(i) {
		# download query
		tmpfile <- tempfile()
		suppressWarnings(download.file(addr[i], destfile = tmpfile, mode= "wb", quiet = TRUE))
		doc <- xmlTreeParse(tmpfile, encoding="UTF-8")
		unlink(tmpfile)
		
		# read xml document
		res <- xmlChildren(xmlRoot(doc)) 
		
		# get name
		result_name <- xmlValue(res[[1]])
		Encoding(result_name) <- "UTF-8"
		
		# get osm id, location, bbox
		search_result <- xmlAttrs(res[[1]])
		search_result_id <- search_result[c("place_id", "osm_type", "osm_id", "ref")]
		names(search_result_id) <- c("place_id", "osm_type", "osm_id", "ref") # in case of missings
		Encoding(search_result_id) <- "UTF-8"
		search_result_ll <- as.numeric(search_result[c("lat", "lon")])
		names(search_result_ll) <- c("lat", "lon")
		search_result_bb <- as.numeric(unlist(strsplit(search_result["boundingbox"], ",")))
		names(search_result_bb) <- c("lat_min", "lat_max", "lon_min", "lon_max")
		
		# get address
		addr_result <- xmlChildren(res[[2]])
		dfnames <- names(addr_result)
		dfvalues <- lapply(1:length(addr_result), function(j) {
			v <- xmlValue(addr_result[[j]])	
			Encoding(v) <- "UTF-8"
			v
		})
		names(dfvalues) <- dfnames

		c(list(x=x[i],
			 y=y[i],
			 name=result_name),
		  search_result_id,
		  search_result_ll,
		  search_result_bb,
		  dfvalues)
	})
	
	# cast to data.frame
	if (as.data.frame) {
		addrnames <- sort(unique(unlist(lapply(dfs, function(df) {
			names(df)[14:length(df)]
		}))))
		
		addrlist <- lapply(addrnames, function(a) NA)
		names(addrlist) <- addrnames
		
		do.call(rbind, c(lapply(dfs, function(df) {
			sel <- 14:length(df)
			addrlist[names(df)[sel]] <- df[sel]
			as.data.frame(c(df[1:13], addrlist), stringsAsFactors=FALSE)
		}), list(stringsAsFactors=FALSE)))
	} else {
		dfs
	}
}
