#' Geocodes a location using OpenStreetMap Nominatim
#' 
#' Geocodes a location (based on a search query) to coordinates and a bounding box. Similar to geocode from the ggmap package. It uses OpenStreetMap Nominatim.
#' 
#' @param q a character that specifies a search query. For instance \code{"India"} or \code{"CBS Weg 11, Heerlen, Netherlands"}.
#' @param projection projection in which the coordinates and bounding box are returned. Either a \code{PROJ.4} character string or a shortcut. See \code{\link{get_proj4}} for a list of shortcut values.
#' @param return.first.only Only return the first result
#' @param details provide output details, other than the point coordinates and bounding box
#' @param as.data.frame Return the output as a \code{data.frame}. If \code{FALSE}, a list is returned with at least two items: \code{"coords"}, a vector containing the longitude and latitude coordinates, and \code{"bbox"}, the corresponding bounding box.
#' @param server OpenStreetMap Nominatim server name. Could also be a local OSM Nominatim server.
#' @return See \code{as.data.frame}
#' @export
#' @importFrom XML xmlChildren xmlRoot xmlAttrs xmlTreeParse
#' @example ../examples/geocode_OSM.R
#' @seealso \code{\link{bb}}
geocode_OSM <- function(q, projection="longlat", return.first.only=TRUE, details=FALSE, as.data.frame=details, server="http://nominatim.openstreetmap.org") {
	q <- gsub(" ", "+", enc2utf8(q), fixed = TRUE)
	addr <- paste0(server, "/search?q=", q, "&format=xml&polygon=0&addressdetails=0")
	
	tmpfile <- tempfile()
	suppressWarnings(download.file(addr, destfile = tmpfile, mode= "wb", quiet = TRUE))
	
	doc <- xmlTreeParse(tmpfile, encoding="UTF-8")
	unlink(tmpfile)
	
	res <- xmlChildren(xmlRoot(doc)) 
	
	if (length(res)==0) stop(paste("No results found for \"", q, "\".", sep=""))
	
	idx <- if (return.first.only) 1 else 1:length(res)
	
	output <- lapply(idx, function(i) {
		search_result <- xmlAttrs(res[[i]])
		bbx <- search_result["boundingbox"]
		coords <- c(lon=as.numeric(search_result["lon"]),
					lat=as.numeric(search_result["lat"]))
		
		b <- matrix(as.numeric(unlist(strsplit(bbx, ","))), ncol=2, byrow=TRUE)[2:1,]
		dimnames(b) <- list(c("x", "y"), c("min", "max"))
		
		if (projection!="longlat") {
			current.projection <- get_proj4("longlat")
			prj <- get_proj4(projection)
			single_point <- SpatialPoints(matrix(coords, nrow=1), proj4string=CRS(current.projection))
			coords <- as.vector(attr(set_projection(single_point, projection = prj), "coords"))
			names(coords) <- c("x", "y")
			b <- bb(b, current.projection = current.projection, projection = prj)
		}
		
		if (details) {
			place_id <- search_result["place_id"]
			osm_type <- search_result["osm_type"]
			osm_id <- search_result["osm_id"]
			place_rank <- search_result["place_rank"]
			display_name <- search_result["display_name"]
			osm_class <- search_result["class"]
			type <- search_result["type"]
			importance <- search_result["importance"]
			list(coords=coords, bbox=b,
				 place_id=as.numeric(unname(place_id)),
				 osm_type=unname(osm_type),
				 osm_id=as.numeric(unname(osm_id)),
				 place_rank=unname(place_rank),
				 display_name=unname(display_name),
				 class=unname(osm_class),
				 type=unname(type),
				 importance=as.numeric(unname(importance)))
		} else {
			list(coords=coords, bbox=b)
		}
	})
	
	if (as.data.frame) {
		output <- lapply(output, function(op) {
			lst <- list(lon= unname(op$coords[1]),
						lat = unname(op$coords[2]),
						xmin = unname(op$bbox[1]),
						xmax = unname(op$bbox[2]),
						ymin = unname(op$bbox[3]),
						ymax = unname(op$bbox[4]))
			
			op$coords <- NULL
			op$bbox <- NULL
			as.data.frame(c(lst, op), stringsAsFactors=FALSE)
		}) 
		do.call(rbind, output)
	} else {
		if (length(output)==1) output[[1]] else output
	}
}


#' Reverse geocodes a location using OpenStreetMap Nominatim
#' 
#' Reverse geocodes a location (based on spatial coordinates) to an address. It uses OpenStreetMap Nominatim.
#' 
#' @param x x coordinate(s)
#' @param y y coordinate(s)
#' @param zoom zoom level
#' @param projection projection in which the coordinates \code{x} and \code{y} are provided
#' @param as.data.frame return as data.frame (\code{TRUE}) or list (\code{FALSE}). By default a list, unless multiple coordinates are provided.
#' @param server OpenStreetMap Nominatim server name. Could also be a local OSM Nominatim server.
#' @export
#' @importFrom XML xmlChildren xmlRoot xmlAttrs xmlTreeParse
#' @return A data frmame with all atributes that are contained in the search result
#' @example ../examples/geocode_OSM.R
#' @seealso \code{\link{bb}}
rev_geocode_OSM <- function(x, y, zoom=NULL, projection="longlat", as.data.frame=NA, server="http://nominatim.openstreetmap.org") {
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
	
	if (missing(zoom)) {
		qzoom <- ""
		strzoom <- ""
	} else {
		qzoom <- paste0("&zoom=", zoom)
		strzoom <- paste0(", zoom = ", zoom)
	}
	
	addr <- paste0(server, "/reverse?format=xml&lat=", lat, "&lon=", lon, qzoom, "&addressdetails=1")
	

	dfs <- lapply(1:n, function(i) {
		tmpfile <- tempfile()
		suppressWarnings(download.file(addr[i], destfile = tmpfile, mode= "wb", quiet = TRUE))
		doc <- xmlTreeParse(tmpfile, encoding="UTF-8")
		unlink(tmpfile)
		
		res <- xmlChildren(xmlRoot(doc)) 
		
		result_name <- xmlValue(res[[1]])
		search_result <- xmlAttrs(res[[1]])
		addr_result <- xmlChildren(res[[2]])
		
		dfnames <- names(addr_result)
		dfvalues <- lapply(1:length(addr_result), function(j) xmlValue(addr_result[[j]]))
		
		names(dfvalues) <- dfnames

		#addrdf <- as.data.frame(dfvalues, stringsAsFactors = FALSE)
		
		lat <- as.numeric(search_result["lat"])
		lon <- as.numeric(search_result["lon"])
		bbx <- search_result["boundingbox"]
		b <- as.numeric(unlist(strsplit(bbx, ",")))
		xmin <- b[3]
		xmax <- b[4]
		ymin <- b[1]
		ymax <- b[2]
		
		place_id <- search_result["place_id"]
		osm_type <- search_result["osm_type"]
		osm_id <- search_result["osm_id"]
		ref <- search_result["ref"]
		c(list(x=x[i],
			 y=y[i],
			 name=result_name,
			 lat=lat,
			 lon=lon,
			 xmin=xmin,
			 xmax=xmax,
			 ymin=ymin,
			 ymax=ymax,
			 place_id=as.numeric(unname(place_id)),
			 osm_type=unname(osm_type),
			 osm_id=as.numeric(unname(osm_id)),
			 ref=unname(ref)),
		  dfvalues)
	})
	
	if (as.data.frame) {
		addrnames <- sort(unique(unlist(lapply(dfs, function(df) {
			names(df)[14:length(df)]
		}))))
		addrlist <- lapply(addrnames, function(a)NA)
		names(addrlist) <- addrnames
		init <- c(list(x=NA,
					   y=NA,
					   name=NA,
					   lat=NA,
					   lon=NA,
					   xmin=NA,
					   xmax=NA,
					   ymin=NA,
					   ymax=NA,
					   place_id=NA,
					   osm_type=NA,
					   osm_id=NA,
					   ref=NA), addrlist)
		
		
		do.call(rbind, c(lapply(dfs, function(df) {
			init[names(df)]	<- df
			as.data.frame(init, stringsAsFactors=FALSE)
		}), list(stringsAsFactors=FALSE)))
	} else {
		dfs
	}
	
	
}

