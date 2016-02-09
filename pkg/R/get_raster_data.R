get_RasterLayer_data_vector <- function(r) {
	values <- r@data@values
	if (r@data@isfactor) {
		dt <- r@data@attributes[[1]]
		if ("levels" %in% names(dt)) {
			factor(values, levels=dt$ID, labels=dt$levels)
		} else {
			warning("No 'levels' column found in data@attributes.", call. = FALSE)
			values
		}
	} else {
		values
	}
}


get_raster_data <- function(shp) {
	if (fromDisk(shp)) {
		data <- raster::as.data.frame(shp)
	} else if (inherits(shp, "RasterLayer")) {
		data <- data.frame(get_RasterLayer_data_vector(shp))
		names(data) <- names(shp)
		if (shp@data@names[1]=="" && ncol(data)>=1) names(data)[1] <- "FILE__VALUES"
	} else if (inherits(shp, "RasterStack")) {
		data <- as.data.frame(lapply(shp@layers, get_RasterLayer_data_vector))
		names(data) <- names(shp)
	} else if (inherits(shp, "RasterBrick")) {
		isfactor <- shp@data@isfactor
		data <- as.data.frame(shp@data@values)
		if (is.null(dimnames(shp@data@values))) names(data) <- shp@data@names
		
		atb <- shp@data@attributes
		atb <- atb[sapply(atb, length)!=0]
		
		stopifnot(sum(isfactor)==length(atb))
		
		if (any(isfactor)) data[isfactor] <- mapply(function(d, a){
			if (class(a)=="list") a <- a[[1]]
			levelsID <- ncol(a) # levels is always the last column of the attributes data.frame (?)
			factor(d, levels=a$ID, labels=as.character(a[[levelsID]]))
		}, data[isfactor], atb, SIMPLIFY=FALSE)
	}	
	
	ct <- length(colortable(shp))
	if (ct) {
		minV <- minValue(shp)
		plusone <- minV==0
		data[[1]] <- data[[1]]+plusone
	}

	data
}


preprocess_raster_data <- function(data, sel) {
	if (is.na(sel)[1] || !any(sel %in% names(data))) sel <- names(data)[1]
	sel <- intersect(sel, names(data))
	
	data <- data[, sel, drop=FALSE]
	
	notNumCat <- sapply(data, function(x){
		!is.numeric(x) && !is.factor(x)
	})
	
	if (any(notNumCat)) {
		data[, notNumCat] <- lapply(data[, notNumCat], function(x) {
			factor(x)
		})
	}
	data
}