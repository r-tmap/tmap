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



get_RasterLayer_levels <- function(r) {
	if (r@data@isfactor) {
		dt <- r@data@attributes[[1]]
		if ("levels" %in% names(dt)) {
			dt$levels
		} else {
			NULL
		}
	} else {
		NULL
	}
}

raster_colors <- function(x) {
	if (inherits(x, "Raster")) {
		x <- as(x, "SpatialGridDataFrame")
	}
	if (!all(vapply(x@data, is.integer, FUN.VALUE = logical(1)))) {
		x@data <- as.data.frame(lapply(x@data, as.integer))
	}

	# get alpha transparency
	if (ncol(x@data)==4) {
		a <- x@data[,4]
		x@data <- x@data[,1:3]
	} else {
		a <- NULL
	}

	y <- SGDF2PCT(x, adjust.bands = FALSE)
	if (!is.null(a)) {
		y$idx[a!=255] <- NA
	}
	data <- data.frame(PIXEL__COLOR = as.integer(y$idx))
	levels(data$PIXEL__COLOR) <- as.vector(na.omit(y$ct))
	class(data$PIXEL__COLOR) <- "factor"
	data
}


get_raster_names <- function(shp) {
	nms <- names(shp)

	# overwrite unknown first names with FILE__VALUES
	if (inherits(shp, "RasterStack")) {
		if (shp@layers[[1]]@data@names[1]=="") nms[1] <- "FILE__VALUES"
	} else {
		if (shp@data@names[1]=="") nms[1] <- "FILE__VALUES"
	}
	nms
}


get_raster_data <- function(shp) {
	if (fromDisk(shp)) {
		data <- raster::as.data.frame(shp)
	} else if (inherits(shp, "RasterLayer")) {
		data <- data.frame(get_RasterLayer_data_vector(shp))
		names(data) <- get_raster_names(shp)
	} else if (inherits(shp, "RasterStack")) {
		data <- as.data.frame(lapply(shp@layers, get_RasterLayer_data_vector))
		names(data) <- get_raster_names(shp)
	} else if (inherits(shp, "RasterBrick")) {
		isfactor <- shp@data@isfactor
		data <- as.data.frame(shp@data@values)
		if (is.null(dimnames(shp@data@values)))	names(data) <- get_raster_names(shp)

		atb <- shp@data@attributes
		atb <- atb[sapply(atb, length)!=0]

		stopifnot(sum(isfactor)==length(atb))

		if (any(isfactor)) data[isfactor] <- mapply(function(d, a){
			if (class(a)=="list") a <- a[[1]]
			levelsID <- ncol(a) # levels is always the last column of the attributes data.frame (?)
			factor(d, levels=a$ID, labels=as.character(a[[levelsID]]))
		}, data[isfactor], atb, SIMPLIFY=FALSE)
	}

	data
}

set_raster_levels <- function(shp, lvls) {
	isf <- !sapply(lvls, is.null)
	cls <- class(shp)
	if (any(isf)) {
		shp@data@isfactor <- isf
		dfs <- mapply(function(nm, lv) {
			df <- data.frame(ID=1:length(lv), levels=factor(lv, levels=lv))
			if (cls=="RasterBrick") names(df)[2] <- nm
			df
		}, names(which(isf)), lvls[isf], SIMPLIFY=FALSE)
		shp@data@attributes <- dfs
	}
	shp
}



get_raster_levels <- function(shp, layerIDs) {
	if (missing(layerIDs)) layerIDs <- 1L:nlayers(shp)

	if (inherits(shp, "Spatial")) {
		return(lapply(attr(shp, "data")[,layerIDs], levels))
	}

	shpnames <- get_raster_names(shp)[layerIDs]
	if (inherits(shp, "RasterLayer")) {
		lvls <- list(get_RasterLayer_levels(shp))
	} else if (inherits(shp, "RasterStack")) {
		lvls <- lapply(shp@layers[layerIDs], get_RasterLayer_levels)
	} else if (inherits(shp, "RasterBrick")) {
		isfactor <- shp@data@isfactor
		if (all(!isfactor)) {
			lvls <- lapply(shpnames, function(sn) NULL)
		} else {
			atb <- shp@data@attributes
			atb <- atb[sapply(atb, length)!=0]
			stopifnot(sum(isfactor)==length(atb))
			isfactor2 <- isfactor[layerIDs]

			lvls <- rep(list(NULL), length(layerIDs))
			if (any(isfactor2)) {
				atb2 <- atb[match(layerIDs[isfactor2], which(isfactor))]

				lvls[isfactor2] <- lapply(atb2, function(a) {
					if (class(a)=="list") a <- a[[1]]
					levelsID <- ncol(a) # levels is always the last column of the attributes data.frame (?)
					as.character(a[[levelsID]])
				})
			}
		}
	}
	names(lvls) <- shpnames
	lvls
}

get_data_frame_levels <- function(data) {
	lapply(data, function(x) {
		if (is.factor(x)) {
			levels(x)
		} else if (is.numeric(x)) {
			NULL
		} else {
			if (is.logical(x)) factor(x, levels=c(FALSE, TRUE)) else sort(unique(x))
		}
	})
}

preprocess_raster_data <- function(data, sel) {
	if (is.na(sel)[1] || !any(sel %in% names(data))) sel <- names(data)[1]
	sel <- intersect(sel, names(data))
	
	data <- data[, sel, drop=FALSE]
	
	notNumCat <- sapply(data, function(x){
		!is.numeric(x) && !is.factor(x)
	})
	if (any(notNumCat)) {
		data[, notNumCat] <- lapply(data[, notNumCat, drop=FALSE], function(x) {
			if (is.logical(x)) factor(x, levels=c(FALSE, TRUE)) else factor(x)
		})
	}
	data
}
