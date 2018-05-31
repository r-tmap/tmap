get_RasterLayer_data_vector <- function(r) {
	values <- r@data@values
	if (r@data@isfactor) {
		dt <- r@data@attributes[[1]]
		levelsID <- ncol(dt)
		factor(values, levels=dt$ID, labels=dt[[levelsID]])
	} else {
		values
	}
}



get_RasterLayer_levels <- function(r) {
	if (r@data@isfactor) {
		dt <- r@data@attributes[[1]]
		levelsID <- ncol(dt)
		as.character(dt[[levelsID]])
	} else {
		NULL
	}
}

raster_colors <- function(x) {
	n <- nrow(x)
	
	# get alpha transparency
	if (ncol(x)==4) {
		a <- x[,4]
		x <- x[,1:3]
	} else {
		a <- NULL
	}
	
	storage.mode(x) <- "integer"
	v <- x[, 1] * 1e6L + x[, 2] * 1e3L + x[, 3]
	
	isna <- is.na(v)
	if (!is.null(a)) isna <- isna & (a==255)
	
	v <- v[!isna]
	u <- unique(v)
	nu <- length(u)
	m <- match(v, u)
	nc <- (min(256, nu))
	ta <- tabulate(m, nbins = nu)
	mo <- order(ta, decreasing = TRUE)
	ids <- mo[1:nc]
	
	r <- floor(u/1e6L)
	g <- floor((u-r*1e6L)/1e3L)
	b <- (u - r * 1e6L - g * 1e3L)
	rs <- r[ids]
	gs <- g[ids]
	bs <- b[ids]
	
	RGB <- cbind(r, g, b)
	RGBs <- cbind(rs, gs, bs)
	
	
	dists <- apply(RGBs, MARGIN = 1, function(rw) {
		sqrt((rw[1]-RGB[,1])^2 + (rw[2]-RGB[,2])^2 + (rw[3]-RGB[,3])^2)
	})
	
	ids2 <- apply(dists, MARGIN = 1, which.min)
	
	m2 <- ids2[m]
	
	ind <- integer(length=n)
	
	
	ind[!isna] <- m2
	ind[isna] <- NA
	
	cols <- rgb(rs, gs, bs, maxColorValue = 255)
	
	factor(ind, labels=cols)
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
		atb <- atb[vapply(atb, length, integer(1))!=0]

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
	isf <- !vapply(lvls, is.null, logical(1))
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
			atb <- atb[vapply(atb, length, integer(1))!=0]
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
	
	notNumCat <- vapply(data, function(x){
		!is.numeric(x) && !is.factor(x)
	}, logical(1))
	if (any(notNumCat)) {
		data[, notNumCat] <- lapply(data[, notNumCat, drop=FALSE], function(x) {
			if (is.logical(x)) factor(x, levels=c(FALSE, TRUE)) else factor(x)
		})
	}
	data
}
