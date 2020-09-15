split_alpha_channel <- function(x, alpha) {
	if (is.null(x)) {
		list(col=NULL, opacity=0)
	} else {
		RGBA <- col2rgb(x, alpha = TRUE)
		col <- rgb(RGBA[1,], RGBA[2,], RGBA[3,], maxColorValue = 255)
		opacity <- unname(RGBA[4,]/255 * alpha)
		list(col=col, opacity=opacity)
	}
}
get_x_name <- function(type) {
	if (type=="fill") {
		"xfill"
	} else if (type=="symbol") {
		c("xsize", "xcol", "xshape")
	} else if (type=="raster") {
		"xraster"
	} else if (type=="line") {
		c("xline", "xlwd")
	} else if (type=="text") {
		c("xtext", "xtsize", "xtcol")
	}
}

get_aes_name <- function(type) {
	if (type=="fill") {
		"fill"
	} else if (type=="symbol") {
		c("symbol.size", "symbol.col", "symbol.shape")
	} else if (type=="raster") {
		"raster"
	} else if (type=="line") {
		c("line.col", "line.lwd")
	} else if (type=="text") {
		c("text", "text.size", "text.color")
	}
}

get_labels <- function(gpl, type) {
	var_names <- paste(type, "names", sep=".")
	gpl$data[[gpl[[var_names]]]]
}

get_popups <- function(gpl, type) {
	var_names <- paste(type, "names", sep=".")
	var_vars <- paste(type, "popup.vars", sep=".")
	var_format <- paste(type, "popup.format", sep=".")
	
	dt <- gpl$data
	
	if (is.na(gpl[[var_vars]][1])) {
		popups <- NULL
	} else {
		popups <- view_format_popups(dt[[gpl[[var_names]]]], gpl[[var_vars]], gpl[[var_format]], dt[, gpl[[var_vars]], drop=FALSE])
	}
	popups
}


working_internet <- function(url = "https://www.google.com") {
	# test the http capabilities of the current R build
	if (!capabilities(what = "http/ftp")) return(FALSE)
	
	# test connection by trying to read first line of url
	test <- try(suppressWarnings(readLines(url, n = 1)), silent = TRUE)
	# return FALSE if test inherits 'try-error' class
	!inherits(test, "try-error")
}

bbx_per_line <- function(bbx) {
	max_lines <- 60
	(bbx[4] - bbx[2]) / max_lines
}

units_per_line <- function(bbx) {
	max_lines <- 60
	
	# calculate top-center to bottom-center
	vdist <- suppressWarnings({tmaptools::approx_distances(bbx, projection = 4326, target = "m")$vdist})
	vdist/max_lines
}

lty2dashArray <- function(lty) {
	numlty <- switch(lty,
					 solid=0,
					 blank=0,
					 # These numbers taken from ?par
					 dashed=c(4, 4),
					 dotted=c(1, 3),
					 dotdash=c(1, 3, 4, 3),
					 longdash=c(7, 3),
					 twodash=c(2, 2, 6, 2),
					 # Otherwise we're a hex string
					 as.numeric(as.hexmode(strsplit(lty, "")[[1]])))
	paste(ifelse(numlty == 0,
				 "none",
				 numlty),
		  collapse=",")
}

get_epsg_number <- function(proj) {
	if (inherits(proj, "crs")) {
		if (!is.na(proj$epsg)) {
			return(proj$epsg)	
		} else {
			proj <- proj$proj4string
		}
	}
	if (inherits(proj, "CRS")) proj <- attr(proj, "projargs")
	
	pat <- "^.*\\=epsg ?: ?(\\S*)(.*)$"
	epsg <- as.numeric(sub(pat, "\\1", proj[grepl(pat, proj)]))
	if (length(epsg)==0) NA else epsg
}


submit_labels <- function(labels, cls, pane, group_name, e) {
	
	layerIds <- get("layerIds", envir = e)
	
	
	types <- attr(layerIds, "types")
	groups <- attr(layerIds, "groups")
	
	labels_all <- unlist(layerIds, use.names = FALSE)
	
	pos <- length(labels_all)
	
	labels_all <- make.names(c(labels_all, labels), unique = TRUE)
	
	labels <- labels_all[(pos + 1): length(labels_all)]	
	
	labelsList <- list(labels)
	names(labelsList) <- pane
	
	layerIds <- c(layerIds, labelsList)
	
	#layerIds[[cls]] <- labels_all
	
	attr(layerIds, "types") <- c(types, cls)
	attr(layerIds, "groups") <- c(types, group_name)
	
	assign("layerIds", layerIds, envir = e)
	labels
}

paneName <- function(x) {
	paste0("tmap", sprintf("%03d", x))
}

legendName <- function(x) {
	paste0("legend", sprintf("%03d", x))
}
