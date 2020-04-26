#' Specify icons
#' 
#' Specifies icons from a png images, which can be used as markers in thematic maps. The function \code{marker_icon} is the specification of the default marker.
#' 
#' @param file character value/vector containing the file path(s) or url(s).
#' @param width width of the icon. If \code{keep.asp}, this is interpreted as the maximum width.
#' @param height height of the icon. If \code{keep.asp}, this is interpreted as the maximum height.
#' @param keep.asp keep the aspect ratio of the png image. If \code{TRUE} and the aspect ratio differs from \code{width/height} either \code{width} or \code{height} is adjusted accordingly.
#' @param just justification of the icons relative to the point coordinates.  The first value specifies horizontal and the second value vertical justification. Possible values are: \code{"left"} , \code{"right"}, \code{"center"}, \code{"bottom"}, and \code{"top"}. Numeric values of 0 specify left alignment and 1 right alignment. The default value of \code{just} is \code{c("center", "center")}.
#' @param as.local if the \code{file} is a url, should it be saved to local temporary file?
#' @param ... arguments passed on to \code{\link[leaflet:icons]{icons}}. When \code{iconWidth}, \code{iconHeight}, \code{iconAnchorX} and \code{iconAnchorY} are specified, they override \code{width} and \code{height}, and \code{just}.
#' @return icon data (see \code{\link[leaflet:icons]{icons}})
#' @export
#' @seealso \code{\link{tm_symbols}}
#' @name tmap_icons
#' @rdname tmap_icons
tmap_icons <- function(file, width=48, height=48, keep.asp=TRUE, just=c("center", "center"), as.local=TRUE, ...) {
	icon_names <- names(file)
	icons <- lapply(file, tmap_one_icon, width=width, height=height, keep.asp=keep.asp, just=just, as.local=as.local, ...)
	merge_icons(icons, icon_names)
}


tmap_one_icon <- function(file, width, height, keep.asp, just, as.local, ...) {
	args <- list(...)
	args$iconUrl <- NULL # already specified with file
	pu <- is_path_or_url(file)
	if (is.na(pu)) {
		stop(file, " is neither a valid path nor url", call.=FALSE)
	}
	if (!pu) {
		tmpfile <- tempfile(fileext=".png")
		download.file(file, destfile=tmpfile, mode="wb")
		localfile <- tmpfile
	} else {
		localfile <- file
	}
	
	if (!pu && as.local) file <- localfile
	
	# use exact dimensions when provided
	if (any(c("iconWidth", "iconHeight") %in% names(args))) keep.asp <- FALSE
	
	# adjust to png dimensions
	if (keep.asp) {
		x <- png::readPNG(localfile)
		xasp <- dim(x)[2]/dim(x)[1]
		iasp <- width/height
		if (xasp > iasp) {
			height <- floor(width/xasp)
		} else {
			width <- floor(height*xasp)
		}
	}
	
	# override dimensions
	if (!("iconWidth" %in% names(args))) args$iconWidth <- width
	if (!("iconHeight" %in% names(args))) args$iconHeight <- height
	
	just <- c(ifelse(is_num_string(just[1]), as.numeric(just[1]), ifelse(just[1]=="left", 1, ifelse(just[1]=="right", 0, .5))),
			  ifelse(is_num_string(just[2]), as.numeric(just[2]), ifelse(just[2]=="bottom", 1, ifelse(just[2]=="top", 0, .5))))
	
	# override anchor data
	if (!("iconAnchorX" %in% names(args))) args$iconAnchorX <- round(args$iconWidth * (1-just[1]))
	if (!("iconAnchorY" %in% names(args))) args$iconAnchorY <- round(args$iconHeight * just[2])
	
	do.call(leaflet::icons, c(list(iconUrl=file), args))
}



#' @rdname tmap_icons
#' @export
marker_icon <- function() {
	file <- system.file("htmlwidgets/lib/leaflet/images/marker-icon.png", package="leaflet")
	if (!file.exists(file)) stop("leaflet marker icon not found")
	icons(iconUrl = system.file("htmlwidgets/lib/leaflet/images/marker-icon.png", package="leaflet"), iconWidth=25, iconHeight=41, iconAnchorX = 12, iconAnchorY = 41)
}


# fix.borders is needed to prevent that right-hand side and bottom edges disappear
pngGrob <- function(file, fix.borders=FALSE, n=NULL, height.inch=NULL, target.dpi=NULL) {
	if (!requireNamespace("png", quietly = TRUE)) {
		stop("png package needed for this function to work. Please install it.",
			 call. = FALSE)
	} else {
		pu <- is_path_or_url(file)
		if (is.na(pu)) {
			stop(file, " is neither a valid path nor url", call.=FALSE)
		}
		
		if (!pu) {
			tmpfile <- tempfile(fileext=".png")
			download.file(file, destfile=tmpfile, mode="wb")
			file <- tmpfile
		}
		
		x <- png::readPNG(file)
		
		if (fix.borders) {
			if (dim(x)[3]==3) {
				x <- array(c(x, rep(1, dim(x)[1]*dim(x)[2])), dim = c(dim(x)[1], dim(x)[2], dim(x)[3]+1))
			}
			x2 <- add_zero_borders_to_3d_array(x, n=n, height.inch=height.inch,target.dpi=target.dpi)
			rasterGrob(x2, interpolate=TRUE)
		} else {
			rasterGrob(x, interpolate=TRUE)
		}
	}
}

add_zero_borders_to_3d_array <- function(x, perc=NA, n=NULL, height.inch=NULL, target.dpi=NULL) {
	dims <- dim(x)
	
	if (is.na(perc)) {
		dpi <- dims[2] / height.inch
		compress <- dpi/target.dpi
		borders <- rep(compress * n, 2)
	} else {
		borders <- round(dims / 100 * perc)
	}

	res <- lapply(1:dims[3], function(i) {
		rbind(cbind(x[,,i], matrix(0, nrow=nrow(x), ncol=borders[2])), matrix(0, nrow=borders[1], ncol=ncol(x)+borders[2]))
	})
	array(unlist(res, use.names = FALSE), dim = c(dim(x)[1]+borders[1], dim(x)[2]+borders[2], dim(x)[3]))
}



icon2grob <- function(icon) {
	if (!is.list(icon)) stop("icon is not a list")
	if (!"iconUrl" %in% names(icon)) stop("iconUrl not defined")
	if (length(icon$iconUrl)==1) {
		pngGrob(icon$iconUrl)
	} else {
		lapply(icon$iconUrl, pngGrob)
	}
}

grob2icon <- function(grob, grob.dim, just) {
	tmp <- tempfile(fileext=".png")
	png(filename=tmp, width=grob.dim[3], height=grob.dim[4], bg = "transparent")
	grid.draw(grob)
	dev.off()
	w <- grob.dim[1]
	h <- grob.dim[2]
	icons(iconUrl = tmp, iconWidth = w, iconHeight = h, iconAnchorX = w * (1-just[1]), iconAnchorY = h * just[2])
}

split_icon <- function(icon) {
	ni <- max(vapply(icon, length, integer(1)))
	icon_max <- lapply(icon, function(ic) {
		rep(ic, length.out=ni)
	})

	if ("iconNames" %in% names(icon_max)) {
		icon_names <- icon_max$iconNames
		icon_max$iconNames <- NULL
	} else {
		icon_names <- NULL
	}
	
	res <- lapply(1:ni, function(i) {
		lapply(icon_max, function(ic) {
			ic[i]	
		})
	})
	
	if (!is.null(icon_names)) names(res) <- icon_names
	
	res
}

merge_icons <- function(icons, icon_names = NULL) {
	list_names <- unique(unlist(lapply(icons, names), use.names = FALSE))
	names(list_names) <- list_names
	
	res <- lapply(list_names, function(ln) {
		unname(sapply(icons, function(ic) {
			if (ln %in% names(ic)) {
				ic[[ln]][1]
			} else NA
		}))
	})
	if (!is.null(icon_names)) res$iconNames <- icon_names
	res
}


is_path_or_url <- function(file) {
	if (file.exists(file)) {
		TRUE
	} else {
		con.url <- suppressWarnings(try({
			u <- url(file, open='rb')
			close(u)
		}, silent=TRUE))
		try.error <- inherits(con.url, "try-error")
		if (try.error) NA else FALSE
	}
}
