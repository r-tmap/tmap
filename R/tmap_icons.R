#' Specify icons
#'
#' Specifies icons from a png images, which can be used as markers in thematic maps.
#' The function `marker_icon()` is the specification of the default marker.
#'
#' @param file character value/vector containing the file path(s) or url(s).
#' @param names names to be given to the icons. Useful when icons are assigned to factor levels.
#' @param width width of the icon. If `keep.asp`, this is interpreted as the maximum width.
#' @param height height of the icon. If `keep.asp`, this is interpreted as the maximum height.
#' @param keep.asp keep the aspect ratio of the png image. If `TRUE` and the aspect
#'   ratio differs from `width/height`, either `width` or `height` is adjusted accordingly.
#' @param just justification of the icons relative to the point coordinates.
#'   The first value specifies horizontal and the second value vertical justification.
#'   Possible values are: `"left"` , `"right"`, `"center"`, `"bottom"`, and `"top"`.
#'   Numeric values of 0 specify left alignment and 1 right alignment.
#'   The default value of `just` is `c("center", "center")`.
#' @param merge merge icons to one icon list (see return value)? If `FALSE`, a list is created per file. By default `TRUE`, unless `names` are specified.
#' @param as.local if the `file` is a url, should it be saved to local temporary file?
#' @param ... arguments passed on to [leaflet::icons()].
#'   When `iconWidth`, `iconHeight`, `iconAnchorX`, and `iconAnchorY` are specified,
#'   they override `width` and `height`, and `just`.
#' @return icon data (see [leaflet::icons()])
#' @export
#' @seealso [tm_symbols()]
tmap_icons <- function(file, names = NULL, width=48, height=48, keep.asp=TRUE, just=c("center", "center"), merge = NA, as.local=TRUE, ...) {
	if (is.na(merge)) merge = is.null(names)
	icon_names <- names(file)
	icons <- lapply(file, tmap_one_icon, width=width, height=height, keep.asp=keep.asp, just=just, as.local=as.local, ...)
	if (merge) {
		merge_icons(icons, icon_names)
	} else {
		if (!is.null(names)) {
			if (length(names) != length(icons)) stop("Icons and names have different lengths")
			names(icons) = names
		}
		icons
	}

}

file_extension <- function(filenames) {
	sub(pattern = "^(.*\\.|[^.]+)(?=[^.]*)", replacement = "", filenames, perl = TRUE)
}

tmap_one_icon <- function(file, width, height, keep.asp, just, as.local, ...) {
	args <- list(...)
	args$iconUrl <- NULL # already specified with file
	pu <- is_path_or_url(file)
	if (is.na(pu)) {
		stop(file, " is neither a valid path nor url", call.=FALSE)
	}
	if (!pu) {
		tmpfile <- file.path(tempdir(), basename(file))
		if (!file.exists(tmpfile)) download.file(file, destfile=tmpfile, mode="wb")
		localfile <- tmpfile
	} else {
		localfile <- file
	}

	if (!pu && as.local) file <- localfile

	# use exact dimensions when provided
	if (any(c("iconWidth", "iconHeight") %in% names(args))) keep.asp <- FALSE

	# adjust to png dimensions
	if (keep.asp) {
		ext = file_extension(localfile)

		if (ext %in% c("png", "jpg", "bmp", "jpeg", "tiff")) {
			x = stars::read_stars(localfile)
			dms = unname(dim(x))
			dms[1:2] = dms[2:1]
		} else {
			rlang::check_installed("rsvg")
			bitmap <- rsvg::rsvg(localfile, width = width)
			dim(bitmap) # h*w*c
			dms = dim(bitmap)
		}

		xasp <- dms[2]/dms[1]
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
	structure(icons(iconUrl = file, iconWidth=25, iconHeight=41, iconAnchorX = 12, iconAnchorY = 41), class = "tmap_icons")
}


# fix.borders is needed to prevent that right-hand side and bottom edges disappear
pngGrob <- function(file, fix.borders=FALSE, n=NULL, height.inch=NULL, target.dpi=NULL) {
	rlang::check_installed("png", reason = "for this function to work.")
	pu <- is_path_or_url(file)
	if (is.na(pu)) {
		stop(file, " is neither a valid path nor url", call.=FALSE)
	}

	if (!pu) {
		tmpfile <- file.path(tempdir(), basename(file))
		if (!file.exists(tmpfile)) download.file(file, destfile=tmpfile, mode="wb")
		file <- tmpfile
	}

	ext = file_extension(file)
	if (ext %in% c("png", "jpg", "bmp", "jpeg", "tiff")){
		x <- aperm(stars::read_stars(file)[[1]], c(2, 1, 3))
		if (max(x) > 1) {
			x[] = x[] / max(x)
		}
	} else {
		rlang::check_installed("rsvg")
		x <- rsvg::rsvg(file, height = height.inch * target.dpi)
	}

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
	structure(res, class = "tmap_icons")
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
