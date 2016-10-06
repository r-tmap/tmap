#' Render a png image
#' 
#' Render a png image (from file path or url) to a grob, which can be used for proportional symbol maps
#' 
#' @param file character value containing the ile path or url
#' @return \code{\link[grid:rasterGrob]{rasterGrob}}
#' @export
#' @seealso \code{\link{tm_symbols}}
pngGrob <- function(file) {
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
		rasterGrob(x)
	}
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

grob2icon <- function(grob, icon.data, just) {
	tmp <- tempfile(fileext=".png")
	png(filename=tmp, width=icon.data$icon.render.width, height=icon.data$icon.render.height)
	grid.draw(grob)
	dev.off()
	w <- icon.data$icon.width
	h <- icon.data$icon.height
	icons(iconUrl = tmp, iconWidth = w, iconHeight = h, iconAnchorX = w * (1-just[1]), iconAnchorY = h * just[2])
}

split_icon <- function(icon) {
	ni <- max(sapply(icon, length))
	icon_max <- lapply(icon, function(ic) {
		rep(ic, length.out=ni)
	})
	lapply(1:ni, function(i) {
		lapply(icon_max, function(ic) {
			ic[i]	
		})
	})
}

merge_icons <- function(icons) {
	list_names <- unique(unlist(lapply(icons, names)))
	names(list_names) <- list_names
	
	lapply(list_names, function(ln) {
		unname(sapply(icons, function(ic) {
			if (ln %in% names(ic)) {
				ic[[ln]][1]
			} else NA
		}))
	})
	
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
