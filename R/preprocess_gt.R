maybe_longlat <- function (bb) (bb[1] >= -180.1 && bb[3] <= 180.1 && bb[2] >= -90.1 && bb[4] <= 90.1)

preprocess_gt <- function(x, interactive, orig_crs) {
	set.bounds <- bg.color <- set.zoom.limits <- legend.position <- colorNA <- NULL
	
	
	gt <- get("tmapOptions", envir = .TMAP_CACHE)
	
	gts <- x[names(x) == "tm_layout"]
	if (length(gts)) {
		for (i in 1L:length(gts)) {
			g <- gts[[i]]
			
			called <- if (is.null(attr(g, "format_args"))) {
				names(g)
			} else {
				attr(g, "format_args")
			}
			
			
			if (("legend.position" %in% called) && interactive) {
				if (gt$show.messages) message("legend.postion is used for plot mode. Use view.legend.position in tm_view to set the legend position in view mode.")
			}
			
			if (!is.na(g$style)) {
				if (i !=1 && gt$show.messages) message("Note that tm_style(\"", g$style, "\") resets all options set with tm_layout, tm_view, tm_format, or tm_legend. It is therefore recommended to place the tm_style element prior to the other tm_layout/tm_view/tm_format/tm_legend elements.")
				gt <- .defaultTmapOptions
				if (g$style != "white") {
					styleOptions <- get("tmapStyles", envir = .TMAP_CACHE)[[g$style]]
					gt[names(styleOptions)] <- styleOptions
				}
			} 
			g$style <- NULL
			if ("aes.color" %in% names(g)) {
				aes <- g$aes.color
				if (!all(names(aes) %in% names(gt$aes.color))) stop("Names in aes.color unknown: ", paste(setdiff(names(aes), names(gt$aes.color)), collapse = ", "), call. = FALSE)
				g$aes.color <- gt$aes.color
				g$aes.color[names(aes)] <- aes
			}
			if ("aes.palette" %in% names(g)) {
				aes <- g$aes.palette
				if (!all(names(aes) %in% names(gt$aes.palette))) stop("Names in aes.color unknown: ", paste(setdiff(names(aes), names(gt$aes.palette)), collapse = ", "), call. = FALSE)
				g$aes.palette <- gt$aes.palette
				g$aes.palette[names(aes)] <- aes
			}
			if (("legend.format" %in% names(g))) {
				lf <- g$legend.format
				
				extraArgs <- setdiff(names(lf), names(gt$legend.format))
				
				if (length(extraArgs) > 1) {
					lf_base <- lf[intersect(names(lf), names(gt$legend.format))]
					lf_extra <- lf[extraArgs]
				} else {
					lf_base <- lf
					lf_extra <- list()
				}
				
				#if (!all(names(lf) %in% names(gt$legend.format))) stop("Names in legend.format unknown: ", paste(setdiff(names(lf), names(gt$legend.format)), collapse = ", "), call. = FALSE)
				g$legend.format <- gt$legend.format
				g$legend.format[names(lf_base)] <- lf_base
				
				if (length(extraArgs) > 1) {
					g$legend.format <- c(g$legend.format, lf_extra) 
				}
			}
			
			if (length(g)) gt[names(g)] <- g
		}
	}

	## preprocess gt
	gt <- within(gt, {
		pc <- list(sepia.intensity=sepia.intensity, saturation=saturation)
		sepia.intensity <- NULL
		saturation <- NULL
		
		# put aes colors in right order and name them
		if (length(aes.color)==1 && is.null(names(aes.color))) names(aes.color) <- "base"
		
		if (!is.vector(aes.color) || !is.character(aes.color) || length(aes.color) != 8 || !setequal(names(aes.color), c("fill", "borders", "symbols", "dots", "lines", "text", "na", "null"))) {
			stop("aes.color should the be a character vector of 8 colors named \"fill\", \"borders\", \"symbols\", \"dots\", \"lines\", \"text\", \"na\", \"null\"", call. = FALSE)
		}
		aes.colors <- vapply(aes.color, function(ac) if (is.na(ac)) "#000000" else ac, character(1))
		
		# override na
		if (interactive) aes.colors["na"] <- if (is.null(colorNA)) "#00000000" else if (is.na(colorNA)) aes.colors["na"] else colorNA
		
		aes.colors.light <- vapply(aes.colors, is_light, logical(1))
		aes.color <- NULL
		
		if (is.na(alpha)) alpha <- 1
		
		if (!is.logical(set.bounds)) if (!length(set.bounds)==4 || !is.numeric(set.bounds)) stop("Incorrect set_bounds argument", call.=FALSE)
		
		if (!is.null(bbox)) {
			if (is.character(bbox)) {
				res <- geocode_OSM(bbox)
				bbox <- res$bbox
				center <- res$coords
				res <- NULL
			} else {
				bbox <- bb(bbox)
				if (is.na(attr(bbox, "crs"))) {
					if (!maybe_longlat(bbox)) stop("bounding box specified with tm_view (or tmap_options) is projected, but the projection is unknown", call. = FALSE)
				} else {
					bbox <- bb(bbox, projection = .crs_longlat)
				}
				center <- NULL
			}
			set.view <- NA
		}

		if (!is.na(set.view[1])) {
			if (!is.numeric(set.view)) stop("set.view is not numeric")
			if (!length(set.view) %in% c(1,3)) stop("set.view does not have length 1 or 3")
		}
		if (!is.na(set.zoom.limits[1])) {
			if (!is.numeric(set.zoom.limits)) stop("set.zoom.limits is not numeric")
			if (!length(set.zoom.limits)==2) stop("set.zoom.limits does not have length 2")
			if (set.zoom.limits[1]<0 || set.zoom.limits[1] >= set.zoom.limits[2]) stop("incorrect set.zoom.limits")
		}
		if (!is.na(set.view[1]) && !is.na(set.zoom.limits[1])) {
			if (set.view[length(set.view)] < set.zoom.limits[1]) {
				warning("default zoom smaller than minimum zoom, now it is set to the minimum zoom")
				set.view[length(set.view)] <- set.zoom.limits[1]
			}
			if (set.view[length(set.view)] > set.zoom.limits[2]) {
				warning("default zoom larger than maximum zoom, now it is set to the maximum zoom")
				set.view[length(set.view)] <- set.zoom.limits[2]
			}
		}
		view.legend.position <- if (is.na(view.legend.position)[1]) {
			if (is.null(legend.position)) {
				"topright"
			} else if (is.character(legend.position) && 
					   tolower(legend.position[1]) %in% c("left", "right") &&
					   tolower(legend.position[2]) %in% c("top", "bottom")) {
				paste(tolower(legend.position[c(2,1)]), collapse="")
			}
		} else if (is.character(view.legend.position) && 
				   view.legend.position[1] %in% c("left", "right") &&
				   view.legend.position[2] %in% c("top", "bottom")) {
			paste(view.legend.position[c(2,1)], collapse="")
		} else {
			"topright"
		}
		
		if (!inherits(projection, "leaflet_crs")) {
			
			if (projection==0) {
				epsg <- get_epsg_number(orig_crs)
				if (is.na(epsg)) {
					projection <- 3857
				} else {
					projection <- epsg
				}
			}
			
			if (projection %in% c(3857, 4326, 3395)) {
				projection <- leaflet::leafletCRS(crsClass = paste("L.CRS.EPSG", projection, sep=""))	
			} else {
				projection <- leaflet::leafletCRS(crsClass = "L.Proj.CRS", 
												  code= paste("EPSG", projection, sep=":"),
												  proj4def=sf::st_crs(projection)$proj4string,
												  resolutions = c(65536, 32768, 16384, 8192, 4096, 2048,1024, 512, 256, 128))	
			}
			
			
		}

				
	})
	

	# append view to layout
	# gt[c("basemaps", "basemaps.alpha")] <- NULL
	# gv[c("colorNA", "call", "legend.position")] <- NULL
	# gt <- c(gt, gv)
	
	gtnull <- names(which(vapply(gt, is.null, logical(1))))
	gt[gtnull] <- list(NULL)
	gt
}
