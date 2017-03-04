preprocess_gt <- function(x, interactive, orig_CRS) {
	set.bounds <- bg.color <- set.zoom.limits <- legend.position <- NULL
	
	style <- options("tmap.style")
	tln <- paste("tm_style", style,sep="_" )
	if (!exists(tln)) {
		warning("Style ", style, " unknown; ", tln, " does not exist. Please specify another style with the option \"tmap.stype\".", call. = FALSE)
		tln <- "tm_style_default"
	}

	# process tm_layout: merge multiple to one gt
	gt <- do.call(tln, args = list())$tm_layout
	gts <- x[names(x)=="tm_layout"]
	if (length(gts)) {
		gtsn <- length(gts)
		extraCall <- character(0)
		for (i in 1:gtsn) {
			gt[gts[[i]]$call] <- gts[[i]][gts[[i]]$call]
			if ("attr.color" %in% gts[[i]]$call) gt[c("earth.boundary.color", "legend.text.color", "title.color")] <- gts[[i]]["attr.color"]
			extraCall <- c(extraCall, gts[[i]]$call)
		}
		gt$call <- c(gt$call, extraCall)
	}

	# process tm_view: merge multiple to one gv
	if (any("tm_view" %in% names(x))) {
		vs <- which("tm_view" == names(x))
		gv <- x[[vs[1]]]
		if (length(vs)>1) {
			for (i in 2:length(vs)) {
				gv2 <- x[[vs[i]]]
				gv[gv2$call] <- gv2[gv2$call]
				gv$call <- unique(c(gv$call, gv2$call))
			}
		}
	} else {
		gv <- tm_view()$tm_view
	}
	
	## preprocess gt
	gt <- within(gt, {
		pc <- list(sepia.intensity=sepia.intensity, saturation=saturation)
		sepia.intensity <- NULL
		saturation <- NULL
		
		if (!"scientific" %in% names(legend.format)) legend.format$scientific <- FALSE
		if (!"digits" %in% names(legend.format)) legend.format$digits <- NA
		if (!"text.separator" %in% names(legend.format)) legend.format$text.separator <- "to"
		if (!"text.less.than" %in% names(legend.format)) legend.format$text.less.than <- "Less than"
		if (!"text.or.more" %in% names(legend.format)) legend.format$text.or.more <- "or more"
		
		# put aes colors in right order and name them
		if (length(aes.color)==1 && is.null(names(aes.color))) names(aes.color) <- "base"
		
		if (!is.null(names(aes.color))) {
			aes.colors <- c(fill="grey85", borders="grey40", symbols="blueviolet", dots="black", lines="red", text="black", na="grey60")
			aes.colors[names(aes.color)] <- aes.color
		} else {
			aes.colors <- rep(aes.color, length.out=7)
			names(aes.colors) <- c("fill", "borders", "symbols", "dots", "lines", "text", "na")
		}
		aes.colors <- sapply(aes.colors, function(ac) if (is.na(ac)) "#000000" else ac)
		
		# override na
		if (interactive) aes.colors["na"] <- if (is.null(gv$colorNA)) "#00000000" else if (is.na(gv$colorNA)) aes.colors["na"] else gv$colorNA
		
		aes.colors.light <- sapply(aes.colors, is_light)
		aes.color <- NULL
		
		
		
	})
	
	# process view
	gv <- within(gv, {
		if (!get(".internet", envir = .TMAP_CACHE) || identical(basemaps, FALSE)) {
			basemaps <- character(0)
		} else {
			# with basemap tiles
			if (is.na(basemaps.alpha)) basemaps.alpha <- gt$basemaps.alpha
			if (identical(basemaps, NA)) basemaps <- gt$basemaps
			if (identical(basemaps, TRUE)) basemaps <- c("OpenStreetMap", "OpenStreetMap.Mapnik", "OpenTopoMap", "Stamen.Watercolor", "Esri.WorldTopoMap", "Esri.WorldImagery", "CartoDB.Positron", "CartoDB.DarkMatter")
			basemaps.alpha <- rep(basemaps.alpha, length.out=length(basemaps))
			if (is.na(alpha)) alpha <- .7
		}
		if (!is.logical(set.bounds)) if (!length(set.bounds)==4 || !is.numeric(set.bounds)) stop("Incorrect set_bounds argument", call.=FALSE)

		if (!is.na(set.view[1])) {
			if (!is.numeric(set.view)) stop("set.view is not numeric")
			if (!length(set.view)==3) stop("set.view does not have length 3")
		}
		if (!is.na(set.zoom.limits[1])) {
			if (!is.numeric(set.zoom.limits)) stop("set.zoom.limits is not numeric")
			if (!length(set.zoom.limits)==2) stop("set.zoom.limits does not have length 2")
			if (set.zoom.limits[1]<0 || set.zoom.limits[1] >= set.zoom.limits[2]) stop("incorrect set.zoom.limits")
		}
		if (!is.na(set.view[1]) && !is.na(set.zoom.limits[1])) {
			if (set.view[3] < set.zoom.limits[1]) {
				warning("default zoom smaller than minimum zoom, now it is set to the minimum zoom")
				set.view[3] <- set.zoom.limits[1]
			}
			if (set.view[3] > set.zoom.limits[2]) {
				warning("default zoom larger than maximum zoom, now it is set to the maximum zoom")
				set.view[3] <- set.zoom.limits[2]
			}
		}
		view.legend.position <- if (is.na(legend.position)[1]) {
			if (is.null(gt$legend.position)) {
				"topright"
			} else if (is.character(gt$legend.position) && 
					   tolower(gt$legend.position[1]) %in% c("left", "right") &&
					   tolower(gt$legend.position[2]) %in% c("top", "bottom")) {
				paste(tolower(gt$legend.position[c(2,1)]), collapse="")
			}
		} else if (is.character(legend.position) && 
				   legend.position[1] %in% c("left", "right") &&
				   legend.position[2] %in% c("top", "bottom")) {
			paste(legend.position[c(2,1)], collapse="")
		} else {
			"topright"
		}
		
		if (!inherits(projection, "leaflet_crs")) {
			
			if (projection==0) {
				epsg <- get_epsg_number(orig_CRS)
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
												  proj4def=get_proj4(projection),
												  resolutions = c(65536, 32768, 16384, 8192, 4096, 2048,1024, 512, 256, 128))	
			}
			
			
		}
		
	})
	
	# append view to layout
	gt[c("basemaps", "basemaps.alpha")] <- NULL
	gv[c("colorNA", "call", "legend.position")] <- NULL
	gt <- c(gt, gv)
	
	gtnull <- names(which(sapply(gt, is.null)))
	gt[gtnull] <- list(NULL)
	gt
}
