view_tmap <- function(gps, shps, bbx) {
	
	# determine view
	view <- get_view(bbx)
	
	# take first small multiple
	gp <- gps[[1]]
	gt <- gp$tm_layout
	gp$tm_layout <- NULL
	
	lf <- leaflet()
	basemaps <- gt$basemaps

	if (is.null(names(basemaps))) names(basemaps) <- sapply(basemaps, FUN = function(bm) {
		if (substr(bm, 1, 4) == "http") {
			x <- strsplit(bm, "/", fixed=TRUE)[[1]]
			x <- x[-c(1, (length(x)-2):length(x))]
			x <- x[x!=""]
			paste(x, collapse="/")
		} else bm
	})
	
	if (!is.na(gt$set.zoom.limits[1])) {
	  tileOptions <- tileOptions(minZoom=gt$set.zoom.limits[1], maxZoom=gt$set.zoom.limits[2])
	} else {
	  tileOptions <- tileOptions()
	}
	
	# add base layer(s)
	if (length(basemaps)) {
		for (i in 1:length(basemaps)) {
			bm <- unname(basemaps[i])
			bmname <- names(basemaps)[i]
			if (substr(bm, 1, 4) == "http") {
				lf <- lf %>% addTiles(bm, group=bmname, options=tileOptions)
			} else {
				lf <- lf %>% addProviderTiles(bm, group=bmname, options = tileOptions)
			}
		}
	}
	
	# add background overlay
	if (gt$bg.overlay.alpha!=0) {
		if (any(sapply(gp, function(gpl)!is.null(gpl$raster)))) {
			warning("Background overlays do not work yet with raster images. Background disabled.", call. = FALSE)
		} else {
			lf <- lf %>%  addRectangles(-540,-90,540,90, stroke=FALSE, fillColor=gt$bg.overlay, fillOpacity = gt$bg.overlay.alpha, layerId=0) 
			lf$x$limits <- NULL
		}
	}
	
		
	if (!length(gp)) {
		if (length(basemaps)>1) lf <- lf %>% addLayersControl(baseGroups=names(basemaps), options = layersControlOptions(autoZIndex = TRUE))
		
		if (!is.null(gt$bbx)) {
			lf <- lf %>% 
				fitBounds(gt$bbx[1], gt$bbx[2], gt$bbx[3], gt$bbx[4]) %>% 
				addMarkers(gt$center[1], gt$center[2])
		}
		lf <- set_bounds_view(lf, gt)
		return(lf)
	}
	
	e <- environment()
	id <- 1
	alpha <- gt$alpha

	group_selection <- mapply(function(shp, gpl, shp_name) {
		bbx <- attr(shp, "bbox")
		upl <- units_per_line(bbx)
		bpl <- bbx_per_line(bbx)
		if (inherits(shp, "Spatial")) {
			res <- get_sp_coordinates(shp, gpl, gt, bbx)
			co <- res$co
			if (gt$line.center.type[1]=="segment") {
				gpl <- res$gpl
				shp <- res$shp
			}	
		}
		
		plot_tm_fill <- function() {
			bres <- split_alpha_channel(gpl$col, alpha=alpha)
			bcol <- bres$col
			bopacity <- bres$opacity
			
			fres <- split_alpha_channel(gpl$fill, alpha=alpha)
			fcol <- fres$col
			fopacity <- fres$opacity
			
			if (!is.null(gpl$fill)) {
				popups <- get_popups(gpl, type="fill")
			} else {
				popups <- NULL
			}
			stroke <- gpl$lwd>0 && !is.na(bcol) && bopacity!=0
			
			lf <- lf %>% addPolygons(data=shp, stroke=stroke, weight=gpl$lwd, color=bcol, fillColor = fcol, fillOpacity = fopacity, popup = popups, options = pathOptions(clickable=!is.null(popups)), group=shp_name, layerId = id)
			
			
			if (!is.na(gpl$xfill)) {
				if (gpl$fill.legend.show) lf <- lf %>% add_legend(gpl, gt, aes="fill", alpha=alpha)
			}
			
			assign("lf", lf, envir = e)
			assign("id", id+1, envir = e)
			TRUE
		}
		
		plot_tm_lines <- function() {
			lres <- split_alpha_channel(gpl$line.col, alpha=alpha)
			lcol <- lres$col
			lopacity <- lres$opacity

			popups <- get_popups(gpl, type="line")
			dashArray <- lty2dashArray(gpl$line.lty)
			
			lf <- lf %>% addPolylines(data=shp, stroke=TRUE, weight=gpl$line.lwd, color=lcol, opacity = lopacity, popup = popups, options = pathOptions(clickable=!is.null(popups)), dashArray=dashArray, group=shp_name, layerId = id)

			
			if (!is.na(gpl$xline)) {
				if (gpl$line.col.legend.show) lf <- lf %>% add_legend(gpl, gt, aes="line.col", alpha=alpha)
			}
			
			assign("lf", lf, envir = e)
			assign("id", id+1, envir = e)
			
			TRUE
			# 			col <- do.call("process_color", c(list(gpl$line.col, alpha=gpl$line.alpha), gt$pc))
			# 			grid.shplines(shp, gp=gpar(col=col, lwd=gpl$line.lwd, lty=gpl$line.lty,
			# 									   lineend="butt"), i, k)
		}
		
		plot_tm_symbols <- function() {
			npol <- nrow(co)
			
			co[, 1] <- co[, 1] + gpl$symbol.xmod * bpl
			co[, 2] <- co[, 2] + gpl$symbol.ymod * bpl
			
			
			bres <- split_alpha_channel(gpl$symbol.border.col, alpha=alpha)
			bcol <- bres$col
			bopacity <- bres$opacity
			
			fres <- split_alpha_channel(rep(gpl$symbol.col, length.out=npol), alpha=alpha)
			fcol <- fres$col
			fopacity <- fres$opacity
			
			symbol.size <- gpl$symbol.size
			symbol.shape <- gpl$symbol.shape
			
			#browser()
			
			if (gpl$symbol.misc$symbol.are.markers) {
				if (is.null(gpl$symbol.names)) {
					gpl$symbol.names <- gpl$text
				}
			}
			
			popups <- get_popups(gpl, type="symbol")

			# sort symbols
			if (length(symbol.size)!=1) {
				decreasing <- order(-symbol.size)
				co2 <- co[decreasing,]
				symbol.size2 <- symbol.size[decreasing]
				symbol.shape2 <- symbol.shape[decreasing]
				
				fcol2 <- if (length(fcol)==1) fcol else fcol[decreasing]
				popups2 <- popups[decreasing]
			} else {
				co2 <- co
				symbol.size2 <- symbol.size
				symbol.shape2 <- symbol.shape
				fcol2 <- fcol
				popups2 <- popups
			}
			
			
			rad <- symbol.size2 * upl
			
			fixed <- ifelse(gpl$symbol.misc$symbol.are.dots, gt$dot.size.fixed, gt$symbol.size.fixed)
			are.icons <- gpl$symbol.misc$symbol.are.icons
			
			if (are.icons) {
				#symbol.size2 <- symbol.size2 / 3 # Correct for the fact that markers are larger than circle markers. This is good, but for static plots the icon size was already increased by icon.size=3, so this is to revert it for view mode
				if (any(symbol.shape2<1000)) {
					icons <- NULL
				} else {
					iconLib <- get(".shapeLib", envir = .TMAP_CACHE)[symbol.shape2-999]
					icons <- merge_icons(iconLib)
					#print(summary(symbol.size2))
					icons$iconWidth <- icons$iconWidth * symbol.size2
					icons$iconHeight <- icons$iconHeight * symbol.size2
					if (all(c("iconAnchorX", "iconAnchorY") %in% names(icons))) {
						icons$iconAnchorX <- icons$iconAnchorX * symbol.size2
						icons$iconAnchorY <- icons$iconAnchorY * symbol.size2
					}
				}
				lf <- lf %>% addMarkers(lng = co2[,1], lat=co2[,2], popup=popups2, group=shp_name, icon=icons, layerId = id)
			} else if (fixed) {
				lf <- lf %>% addCircleMarkers(lng=co2[,1], lat=co2[,2], fill = any(!is.na(fcol2)), fillColor = fcol2, fillOpacity=fopacity, color = bcol, stroke = !is.na(bcol) && bopacity!=0, radius = 20*symbol.size2, weight = 1, popup=popups2, group=shp_name, layerId = id)
			} else {
				lf <- lf %>% addCircles(lng=co2[,1], lat=co2[,2], fill = any(!is.na(fcol2)), fillColor = fcol2, fillOpacity=fopacity, color = bcol, stroke = !is.na(bcol) && bopacity!=0, radius=rad, weight =1, popup=popups2, group=shp_name, layerId = id)
			}
			
			
			if (!is.na(gpl$xcol)) {
				if (gpl$symbol.col.legend.show) lf <- lf %>% add_legend(gpl, gt, aes="symbol.col", alpha=alpha)
			}
			

			assign("lf", lf, envir = e)
			assign("id", id+1, envir = e)
			TRUE
			
		}
		plot_tm_text <- function() {
 			FALSE
		}
		plot_tm_raster <- function() {
			if (gpl$raster.misc$is.OSM) return(FALSE)	
			if (is.na(gpl$xraster)) {
				gpl$raster.legend.palette <- unique(gpl$raster)
			}
			
			shp@data@values <- match(gpl$raster, gpl$raster.legend.palette)

			res_leg <- add_legend(map=NULL, gpl, gt, aes="raster", alpha = alpha, list.only=TRUE)
			
			lf <- lf %>% addRasterImage(x=shp, colors=res_leg$col, opacity = res_leg$opacity, group=shp_name, project = FALSE, layerId = id)
			
			if (!is.na(gpl$xraster)) {
				if (gpl$raster.legend.show) lf <- lf %>% add_legend(gpl, gt, aes="raster", alpha=alpha)
			}

			assign("lf", lf, envir = e)
			assign("id", id+1, envir = e)
			TRUE
		}
		plot_tm_grid <- function() {
			FALSE
		}
		
		e2 <- environment()
		
		fnames <- paste("plot", gpl$plot.order, sep="_")
		layer_selection <- sapply(fnames, do.call, args=list(), envir=e2)
		any(layer_selection)
	}, shps, gp, gt$shp_name, SIMPLIFY = TRUE)
	
	groups <- gt$shp_name[group_selection]
	
	#center <- c(mean(lims[c(1,3)]), mean(lims[c(2,4)]))
	
	if (is.na(gt$control.position[1])) {
		control.position <- c("lefttop")
	} else if (!is.character(gt$control.position) || (!length(gt$control.position)==2)) {
		stop("Invalid control.position", call.=FALSE)
	} else if (gt$control.position[1] %in% c("left", "right") &&
		gt$control.position[2] %in% c("top", "bottom")) {
		control.position <- paste(gt$control.position[c(2,1)], collapse="")
	} else {
		stop("Invalid control.position", call.=FALSE)
	}

	
	lf <- lf %>% addLayersControl(baseGroups=names(basemaps), overlayGroups = groups, options = layersControlOptions(autoZIndex = TRUE), position=control.position)  
	
	set_bounds_view(lf, gt, bbx)
}

set_bounds_view <- function(lf, gt, bbx) {
	if (is.logical(gt$set.bounds)) {
		lims <- unname(unlist(lf$x$limits)[c(3,1,4,2)])
	} else {
		lims <- gt$set.bounds
	}
	if (!(identical(gt$set.bounds, FALSE))) {
		lf <- lf %>% setMaxBounds(lims[1], lims[2], lims[3],lims[4])
	}
	if (!is.na(gt$set.zoom.limits[1])) {
		if (is.na(gt$set.view[1])) {
			gt$set.view <- c(mean(lims[c(1,3)]), mean(lims[c(2,4)]), gt$set.zoom.limits[1])
		}
	}
	
	if (!is.na(gt$set.view[1])) {
		lf <- lf %>% setView(gt$set.view[1], gt$set.view[2], gt$set.view[3])
	} else {
		bbx <- unname(bbx)
		lf <- lf %>% fitBounds(bbx[1], bbx[2], bbx[3], bbx[4]) #setView(view[1], view[2], view[3])
	}
	lf
}

format_popups <- function(id=NULL, titles, values) {
	isnull <- sapply(values, is.null)
	
	titles <- titles[!isnull]
	values <- values[!isnull]
	
	if (!is.null(id)) {
		labels <- paste("<b>", htmlEscape(id), "</b>", sep="")
	} else {
		labels <- ""
	}
	
	titles_format <- sapply(titles, htmlEscape)
	values_format <- lapply(values, function(v) {
		htmlEscape(if (is.numeric(v)) fancy_breaks(v) else v)
	})
	
	
	labels2 <- mapply(function(l, v) {
		paste0("<tr><td style=\"color: #888888;\">", l, "</td><td>", v, "</td>")
	}, titles_format, values_format, SIMPLIFY=FALSE)
	
	labels3 <- paste0(do.call("paste", c(labels2, list(sep="</tr>"))), "</tr>")
	x <- paste("<div style=\"max-height:10em;overflow:auto;\"><table>
			   <thead><tr><th colspan=\"2\">", labels, "</th></thead></tr>", labels3, "</table></div>", sep="")
	x
}

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

get_popups <- function(gpl, type) {
	var_names <- paste(type, "names", sep=".")
	var_vars <- paste(type, "popup.vars", sep=".")
	
	dt <- gpl$data
	
	if (is.na(gpl[[var_vars]][1])) {
		popups <- NULL
	} else {
		popups <- format_popups(dt[[gpl[[var_names]]]], gpl[[var_vars]], dt[, gpl[[var_vars]], drop=FALSE])
	}
	popups
}

add_legend <- function(map, gpl, gt, aes, alpha, list.only=FALSE) {
	pal_name <- paste(aes, "legend.palette", sep=".")
	pal <- gpl[[pal_name]]
	
	if (nchar(pal[1])>10) {
		style <- attr(pal, "style")
		is.cont <- TRUE
		incl.na <- nchar(pal[length(pal)]) < 10
		pal <- sapply(pal, function(x) {
			p <- strsplit(x, split = "-", fixed=TRUE)[[1]]
			if (length(p)==1) p[1] else if (p[6]=="NA") p[5] else p[6]
		})
		if (incl.na) {
			colNA <- unname(pal[length(pal)])
			pal <- pal[-length(pal)]
			val <- as.numeric(gsub(",", "", gpl$fill.legend.labels, fixed = TRUE))
		} else {
			val <- as.numeric(gsub(",", "", gpl$fill.legend.labels, fixed = TRUE))
			colNA <- NA
		}
	} else {
		is.cont <- FALSE
	}
	RGBA <- col2rgb(pal, alpha = TRUE)
	col <- rgb(RGBA[1,], RGBA[2,], RGBA[3,], maxColorValue = 255)
	opacity <- unname(RGBA[4,1]/255) * alpha
	
	if (list.only) {
		return(list(col=col, opacity=opacity))
	}
	
	title_name <- paste(aes, "legend.title", sep=".")
	lab_name <- paste(aes, "legend.labels", sep=".")
	
	title <- if (nonempty_text(gpl[[title_name]])) expr_to_char(gpl[[title_name]]) else NULL

	legend.position <- gt$view.legend.position

	if (is.cont) {
		if (style=="quantile") {
			addLegend(map, position=legend.position,
					  pal=colorQuantile(pal, val, na.color=colNA), values=val)
		} else {
			addLegend(map, position=legend.position,
					  pal=colorNumeric(pal, val, na.color=colNA), values=val)
		}
	} else {
		addLegend(map, position=legend.position,
				  colors=col, labels = gpl[[lab_name]], opacity=opacity, title=title)
	}
}

working_internet <- function(url = "http://www.google.com") {
	# test the http capabilities of the current R build
	if (!capabilities(what = "http/ftp")) return(FALSE)
	
	# test connection by trying to read first line of url
	test <- try(suppressWarnings(readLines(url, n = 1)), silent = TRUE)
	
	# return FALSE if test inherits 'try-error' class
	!inherits(test, "try-error")
}

bbx_per_line <- function(bbx) {
	max_lines <- par("din")[2]*10
	(bbx[2,2] - bbx[2,1]) / max_lines
}

units_per_line <- function(bbx) {
	max_lines <- par("din")[2]*10
	
	# calculate top-center to bottom-center
	vdist <- distGeo(p1=c(mean(bbx[1,]), bbx[2,1]),
					 p2=c(mean(bbx[1,]), bbx[2,2]))
	vdist/max_lines
}

lty2dashArray <- function(lty) {
	numlty <- switch(lty,
					 solid=0,
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

get_view <- function(bbx) {
	lng <- mean(bbx[1,])
	lat <- mean(bbx[2,])
	
	lng_diff <- range(bbx[1,])
	lat_diff <- range(bbx[2,])
	max_diff <- max(lng_diff, lat_diff)
	if (max_diff < 360 / (2^20)) {
		zoom <- -21
	} else {
		zoom <- round(-(log2(max_diff) - (log2(360)))) + 2
		if (zoom < 1) zoom <- 1
	}
	c(lng=lng, lat=lat, zoom=zoom)
}
