view_tmap <- function(gp, shps=NULL, leaflet_id=1, showWarns=TRUE) {
	
	# determine view

	# take first small multiple
	#gp <- gps[[1]]
	gt <- gp$tm_layout
	gp$tm_layout <- NULL
	
	lf <- leaflet(options = leaflet::leafletOptions(crs=gt$projection))
	basemaps <- gt$basemaps
	basemaps.alpha <- gt$basemaps.alpha
	
	pOptions <- leaflet::popupOptions(minWidth = 200, maxWidth = 200)
	
	if (is.null(names(basemaps))) names(basemaps) <- sapply(basemaps, FUN = function(bm) {
		if (substr(bm, 1, 4) == "http") {
			x <- strsplit(bm, "/", fixed=TRUE)[[1]]
			x <- x[-c(1, (length(x)-2):length(x))]
			x <- x[x!=""]
			paste(x, collapse="/")
		} else bm
	})
	
	if (!is.na(gt$set.zoom.limits[1])) {
	  tileOptions <- lapply(basemaps.alpha, function(a) {
	  	tileOptions(minZoom=gt$set.zoom.limits[1], maxZoom=gt$set.zoom.limits[2], opacity=a)
	  })
	  	
	} else {
	  tileOptions <- lapply(basemaps.alpha, function(a) {
	  	tileOptions(opacity=a)
	  })
	}
	
	# add base layer(s)
	if (length(basemaps)) {
		for (i in 1:length(basemaps)) {
			bm <- unname(basemaps[i])
			bmname <- names(basemaps)[i]
			if (substr(bm, 1, 4) == "http") {
				lf <- lf %>% addTiles(bm, group=bmname, options=tileOptions[[i]])
			} else {
				lf <- lf %>% addProviderTiles(bm, group=bmname, options = tileOptions[[i]])
			}
		}
	}
	
	# add background overlay
	lf <- appendContent(lf, {
		tags$head(
			tags$style(HTML(paste(".leaflet-container {background:", gt$bg.color, ";}", sep="")))
		)	
	})
	
	# if (gt$bg.overlay.alpha!=0) {
	# 	if (any(sapply(gp, function(gpl)!is.null(gpl$raster)))) {
	# 		warning("Background overlays do not work yet with raster images. Background disabled.", call. = FALSE)
	# 	} else {
	# 		lf <- lf %>%  addRectangles(-540,-90,540,90, stroke=FALSE, fillColor=gt$bg.overlay, fillOpacity = gt$bg.overlay.alpha, layerId=0)
	# 		lf$x$limits <- NULL
	# 	}
	# }
	
	if (!length(gp)) {
		if (length(basemaps)>1) lf <- lf %>% addLayersControl(baseGroups=names(basemaps), options = layersControlOptions(autoZIndex = TRUE))

		if (!is.null(gt$shape.bbx)) {
			lf <- lf %>%
				fitBounds(gt$shape.bbx[1], gt$shape.bbx[2], gt$shape.bbx[3], gt$shape.bbx[4]) %>%
				addMarkers(gt$shape.center[1], gt$shape.center[2])
		}
		lf <- set_bounds_view(lf, gt)
		return(lf)
	}
	
	e <- environment()
	id <- 1
	alpha <- gt$alpha

	bbx <- attr(shps[[1]], "bbox")
	
	warns <- c(symbol=FALSE, text=FALSE, raster=FALSE, symbol_legend=FALSE, linelwd_legend=FALSE) # to prevent a warning for each shape
	
	group_selection <- mapply(function(shp, gpl, shp_name) {
		bbx <- attr(shp, "bbox")
		upl <- units_per_line(bbx)
		bpl <- bbx_per_line(bbx)
		if (inherits(shp, "Spatial")) {
			res <- get_sp_coordinates(shp, gpl, gt, bbx)
			co <- res$co
			if (gt$shape.line.center.type[1]=="segment") {
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

		
			lf <- lf %>% addPolygons(data=shp, stroke=stroke, weight=gpl$lwd, color=bcol, fillColor = fcol, opacity=bopacity, fillOpacity = fopacity, popup = popups, options = pathOptions(clickable=!is.null(popups)), group=shp_name, layerId = id, popupOptions = pOptions)


			if (!is.na(gpl$xfill[1])) {
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
			
			lf <- lf %>% addPolylines(data=shp, stroke=TRUE, weight=gpl$line.lwd, color=lcol, opacity = lopacity, popup = popups, options = pathOptions(clickable=!is.null(popups)), dashArray=dashArray, group=shp_name, layerId = id, popupOptions = pOptions)

			
			if (!is.na(gpl$xline[1])) {
				if (gpl$line.col.legend.show) lf <- lf %>% add_legend(gpl, gt, aes="line.col", alpha=alpha)
			}
			
			if (!is.na(gpl$xlinelwd[1]) && gpl$line.lwd.legend.show) {
				warns["linelwd_legend"] <- TRUE
				assign("warns", warns, envir = e)
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
			sel <- !is.na(symbol.size) & !is.na(fcol) & !is.na(symbol.shape)
			
			# return NULL is no symbols are selected (see tm_facets example)
			if (!any(sel)) return(FALSE)
			
			if (!all(sel)) {
				co <- co[sel, , drop=FALSE]
				fcol <- fcol[sel]
				fopacity <- fopacity[sel]
				symbol.size <- symbol.size[sel]
				symbol.shape <- symbol.shape[sel]
			}
			
			if (gpl$symbol.misc$symbol.are.markers) {
				if (is.na(gpl$symbol.names)) {
					gpl$data$MARKER__TEXT <- gpl$text 
					gpl$symbol.names <- "MARKER__TEXT"
				}
			}
			
			popups <- get_popups(gpl, type="symbol")[sel]

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
			
			
			rad <- unname(symbol.size2 * upl)
			
			fixed <- ifelse(gpl$symbol.misc$symbol.are.dots, gt$dot.size.fixed, gt$symbol.size.fixed)
			are.icons <- gpl$symbol.misc$symbol.are.icons
			clustering <- gpl$symbol.misc$clustering
			
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
				lf <- lf %>% addMarkers(lng = co2[,1], lat=co2[,2], popup=popups2, group=shp_name, icon=icons, layerId = id, clusterOptions=clustering)
			} else {
				if (!all(symbol.shape2 %in% c(1, 16, 19, 20, 21))) {
					warns["symbol"] <- TRUE
					assign("warns", warns, envir = e)
				}

				if (fixed) {
					lf <- lf %>% addCircleMarkers(lng=co2[,1], lat=co2[,2], fill = any(!is.na(fcol2)), fillColor = fcol2, fillOpacity=fopacity, color = bcol, stroke = !is.na(bcol) && bopacity!=0, radius = 20*symbol.size2, weight = 1, popup=popups2, group=shp_name, layerId = id, popupOptions = pOptions, clusterOptions=clustering)
				} else {
					lf <- lf %>% addCircles(lng=co2[,1], lat=co2[,2], fill = any(!is.na(fcol2)), fillColor = fcol2, fillOpacity=fopacity, color = bcol, stroke = !is.na(bcol) && bopacity!=0, radius=rad, weight =1, popup=popups2, group=shp_name, layerId = id, popupOptions = pOptions)
				}
			}
				
			
			
			if (!is.na(gpl$xcol[1])) {
				if (gpl$symbol.col.legend.show) lf <- lf %>% add_legend(gpl, gt, aes="symbol.col", alpha=alpha)
			}
			
			if (!is.na(gpl$xsize[1]) && gpl$symbol.size.legend.show) {
				warns["symbol_legend"] <- TRUE
				assign("warns", warns, envir = e)
			}

			

			assign("lf", lf, envir = e)
			assign("id", id+1, envir = e)
			TRUE
			
		}
		plot_tm_text <- function() {
# 			if (is.null(gpl$symbol.misc) || !gpl$symbol.misc$symbol.are.markers) {
# 				warns["text"] <- TRUE
# 				assign("warns", warns, envir = e)
# 			}
#  			FALSE
			npol <- nrow(co)
			text <- gpl$text
			col <- unname(gpl$text.color)
			size <- unname(gpl$text.size)
			
			opacity <- gpl$text.alpha
			
			
			co[, 1] <- co[, 1] + gpl$text.xmod * bpl
			co[, 2] <- co[, 2] + gpl$text.ymod * bpl
			
			# return NULL is no symbols are selected (see tm_facets example)
			if (!any(gpl$text_sel)) return(FALSE)
			
			if (!all(gpl$text_sel)) {
				co <- co[gpl$text_sel, , drop=FALSE]
				text <- text[gpl$text_sel]
				col <- col[gpl$text_sel]
				size <- size[gpl$text_sel]
			}
			
			sizeChar <- paste(round(size * 12), "px", sep="")
			colsize <- paste(col, sizeChar, sep="_^_")
			
			just <- switch(gpl$text.just[1],
						   left="right", 
						   right="left",
						   "bottom")
			
				
			cs_set <- unique(colsize)
			
			if (length(cs_set)==1) {
				lf <- lf %>% addLabelOnlyMarkers(lng = co[,1], lat = co[,2], label=text,
												 labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, direction = just, 
												 							opacity=opacity,
												 							textsize=sizeChar[1],
												 							style=list(color=col[1])))
			} else {
				for (i in 1:length(text)) {
					lf <- lf %>% addLabelOnlyMarkers(lng = co[i,1], lat = co[i,2], label=text[i],
													 labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, direction = just, 
													 							opacity=opacity,
													 							textsize=sizeChar[i],
													 							style=list(color=col[i])))	
				}
			}
			
			if (!is.na(gpl$xtcol[1])) {
				if (gpl$text.col.legend.show) lf <- lf %>% add_legend(gpl, gt, aes="text.col", alpha=alpha)
			}
			
			assign("lf", lf, envir = e)
			assign("id", id+1, envir = e)
			
			TRUE
		}
		plot_tm_raster <- function() {
			if (gpl$raster.misc$is.OSM) {
				if (is.na(gpl$raster.misc$leaflet.provider)) {
					warns["raster"] <- TRUE
					assign("warns", warns, envir = e)
				} else {
					if (gpl$raster.misc$leaflet.provider==gt$basemaps[1]) {
						warns["raster"] <- gpl$raster.misc$leaflet.provider
						assign("warns", warns, envir = e)
					}
				}
				return(FALSE)	
			}
			if (is.na(gpl$xraster[1])) {
				gpl$raster.legend.palette <- unique(gpl$raster)
			}
			
			shp@data@values <- match(gpl$raster, gpl$raster.legend.palette)

			res_leg <- add_legend(map=NULL, gpl, gt, aes="raster", alpha = alpha, list.only=TRUE)
			
			lf <- lf %>% addRasterImage(x=shp, colors=res_leg$col, opacity = res_leg$opacity, group=shp_name, project = FALSE, layerId = id)
			
			if (!is.na(gpl$xraster[1])) {
				if (gpl$raster.legend.show) lf <- lf %>% add_legend(gpl, gt, aes="raster", alpha=alpha)
			}

			assign("lf", lf, envir = e)
			assign("id", id+1, envir = e)
			TRUE
		}
		plot_tm_grid <- function() {
			lf <- lf %>% addGraticule()
			
			assign("lf", lf, envir = e)
			assign("id", id+1, envir = e)
			TRUE
		}
		
		e2 <- environment()
		
		fnames <- paste("plot", gpl$plot.order, sep="_")
		layer_selection <- sapply(fnames, do.call, args=list(), envir=e2)
		any(layer_selection)
	}, shps, gp, gt$shp_name, SIMPLIFY = TRUE)
	
	if (showWarns) {
		if (warns["symbol"]) message("Symbol shapes other than circles or icons are not supported in view mode.")
		if (warns["symbol_legend"]) message("Legend for symbol sizes not available in view mode.")
		if (warns["linelwd_legend"]) message("Legend for line widths not available in view mode.")
		if (identical(unname(warns["raster"]), TRUE)) {
			message("Raster data contains OpenStreetMapData (read with read_osm) from provider that is not known from http://leaflet-extras.github.io/leaflet-providers/preview/index.html")	
		} else if (!(identical(unname(warns["raster"]), FALSE))) {
			message("Raster data contains OpenStreetMapData (read with read_osm). Therefore, the basemap has been set to \"", warns["raster"], "\"")
		}
	}
	
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

	if (gt$scale.show) {
		u <- gt$shape.units_args$target
		metric <- (u %in% c("m", "km", "metric"))
	 	lf <- lf %>% addScaleBar(position = gt$scale.position, options = scaleBarOptions(maxWidth=gt$scale.width, metric=metric, imperial = !metric))
	}
	# print(leaflet_id)
	# if (gt$title!="") {
	# 	lf <- lf %>% onRender(paste("
	# 		function(el, x) {
	# 			var tldiv = document.getElementsByClassName(\"leaflet-top leaflet-left\")[", leaflet_id-1, "];
	# 			var titlediv = document.createElement('div');
	# 			titlediv.className = \"info legend leaflet-control\";
	# 			titlediv.innerHTML = \"<b>", gt$title, "</b>\";
	# 			tldiv.insertBefore(titlediv, tldiv.childNodes[0]);
	# 		}", sep=""))
	# }
	
	lf <- set_bounds_view(lf, gt, bbx)
	lf$title <- gt$title
	lf
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
	} else if (!missing(bbx)) {
		bbx <- unname(bbx)
		lf <- lf %>% fitBounds(bbx[1], bbx[2], bbx[3], bbx[4]) #setView(view[1], view[2], view[3])
	}
	lf
}

format_popups <- function(id=NULL, titles, format, values) {
	isnull <- sapply(values, is.null)
	
	titles <- titles[!isnull]
	titles[names(titles)!=""] <- names(titles)[names(titles)!=""]
	
	values <- values[!isnull]
	
	islist <- is.list(format) && length(format)>0 && is.list(format[[1]])
	if (!islist) {
		format <- lapply(1:length(titles), function(i) format)
	}
	
	
	if (!is.null(id)) {
		labels <- paste("<b>", htmlEscape(id), "</b>", sep="")
	} else {
		labels <- ""
	}
	
	titles_format <- sapply(titles, htmlEscape)
	values_format <- mapply(function(v, f) {
		htmlEscape(if (is.numeric(v)) do.call("fancy_breaks", c(list(vec=v, intervals=FALSE), f)) else v)
	}, values, format, SIMPLIFY = FALSE)
	
	
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
	var_format <- paste(type, "popup.format", sep=".")
	
	dt <- gpl$data

	if (is.na(gpl[[var_vars]][1])) {
		popups <- NULL
	} else {
		popups <- format_popups(dt[[gpl[[var_names]]]], gpl[[var_vars]], gpl[[var_format]], dt[, gpl[[var_vars]], drop=FALSE])
	}
	popups
}

add_legend <- function(map, gpl, gt, aes, alpha, list.only=FALSE) {
	pal_name <- paste(aes, "legend.palette", sep=".")
	val_name <- paste(aes, "legend.values", sep=".")
	lab_name <- paste(aes, "legend.labels", sep=".")
	
	pal <- gpl[[pal_name]]
	val <- gpl[[val_name]]
	lab <- gpl[[lab_name]]
	
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
			textNA <- lab[length(lab)]
		} else {
			colNA <- NA
			textNA <- NA
		}
	} else {
		is.cont <- FALSE
		if (length(pal) != length(val)) {
			colNA <- pal[length(pal)]
			textNA <- lab[length(pal)]
			pal <- pal[-length(pal)]
			lab <- lab[-length(lab)]
		} else {
			colNA <- NA
			textNA <- NA
		}
	}

	allNAs <- (length(pal) == 0)
	
	if (allNAs) {
		col <- character()
		opacity <- alpha
	} else {
		RGBA <- col2rgb(pal, alpha = TRUE)
		col <- rgb(RGBA[1,], RGBA[2,], RGBA[3,], maxColorValue = 255)
		opacity <- unname(RGBA[4,1]/255) * alpha
	}
	
	if (!is.na(colNA)) {
		RGBA_NA <- col2rgb(colNA, alpha = TRUE)
		colNA <- rgb(RGBA_NA[1,], RGBA_NA[2,], RGBA_NA[3,], maxColorValue = 255)
	}
	
	if (list.only) {
		if (!is.na(colNA)) col <- c(col, colNA)
		return(list(col=pal, opacity=opacity))
	}
	
	title_name <- paste(aes, "legend.title", sep=".")
	
	title <- if (nonempty_text(gpl[[title_name]])) expr_to_char(gpl[[title_name]]) else NULL

	legend.position <-gt$view.legend.position

	if (is.cont) {
		legvals <- if (!is.na(colNA)) c(val, NA) else val

		if (style=="quantile") {
			addLegend(map, position=legend.position,
					  pal=colorQuantile(col, val, na.color=colNA, alpha = FALSE), values=legvals, na.label = textNA, title=title, opacity=opacity)
		} else {
			addLegend(map, position=legend.position,
					  pal=colorNumeric(col, val, na.color=colNA, alpha = FALSE), values=legvals, na.label = textNA, title=title, opacity=opacity)
		}
	} else {
		if (allNAs) {
			addLegend(map, position=legend.position, colors=colNA, labels=textNA, title=title, opacity=opacity)
		} else {
			legvals <- if (!is.na(colNA)) factor(c(lab, NA), levels=lab) else factor(lab, levels=lab)
			lab <- factor(lab, levels=lab)
			addLegend(map, position=legend.position, 
					  pal=colorFactor(col, domain=lab, na.color = colNA, ordered = TRUE, alpha = FALSE), values = legvals, na.label=textNA, title=title, opacity=opacity)
		}
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
	max_lines <- 60 #par("din")[2]*10 #disabled because window height doesn't influence scaling
	(bbx[2,2] - bbx[2,1]) / max_lines
}

units_per_line <- function(bbx) {
	max_lines <- 60 #par("din")[2]*10 #disabled because window height doesn't influence scaling

	# calculate top-center to bottom-center
	vdist <- tmaptools::approx_distances(bbx, projection = "longlat", target = "m")$vdist
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

get_epsg_number <- function(proj) {
	if (inherits(proj, "CRS")) proj <- attr(proj, "projargs")
	
	pat <- "^.*\\=epsg ?: ?(\\S*)(.*)$"
	epsg <- as.numeric(sub(pat, "\\1", proj[grepl(pat, proj)]))
	if (length(epsg)==0) NA else epsg
}

