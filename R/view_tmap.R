view_tmap <- function(gp, shps=NULL, leaflet_id=1, showWarns=TRUE, gal = NULL, in.shiny = FALSE, lf = NULL) {
	
	gt <- gp$tm_layout
	gp$tm_layout <- NULL
	
	proxy <- !is.null(lf)
	
	if (!proxy) lf <- leaflet(options = leaflet::leafletOptions(crs=gt$projection))

	# add background overlay
	if (!in.shiny) {
		lf <- appendContent(lf, {
			tags$head(
				tags$style(HTML(paste(".leaflet-container {background:", gt$bg.color, ";}", sep="")))
			)	
		})
	}
	
	# if (gt$bg.overlay.alpha!=0) {
	# 	if (any(sapply(gp, function(gpl)!is.null(gpl$raster)))) {
	# 		warning("Background overlays do not work yet with raster images. Background disabled.", call. = FALSE)
	# 	} else {
	# 		lf <- lf %>%  addRectangles(-540,-90,540,90, stroke=FALSE, fillColor=gt$bg.overlay, fillOpacity = gt$bg.overlay.alpha, layerId=0)
	# 		lf$x$limits <- NULL
	# 	}
	# }
	
	# if (!length(gp)) {
	# 	if (length(basemaps)>1) lf <- lf %>% addLayersControl(baseGroups=names(basemaps), options = layersControlOptions(autoZIndex = TRUE))
	# 
	# 	if (!is.null(gt$shape.bbx)) {
	# 		lf <- lf %>%
	# 			fitBounds(gt$shape.bbx[1], gt$shape.bbx[2], gt$shape.bbx[3], gt$shape.bbx[4]) %>%
	# 			addMarkers(gt$shape.center[1], gt$shape.center[2])
	# 	}
	# 	lf <- set_bounds_view(lf, gt)
	# 	return(lf)
	# }
	
	e <- environment()
	alpha <- gt$alpha

	gt$global_bbox_specified <- !is.null(gt$bbox)
	
	if (!gt$global_bbox_specified) {
		gt$bbox <- attr(shps[[gt$shape.masterID]], "bbox")
	}

	warns <- c(symbol=FALSE, text=FALSE, raster=FALSE, symbol_legend=FALSE, linelwd_legend=FALSE) # to prevent a warning for each shape
	
	if (inherits(shps, "sf")) shps <- list(shps)

	bases <- if ("bases" %in% ls(envir = .TMAP_CACHE)) get("bases", envir = .TMAP_CACHE) else NA
	overlays <- if ("overlays" %in% ls(envir = .TMAP_CACHE)) get("overlays", envir = .TMAP_CACHE) else NA
	overlays_tiles <- if ("overlays_tiles" %in% ls(envir = .TMAP_CACHE)) get("overlays_tiles", envir = .TMAP_CACHE) else character(0)
	
	# bases <- NA
	# overlays <- NA
	# overlays_tiles <- character(0)
	
	## keep track of layerIds to prevent duplicates (duplicates are not shown in leaflet)
	# layerIds <- list(polygons = character(0),
	# 				 lines = character(0),
	# 				 points = character(0))
	
	
	if (proxy) {
		layerIds <- get("layerIdsNew", envir = .TMAP_CACHE)
		if (length(layerIds) == 0) {
			start_pane_id <- 401
		} else {
			start_pane_id <- min(as.integer(substr(names(layerIds), 5, 7)))	
		}
	} else {
		layerIds <- list()
		start_pane_id <- 401
	}
	


	# should the layer control include base layers? TRUE if |basemaps| > 1 || names/groups are specified
	basename.specified <- FALSE
	

	addBaseGroup <- function(group) {
		for (g in group) {
			if (is.na(bases[1])) {
				bases <- g
			} else if (!(g %in% bases)) {
				bases <- c(bases, g)
			}
		}
		assign("bases", bases, envir = e)
	}
	
	eraseBaseGroup <- function() {
		assign("bases", character(0), envir = e)
	}
	
	eraseOverlayTiles <- function() {
		overlays <- setdiff(overlays, overlays_tiles)
		assign("overlays", overlays, envir = e)
	}
	
	addOverlayGroup <- function(group, are.tiles = FALSE) {
		for (g in group) {
			if (is.na(overlays[1])) {
				overlays <- g
			} else if (!(g %in% overlays)) {
				overlays <- c(overlays, g)
			}
		}
		assign("overlays", overlays, envir = e)
		if (are.tiles) assign("overlays_tiles", c(overlays_tiles, group), envir = e)
	}
	
	### find z indeces and create tmapXXX panes
	zids <- lapply(gp, function(gpl) {
		po <- gpl$plot.order
		
		po2 <- substr(po, 4, nchar(po))
		po2[po2 == "symbols"] <- "symbol"
		po2[po2 == "tiles"] <- "tile"
		po2[po2 == "lines"] <- "line"

		zi <- sapply(po2, function(p) {
			if (p == "grid") gt$grid.zindex else gpl[[paste0(p, ".zindex")]]
		})

		if (!is.null(gpl$tile.gtype) && gpl$tile.gtype == "base") {
			zi[names(zi) == "tile"] <- 0
		}
		zi
	})
	zids_vec <- unlist(zids)
	
	# For tmapProxy: only use pane with a higher z number than existing ones
	# Only use free panes: every layer must be in a different pane
	z_free <- setdiff(start_pane_id:(start_pane_id+length(zids_vec)*2-1), na.omit(zids_vec))
	zids_vec[is.na(zids_vec)] <- rep(z_free, length.out = sum(is.na(zids_vec)))
	zids_len <- sapply(zids, length)
	zindices <- split(zids_vec, unlist(mapply(rep, 1:length(zids), each = zids_len, SIMPLIFY = FALSE)))
	tmap_zindices <- sort(unique(unname(setdiff(zids_vec, 0))))

	## get/set existing panes
	if (!proxy) {
		assign("pane_ids", tmap_zindices, envir = .TMAP_CACHE)
		z_panes <- integer()
	} else {
		z_panes <- get("pane_ids", envir = .TMAP_CACHE)
		assign("pane_ids", union(tmap_zindices, z_panes), envir = .TMAP_CACHE)
	}
	
	# add new panes
	for (z in setdiff(tmap_zindices, z_panes)) {
		lf <- addMapPane(lf, paneName(z), zIndex = z)
	}
	

	group_selection <- mapply(function(shp, gpl, shp_name, zindex) {
		if (!is.null(shp)) {
			if (nrow(shp) == 0) {
				shp <- NULL
			} else {
				bbx <- attr(shp, "bbox")
				upl <- units_per_line(bbx)
				bpl <- bbx_per_line(bbx)
				if (inherits(shp, "sf")) {
					res <- get_sf_coordinates(shp, gpl)
					co <- res$co
					if (attr(shp, "point.per")=="segment") {
						gpl <- res$gpl
						shp <- res$shp
					}
				}
			}
		}
		
		
		plot_tm_fill <- function(zi) {
			if (is.null(shp)) return(FALSE)
			
			bres <- split_alpha_channel(gpl$col, alpha=alpha)
			bcol <- bres$col
			bopacity <- bres$opacity

			fres <- split_alpha_channel(gpl$fill, alpha=alpha)
			fcol <- fres$col
			fopacity <- fres$opacity
			
			if (!is.null(gpl$fill)) {
				popups <- get_popups(gpl, type="fill")
				labels <- get_labels(gpl, type="fill")
			} else {
				popups <- NULL
				labels <- NULL
			}
			
			dashArray <- lty2dashArray(gpl$lty)
			stroke <- gpl$lwd>0 && !is.na(bcol) && bopacity!=0
			
			group_name <- if (is.na(gpl$fill.group)) shp_name else gpl$fill.group
			addOverlayGroup(group_name)
			
			pane <- paneName(zi)
			
			# pane <- nextPane(pane)
			# lf <- addPane(lf, pane)
			
			shp$tmapID <- if (!is.null(labels)) as.character(labels) else shp$tmapID
			shp$tmapID2 <- submit_labels(shp$tmapID, "polygons", pane, group_name, e)
			
			lf <- lf %>% addPolygons(data=shp, label = ~tmapID, layerId = shp$tmapID2, stroke=stroke, weight=gpl$lwd, color=bcol, fillColor = fcol, opacity=bopacity, fillOpacity = fopacity, dashArray = dashArray, popup = popups, options = pathOptions(clickable=!is.null(popups), pane=pane), group=group_name)
			
			# if (!is.null(labels)) {
			# 	lf <- lf %>% 
			# 		addSearchFeatures(targetGroups  = shp_name, options = searchFeaturesOptions(zoom = 7, openPopup=FALSE))
			# }

			if (!is.na(gpl$xfill[1])) {
				if (gpl$fill.legend.show) lf <- lf %>% add_legend(gpl, gt, aes="fill", alpha=alpha, group = if (gt$free.scales.fill) group_name else NULL, zindex = zi)
			}

			assign("lf", lf, envir = e)
			TRUE
		}
		
		plot_tm_lines <- function(zi) {
			if (is.null(shp)) return(FALSE)
			lres <- split_alpha_channel(gpl$line.col, alpha=alpha)
			lcol <- lres$col
			lopacity <- lres$opacity

			popups <- get_popups(gpl, type="line")
			labels <- get_labels(gpl, type="line")

			dashArray <- lty2dashArray(gpl$line.lty)
			

			group_name <- if (is.na(gpl$line.group)) shp_name else gpl$line.group
			addOverlayGroup(group_name)
			
			pane <- paneName(zi)
			# pane <- nextPane(pane)
			# lf <- addPane(lf, pane)
			
			shp$tmapID <- if (!is.null(labels)) as.character(labels) else shp$tmapID
			shp$tmapID2 <- submit_labels(shp$tmapID, "lines", pane, group_name, e)
			
			
			lf <- lf %>% addPolylines(data=shp, label = ~tmapID, layerId = shp$tmapID2, stroke=TRUE, weight=gpl$line.lwd, color=lcol, opacity = lopacity, popup = popups, options = pathOptions(clickable=!is.null(popups), pane=pane), dashArray=dashArray, group=group_name) 

			# if (!is.null(labels)) {
			# 	lf <- lf %>% 
			# 		addSearchFeatures(targetGroups  = shp_name, options = searchFeaturesOptions(zoom = 7, openPopup=FALSE))
			# }
			
			if (!is.na(gpl$xline[1])) {
				if (gpl$line.col.legend.show) lf <- lf %>% add_legend(gpl, gt, aes="line.col", alpha=alpha, group = if (gt$free.scales.line.lwd) group_name else NULL, zindex = zi)
			}
			
			if (!is.na(gpl$xlinelwd[1]) && gpl$line.lwd.legend.show) {
				warns["linelwd_legend"] <- TRUE
				assign("warns", warns, envir = e)
			}
			
			
			assign("lf", lf, envir = e)

			TRUE
			# 			col <- do.call("process_color", c(list(gpl$line.col, alpha=gpl$line.alpha), gt$pc))
			# 			grid.shplines(shp, gp=gpar(col=col, lwd=gpl$line.lwd, lty=gpl$line.lty,
			# 									   lineend="butt"), i, k)
		}
		
		plot_tm_symbols <- function(zi) {
			if (is.null(shp)) return(FALSE)
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

			pane <- paneName(zi)
			# pane <- nextPane(pane)
			# lf <- addPane(lf, pane)
			
			group_name <- if (is.na(gpl$symbol.group)) shp_name else gpl$symbol.group
			addOverlayGroup(group_name)
			
			popups <- get_popups(gpl, type="symbol")
			labels <- as.character(get_labels(gpl, type="symbol"))
			ids <- submit_labels(labels, "symbols", pane, group_name, e)
			
			

			popups <- popups[sel]
			
			# sort symbols
			if (length(symbol.size)!=1) {
				decreasing <- order(-symbol.size)
				co2 <- co[decreasing,]
				symbol.size2 <- symbol.size[decreasing]
				symbol.shape2 <- symbol.shape[decreasing]
				
				fcol2 <- if (length(fcol)==1) fcol else fcol[decreasing]
				popups2 <- popups[decreasing]
				labels2 <- labels[decreasing]
				ids2 <- ids[decreasing]
			} else {
				co2 <- co
				symbol.size2 <- symbol.size
				symbol.shape2 <- symbol.shape
				fcol2 <- fcol
				popups2 <- popups
				labels2 <- labels
				ids2 <- ids
			}
			
			if (length(labels2) == 0) labels2 <- NULL
			
			rad <- unname(symbol.size2 * upl)
			
			fixed <- ifelse(gpl$symbol.misc$symbol.are.dots, gt$dot.size.fixed, gt$symbol.size.fixed)
			are.icons <- gpl$symbol.misc$symbol.are.icons
			clustering <- gpl$symbol.misc$clustering
			
			
			
			if (are.icons) {
				#symbol.size2 <- symbol.size2 / 3 # Correct for the fact that markers are larger than circle markers. This is good, but for static plots the icon size was already increased by icon.size=3, so this is to revert it for view mode
				if (any(symbol.shape2<1000)) {
					icons <- NULL
				} else {
					iconLib <- get("shapeLib", envir = .TMAP_CACHE)[symbol.shape2-999]
					icons <- merge_icons(iconLib)
					#print(summary(symbol.size2))
					icons$iconWidth <- icons$iconWidth * symbol.size2
					icons$iconHeight <- icons$iconHeight * symbol.size2
					if (all(c("iconAnchorX", "iconAnchorY") %in% names(icons))) {
						icons$iconAnchorX <- icons$iconAnchorX * symbol.size2
						icons$iconAnchorY <- icons$iconAnchorY * symbol.size2
					}
				}
				lf <- lf %>% addMarkers(lng = co2[,1], lat=co2[,2], popup=popups2, label = labels2, layerId = ids2, group=group_name, icon=icons, clusterOptions=clustering, options = markerOptions(clickable=!is.null(popups), pane=pane))
			} else {
				if (!all(symbol.shape2 %in% c(1, 16, 19, 20, 21))) {
					warns["symbol"] <- TRUE
					assign("warns", warns, envir = e)
				}
				
				if (fixed) {
					lf <- lf %>% addCircleMarkers(lng=co2[,1], lat=co2[,2], label = labels2, layerId = ids2, fill = any(!is.na(fcol2)), fillColor = fcol2, fillOpacity=fopacity, color = bcol, stroke = !is.na(bcol) && bopacity!=0, radius = 20*symbol.size2, weight = 1, popup=popups2, group=group_name, clusterOptions=clustering, options = pathOptions(clickable=!is.null(popups), pane=pane))
				} else {
					lf <- lf %>% addCircles(lng=co2[,1], lat=co2[,2], label = labels2, layerId = ids2, fill = any(!is.na(fcol2)), fillColor = fcol2, fillOpacity=fopacity, color = bcol, stroke = !is.na(bcol) && bopacity!=0, radius=rad, weight =1, popup=popups2, group=group_name, options = pathOptions(clickable=!is.null(popups), pane=pane))
				}
			}
				
			# if (!is.null(labels)) {
			# 	lf <- lf %>% 
			# 		addSearchFeatures(targetGroups  = shp_name, options = searchFeaturesOptions(zoom = 7, openPopup=FALSE))
			# }
			
			if (!is.na(gpl$xcol[1])) {
				if (gpl$symbol.col.legend.show) lf <- lf %>% add_legend(gpl, gt, aes="symbol.col", alpha=alpha, group = if (gt$free.scales.symbol.col) group_name else NULL, zindex = zi)
			}
			
			if (!is.na(gpl$xsize[1]) && gpl$symbol.size.legend.show) {
				warns["symbol_legend"] <- TRUE
				assign("warns", warns, envir = e)
			}

			

			assign("lf", lf, envir = e)
			TRUE
			
		}
		plot_tm_text <- function(zi) {
			if (is.null(shp)) return(FALSE)
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
			
			direction <- ifelse(gpl$text.just == "left", "right",
						 ifelse(gpl$text.just == "right", "left",
						 ifelse(gpl$text.just == "top", "bottom",
						 ifelse(gpl$text.just == "bottom", "top", "center"))))
			
				
			cs_set <- unique(colsize)
			
			clustering <- gpl$text.misc$clustering
			
			group_name <- if (is.na(gpl$text.group)) shp_name else gpl$text.group
			addOverlayGroup(group_name)
			
			pane <- paneName(zi)
			# pane <- nextPane(pane)
			# lf <- addPane(lf, pane)
			
			
			if (length(cs_set)==1) {
				lf <- lf %>% addLabelOnlyMarkers(lng = co[,1], lat = co[,2], label=text,
												 group=group_name, 
												 labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, direction = direction, 
												 							opacity=opacity,
												 							textsize=sizeChar[1],
												 							style=list(color=col[1])),
												 clusterOptions = clustering,
												 options = markerOptions(pane = pane))
			} else {
				for (i in 1:length(text)) {
					lf <- lf %>% addLabelOnlyMarkers(lng = co[i,1], lat = co[i,2], label=text[i],
													 group=group_name, 
													 labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, direction = direction, 
													 							opacity=opacity,
													 							textsize=sizeChar[i],
													 							style=list(color=col[i])),
													 clusterOptions = clustering,
													 options = markerOptions(pane = pane))	
				}
			}
			
			if (!is.na(gpl$xtcol[1])) {
				if (gpl$text.col.legend.show) lf <- lf %>% add_legend(gpl, gt, aes="text.col", alpha=alpha, group = if (gt$free.scales.text.col) group_name else NULL, zindex = zi)
			}
			
			assign("lf", lf, envir = e)

			TRUE
		}
		plot_tm_raster <- function(zi) {
			if (is.null(shp)) return(FALSE)
			if (gpl$raster.misc$is.OSM) {
				if (is.na(gpl$raster.misc$leaflet.server)) {
					warns["raster"] <- TRUE
					assign("warns", warns, envir = e)
				} else {
					if (gpl$raster.misc$leaflet.server==gt$basemaps[1]) {
						warns["raster"] <- gpl$raster.misc$leaflet.server
						assign("warns", warns, envir = e)
					}
				}
				return(FALSE)	
			}
			if (is.na(gpl$xraster[1])) {
				gpl$raster.legend.values <- 1
			}
			

			group_name <- if (is.na(gpl$raster.group)) shp_name else gpl$raster.group
			addOverlayGroup(group_name)
			
			#res_leg <- add_legend(map=NULL, gpl, gt, aes="raster", alpha = alpha, group = if (gt$free.scales.raster) group_name else NULL, list.only=TRUE)

			pal <- na.omit(unique(gpl$raster))
			pal <- pal[substr(pal, 8,10)!="00"] ## remove transparant colors
			
			shp@data@values <- match(gpl$raster, pal)
			
			res <- split_alpha_channel(pal, alpha)
			pal_col <- res$col
			pal_opacity <- if (length(res$opacity) == 0L) 0 else max(res$opacity)
			
			mappal <- function(x) {
				if (all(is.na(shp@data@values))) return(rep("#00000000", length(x)))
				
				y <- pal_col[x]
				y[is.na(y)] <- "#00000000"
				y
			}
			
			pane <- paneName(zi)
			
			layerId <- submit_labels(pane, "raster", pane, group_name, e)

			lf <- lf %>% addRasterImage(x=shp, colors=mappal, opacity = pal_opacity, group=group_name, project = FALSE, layerId = layerId)
			
			if (!is.na(gpl$xraster[1])) {
				if (gpl$raster.legend.show) lf <- lf %>% add_legend(gpl, gt, aes="raster", alpha=alpha, group = if (gt$free.scales.raster) group_name else NULL, zindex = zi)
			}

			assign("lf", lf, envir = e)
			TRUE
		}
		plot_tm_grid <- function(zi) {
			lf <- lf %>% addGraticule(options = pathOptions(pane = paneName(zi)))
			
			assign("lf", lf, envir = e)
			TRUE
		}
		
		plot_tm_tiles <- function(zi) {
			basemaps <- gpl$tile.server
			basemaps.alpha <- gpl$tile.alpha
			type <- gpl$tile.gtype
			tms <- gpl$tile.tms
			
			if (is.null(basemaps)) {
				return(FALSE)
			}
			
			if (is.na(basemaps[1])) {
				if (type == "base") eraseBaseGroup() else eraseOverlayTiles()
				return(FALSE) 
			}
			

			
			
			group_names <- if (is.null(gpl$tile.group)) {
				NULL
			} else if (is.na(gpl$tile.group[1])) {
				vapply(basemaps, FUN = function(bm) {
					if (substr(bm, 1, 4) == "http") {
						x <- strsplit(bm, "/", fixed=TRUE)[[1]]
						x <- x[-c(1, (length(x)-2):length(x))]
						x <- x[x!=""]
						paste(x, collapse="/")
					} else bm
				}, character(1))
			} else {
				if (type == "base") assign("basename.specified", TRUE, envir = e)
				rep(gpl$tile.group, length.out = length(basemaps))
			}
			
			if (!is.null(group_names)) {
				if (type == "base") {
					addBaseGroup(group_names)	
				} else {
					addOverlayGroup(group_names, are.tiles = TRUE)
				}
			}
			
			if(type == "base") {
				pane <- "tilePane"
			} else {
				pane <- paneName(zi)
				# pane <- nextPane(pane)
				# lf <- addPane(lf, pane)
			}
				
			if (!is.na(gt$set.zoom.limits[1])) {
				tileOptions <- mapply(function(a, tmsi) {
					tileOptions(minZoom=gt$set.zoom.limits[1], maxZoom=gt$set.zoom.limits[2], opacity=a, pane=pane, tms = tmsi)
				}, basemaps.alpha, tms, SIMPLIFY = FALSE)
				
			} else {
				tileOptions <- mapply(function(a, tmsi) {
					tileOptions(opacity=a, pane=pane, tms = tmsi)
				}, basemaps.alpha, tms, SIMPLIFY = FALSE)
			}
			
			allLettersOrDots <- function(x) grepl("^[A-Za-z\\.]*$", x)
			
			# add base layer(s)
			if (length(basemaps)) {
				for (i in 1:length(basemaps)) {
					bm <- unname(basemaps[i])
					bmname <- unname(group_names[i])
					bm4 <- substr(bm, 1, 4)
					if (allLettersOrDots(bm4) && bm4 != "http") {
						# it is not possible to check provider options, since leaflet::providers is not exported
						lf <- lf %>% addProviderTiles(bm, group=bmname, options = tileOptions[[i]])
					} else {
						lf <- lf %>% addTiles(bm, group=bmname, options=tileOptions[[i]])
					}
				}
			}
			assign("lf", lf, envir = e)

			TRUE
		}
		
		e2 <- environment()
		
		fnames <- paste("plot", gpl$plot.order, sep="_")

		layer_selection <- unlist(mapply(function(fn, zi) {
			do.call(fn, list(zi = zi), envir = e2)
		}, fnames, zindex, SIMPLIFY = FALSE))
			
		any(layer_selection)
	}, shps, gp, gt$shp_name, zindices, SIMPLIFY = TRUE)
	
	if (gt$show.messages && showWarns) {
		if (warns["symbol"]) message("Symbol shapes other than circles or icons are not supported in view mode.")
		if (warns["symbol_legend"]) message("Legend for symbol sizes not available in view mode.")
		if (warns["linelwd_legend"]) message("Legend for line widths not available in view mode.")
		if (identical(unname(warns["raster"]), TRUE)) {
			message("OpenStreetMapData read with read_osm is static, so not usable in view mode. Please use tm_basemap or tm_tiles.")	
		} else if (!(identical(unname(warns["raster"]), FALSE))) {
			message("OpenStreetMapData read with read_osm is static, so not usable in view mode. Please use tm_basemap or tm_tiles, with the provider name set to \"", warns["raster"], "\"")
		}
	}
	
	## add manual legends (tm_add_legend)
	if (length(gal) > 0) {
		for (gali in gal) {
			if (gali$type != "fill") {
				if (gt$show.messages) {
					message("only legends of type \"fill\" supported in view mode")
				}
			} else {
				RGBA <- col2rgb(gali$col, alpha = TRUE)
				col <- rgb(RGBA[1,], RGBA[2,], RGBA[3,], maxColorValue = 255)
				opacity <- unname(RGBA[4,1]/255) * alpha
				
				lf <- lf %>% addLegend(position=gt$view.legend.position,
									   group = gali$group,
									   colors = col,
									   labels = gali$labels,
									   title=gali$title, 
									   opacity=opacity)
			}
		}
	}
	
	
	
	#groups <- gt$shp_name[group_selection]
	
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
	
	if (length(bases) == 1 && !basename.specified) {
		if (!is.na(overlays[1])) {
			lf <- lf %>% addLayersControl(overlayGroups = unname(overlays), options = layersControlOptions(autoZIndex = TRUE), position=control.position)
		}
	} else if (!is.na(overlays[1])) {
		lf <- lf %>% addLayersControl(baseGroups=unname(bases), overlayGroups = unname(overlays), options = layersControlOptions(autoZIndex = TRUE), position=control.position)  
	} else {
		lf <- lf %>% addLayersControl(baseGroups=unname(bases), options = layersControlOptions(autoZIndex = TRUE), position=control.position)  
	}


	if (gt$scale.show) {
		u <- gt$shape.units$unit
		metric <- (u %in% c("m", "km", "metric"))
	 	lf <- lf %>% addScaleBar(position = gt$scale.position, options = scaleBarOptions(maxWidth=gt$scale.width, metric=metric, imperial = !metric))
	}
	
	if (gt$minimap.show) {
		mmargs <- gt[substr(names(gt), 1, 4) == "mini"]
		names(mmargs) <- substr(names(mmargs), 9, nchar(names(mmargs)))
		names(mmargs)[names(mmargs) == "toggle"] <- "toggleDisplay"
		names(mmargs)[names(mmargs) == "server"] <- "tiles"
		mmargs$show <- NULL
		
		specified_tiles <- !is.na(mmargs$tiles)
		
		if (!specified_tiles) {
			if (length(bases) == 0) {
				mmargs$tiles <- NULL
			} else {
				mmargs$tiles <- bases[1]	
			} 	
		} 
		
		lf <- tryCatch({
			lf <- do.call(addMiniMap, c(list(map = lf), mmargs)) 
			if (!specified_tiles && (length(bases) > 0)) {
				lf <- lf %>% 
					htmlwidgets::onRender("
			    function(el, x) {
			      var myMap = this;
			      myMap.on('baselayerchange',
			        function (e) {
			          myMap.minimap.changeLayer(L.tileLayer.provider(e.name));
			        })
			    }")
			}
			lf
		}, error = function(e) {
			if (gt$show.messages) message("tm_minimap requires the leaflet package. Please run library(leaflet)")
			lf
		})
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
	
	

	if (!proxy) lf <- set_bounds_view(lf, gt)
	lf$title <- gt$title
	
	assign("layerIds", layerIds, envir = .TMAP_CACHE)
	assign("bases", bases, envir = .TMAP_CACHE)
	assign("overlays", overlays, envir = .TMAP_CACHE)
	assign("overlays_tiles", overlays_tiles, envir = .TMAP_CACHE)
	
	
	lf	
	
}

set_bounds_view <- function(lf, gt) {
	# if (!is.null(gt$shape.bbx)) {
	# 	lf <- lf %>%
	# 		fitBounds(gt$shape.bbx[1], gt$shape.bbx[2], gt$shape.bbx[3], gt$shape.bbx[4]) %>%
	# 		addMarkers(gt$shape.center[1], gt$shape.center[2])
	# }
	
	if (is.logical(gt$set.bounds) && !is.null(lf$x$limits)) {
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
	if (length(gt$set.view) == 1 && !is.na(gt$set.view[1])) {
		gt$set.view <- c(mean(lims[c(1,3)]), mean(lims[c(2,4)]), gt$set.view)
	}
	
	if (!is.na(gt$set.view[1]) && !gt$global_bbox_specified) {
		set.view <- gt$set.view
		
		if (!is.null(names(set.view))) {
			if (!setequal(names(set.view), c("lon", "lat", "zoom"))) stop("Incorrect set.view names. They should be \"lon\", \"lat\", and \"zoom\"", call. = FALSE)
			set.view <- unname(set.view[c("lon", "lat", "zoom")])
		}
		
		lf <- lf %>% setView(set.view[1], set.view[2], set.view[3])
	} else {
		bbx <- unname(gt$bbox)
		if (!is.null(bbx)) lf <- lf %>% fitBounds(bbx[1], bbx[2], bbx[3], bbx[4]) #setView(view[1], view[2], view[3])
	}
	
	if (!is.null(gt$center)) lf <- lf %>% addMarkers(gt$center[1], gt$center[2])
	
	lf
}

format_popups <- function(id=NULL, titles, format, values) {
	isnull <- vapply(values, is.null, logical(1))
	
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
	
	titles_format <- vapply(titles, htmlEscape, character(1))
	values_format <- mapply(function(v, f) {
		if (inherits(v, "units")) {
			popup_append <- paste0(" ", as.character(attr(v, "units")))
		} else {
			popup_append <- ""
		}
		numbers <- htmlEscape(if (is.numeric(v)) do.call("fancy_breaks", c(list(vec=as.numeric(v), intervals=FALSE), f)) else v)
		paste0(numbers, popup_append)
	}, values, format, SIMPLIFY = FALSE)
	
	
	labels2 <- mapply(function(l, v) {
		paste0("<tr><td style=\"color: #888888;\">", l, "</td><td align=\"right\"><nobr>", v, "</nobr></td>")
	}, titles_format, values_format, SIMPLIFY=FALSE)
	
	labels3 <- paste0(do.call("paste", c(labels2, list(sep="</tr>"))), "</tr>")
	
	padding_right <- ifelse(length(titles_format) > 13, 15, 0) # add padding for horizontal scroll bar. These will appear on most browsers when there are over 13 normal lines (tested: RStudio, FF, Chrome)

	x <- paste0("<style> div.leaflet-popup-content {width:auto !important;overflow-y:auto; overflow-x:hidden;}</style><div style=\"max-height:25em;padding-right:", padding_right, "px;\"><table>
			   <thead><tr><th colspan=\"2\">", labels, "</th></thead></tr>", labels3, "</table></div>")
	
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
		popups <- format_popups(dt[[gpl[[var_names]]]], gpl[[var_vars]], gpl[[var_format]], dt[, gpl[[var_vars]], drop=FALSE])
	}
	popups
}

add_legend <- function(map, gpl, gt, aes, alpha, group, list.only=FALSE, zindex = NULL) {
	pal_name <- paste(aes, "legend.palette", sep=".")
	val_name <- paste(aes, "legend.values", sep=".")
	lab_name <- paste(aes, "legend.labels", sep=".")
	
	pal <- gpl[[pal_name]]
	val <- gpl[[val_name]]
	lab <- gpl[[lab_name]]
	
	if (!is.null(zindex)) {
		layerId <- legendName(zindex)
	} else {
		layerId <- NULL
	}
	
	if (nchar(pal[1])>10) {
		# check whether style is continuous
		style <- attr(pal, "style")
		is.cont <- TRUE
		incl.na <- nchar(pal[length(pal)]) < 10
		
		orig <- unlist(lapply(pal, function(x) {
			p <- strsplit(x, split = "-", fixed=TRUE)[[1]]
			if (length(p) == 1) NULL else p[p!="NA"]
		}))
		
		
		pal <- vapply(pal, function(x) {
			p <- strsplit(x, split = "-", fixed=TRUE)[[1]]
			if (length(p)==1) p[1] else if (p[6]=="NA") p[5] else p[6]
		}, character(1))
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
		orig <- pal
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
		if (!is.na(colNA)) orig <- c(orig, colNA)
		return(list(col=orig, opacity=opacity))
	}
	
	title_name <- paste(aes, "legend.title", sep=".")
	
	title <- if (nonempty_text(gpl[[title_name]])) expr_to_char(gpl[[title_name]]) else NULL

	legend.position <-gt$view.legend.position

	if (is.cont) {
		legvals <- if (!is.na(colNA)) c(val, NA) else val

		if (style=="quantile") {
			addLegend(map, position=legend.position, group = group,
					  pal=colorQuantile(col, val, na.color=colNA, alpha = FALSE), values=legvals, na.label = textNA, title=title, opacity=opacity, layerId = layerId)
		} else {
			addLegend(map, position=legend.position, group = group,
					  pal=colorNumeric(col, val, na.color=colNA, alpha = FALSE), values=legvals, na.label = textNA, title=title, opacity=opacity, layerId = layerId)
		}
	} else {
		if (allNAs) {
			addLegend(map, position=legend.position, group = group, colors=colNA, labels=textNA, title=title, opacity=opacity, layerId = layerId)
		} else {
			if (!is.na(colNA)) {
				legvals <- c(lab, textNA)
				col <- c(col, colNA)
			} else {
				legvals <- lab
			}
			addLegend(map, position=legend.position,
					  group = group,
					  colors = col,
					  labels = legvals,
					  title=title,
					  opacity=opacity,
					  layerId = layerId)
			
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

# submit_labels <- function(labels, cls, e) {
# 	
# 	layerIds <- get("layerIds", envir = e)
# 
# 	labels_all <- layerIds[[cls]]
# 	
# 	pos <- length(labels_all)
# 	
# 	labels_all <- make.names(c(labels_all, labels), unique = TRUE)
# 	
# 	labels <- labels_all[(pos + 1): length(labels_all)]	
# 
# 	layerIds[[cls]] <- labels_all
# 	assign("layerIds", layerIds, envir = e)
# 	labels
# }

submit_labels <- function(labels, cls, pane, group_name, e) {
	
	layerIds <- get("layerIds", envir = e)
	
	
	types <- attr(layerIds, "types")
	groups <- attr(layerIds, "groups")
	
	labels_all <- unlist(layerIds)
	
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
