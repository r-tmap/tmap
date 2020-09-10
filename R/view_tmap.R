view_tmap <- function(gp, shps=NULL, leaflet_id=1, showWarns=TRUE, gal = NULL, in.shiny = FALSE, lf = NULL) {
	
	gt <- gp$tm_layout
	gp$tm_layout <- NULL
	
	proxy <- !is.null(lf)
	
	leaflet_opts <- do.call(leaflet::leafletOptions, c(list(crs=gt$projection), gt$leaflet.options))
	if (!is.na(gt$set.zoom.limits[1])) leaflet_opts$minZoom <- gt$set.zoom.limits[1]
	if (!is.na(gt$set.zoom.limits[2])) leaflet_opts$maxZoom <- gt$set.zoom.limits[2]
	
	if (!proxy) lf <- leaflet(options = leaflet_opts)

	# add background overlay
	if (!in.shiny) {
		lf <- appendContent(lf, {
			tags$head(
				tags$style(HTML(paste(".leaflet-container {background:", gt$bg.color, ";}", sep="")))
			)	
		})
	}
	
	e <- environment()
	alpha <- gt$alpha

	gt$global_bbox_specified <- !is.null(gt$bbox)
	
	if (!gt$global_bbox_specified) {
		gt$bbox <- attr(shps[[gt$shape.masterID]], "bbox")
	}

	warns <- c(symbol=FALSE, text=FALSE, raster=FALSE, symbol_legend=FALSE, linelwd_legend=FALSE) # to prevent a warning for each shape
	
	if (inherits(shps, c("sf", "stars"))) shps <- list(shps)

	bases <- if ("bases" %in% ls(envir = .TMAP_CACHE)) get("bases", envir = .TMAP_CACHE) else NA
	overlays <- if ("overlays" %in% ls(envir = .TMAP_CACHE)) get("overlays", envir = .TMAP_CACHE) else NA
	overlays_tiles <- if ("overlays_tiles" %in% ls(envir = .TMAP_CACHE)) get("overlays_tiles", envir = .TMAP_CACHE) else character(0)
	
	if (proxy && ("layerIdsNew" %in% ls(envir = .TMAP_CACHE))) {
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
	zids_vec <- unlist(zids, use.names = FALSE)
	
	# For tmapProxy: only use pane with a higher z number than existing ones
	# Only use free panes: every layer must be in a different pane
	z_free <- setdiff(start_pane_id:(start_pane_id+length(zids_vec)*2-1), na.omit(zids_vec))
	zids_vec[is.na(zids_vec)] <- rep(z_free, length.out = sum(is.na(zids_vec)))
	zids_len <- sapply(zids, length)
	zindices <- split(zids_vec, unlist(mapply(rep, 1:length(zids), each = zids_len, SIMPLIFY = FALSE), use.names = FALSE))
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
			
			if (is.null(gpl$fill.group)) {
				group_name <- NULL
			} else {
				group_name <- if (is.na(gpl$fill.group)) {
					shp_name 
				} else {
					gpl$fill.group
				}
				addOverlayGroup(group_name)
			}
			
			pane <- paneName(zi)

			shp$tmapID <- if (!is.null(labels)) as.character(labels) else shp$tmapID
			shp$tmapID2 <- submit_labels(shp$tmapID, "polygons", pane, group_name, e)
			
			suppressWarnings({
				if (is.null(labels)) {
					lf <- lf %>% addPolygons(data=shp, layerId = shp$tmapID2, stroke=stroke, weight=gpl$lwd, color=bcol, fillColor = fcol, opacity=bopacity, fillOpacity = fopacity, dashArray = dashArray, popup = popups, options = pathOptions(interactive = gpl$fill.interactive, pane=pane), group=group_name)	
				} else {
					lf <- lf %>% addPolygons(data=shp, label = ~tmapID, layerId = shp$tmapID2, stroke=stroke, weight=gpl$lwd, color=bcol, fillColor = fcol, opacity=bopacity, fillOpacity = fopacity, dashArray = dashArray, popup = popups, options = pathOptions(interactive = gpl$fill.interactive, pane=pane), group=group_name)	
				}
				
			})
			
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
			

			if (is.null(gpl$line.group)) {
				group_name <- NULL
			} else {
				group_name <- if (is.na(gpl$line.group)) {
					shp_name 
				} else {
					gpl$line.group
				}
				addOverlayGroup(group_name)
			}

			pane <- paneName(zi)

			shp$tmapID <- if (!is.null(labels)) as.character(labels) else shp$tmapID
			shp$tmapID2 <- submit_labels(shp$tmapID, "lines", pane, group_name, e)
			
			suppressWarnings({
				if (is.null(labels)) {
					lf <- lf %>% addPolylines(data=shp, layerId = shp$tmapID2, stroke=TRUE, weight=gpl$line.lwd, color=lcol, opacity = lopacity, popup = popups, options = pathOptions(interactive = gpl$line.interactive, pane=pane), dashArray=dashArray, group=group_name) 
				} else {
					lf <- lf %>% addPolylines(data=shp, label = ~tmapID, layerId = shp$tmapID2, stroke=TRUE, weight=gpl$line.lwd, color=lcol, opacity = lopacity, popup = popups, options = pathOptions(interactive = gpl$line.interactive, pane=pane), dashArray=dashArray, group=group_name) 	
				}
				
			})

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
			
			popups <- get_popups(gpl, type="symbol")
			labels <- as.character(get_labels(gpl, type="symbol"))
			
			
			if (!all(sel)) {
				co <- co[sel, , drop=FALSE]
				fcol <- fcol[sel]
				fopacity <- fopacity[sel]
				symbol.size <- symbol.size[sel]
				symbol.shape <- symbol.shape[sel]
				popups = popups[sel]
				labels = labels[sel]
			}
			
			if (gpl$symbol.misc$symbol.are.markers) {
				if (is.na(gpl$symbol.names)) {
					gpl$data$MARKER__TEXT <- gpl$text 
					gpl$symbol.names <- "MARKER__TEXT"
				}
			}

			pane <- paneName(zi)
			
			if (is.null(gpl$symbol.group)) {
				group_name <- NULL
			} else {
				group_name <- if (is.na(gpl$symbol.group)) {
					shp_name 
				} else {
					gpl$symbol.group
				}
				addOverlayGroup(group_name)
			}

			ids <- submit_labels(labels, "symbols", pane, group_name, e)
			


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
				
				suppressWarnings({
					lf <- lf %>% addMarkers(lng = co2[,1], lat=co2[,2], popup=popups2, label = labels2, layerId = ids2, group=group_name, icon=icons, clusterOptions=clustering, options = markerOptions(interactive = gpl$symbol.interactive, pane=pane))
				})
			} else {
				if (!all(symbol.shape2 %in% c(1, 16, 19, 20, 21))) {
					warns["symbol"] <- TRUE
					assign("warns", warns, envir = e)
				}
				
				suppressWarnings({
					if (fixed) {
						lf <- lf %>% addCircleMarkers(lng=co2[,1], lat=co2[,2], label = labels2, layerId = ids2, fill = any(!is.na(fcol2)), fillColor = fcol2, fillOpacity=fopacity, color = bcol, stroke = !is.na(bcol) && bopacity!=0, radius = 20*symbol.size2, weight = gpl$symbol.border.lwd, popup=popups2, group=group_name, clusterOptions=clustering, options = pathOptions(interactive = gpl$symbol.interactive, pane=pane))
					} else {
						lf <- lf %>% addCircles(lng=co2[,1], lat=co2[,2], label = labels2, layerId = ids2, fill = any(!is.na(fcol2)), fillColor = fcol2, fillOpacity=fopacity, color = bcol, stroke = !is.na(bcol) && bopacity!=0, radius=rad, weight =gpl$symbol.border.lwd, popup=popups2, group=group_name, options = pathOptions(interactive = gpl$symbol.interactive, pane=pane))
					}
				})
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

			npol <- nrow(co)
			text <- gpl$text
			col <- unname(gpl$text.color)
			size <- unname(gpl$text.size)
			
			opacity <- gpl$text.alpha
			
			
			co[, 1] <- co[, 1] + gpl$text.xmod * bpl
			co[, 2] <- co[, 2] + gpl$text.ymod * bpl
			
			# return NULL is no symbols are selected (see tm_facets example)
			if (!any(gpl$text_sel)) return(FALSE)
			
			labels = get_labels(gpl, type="text")

			
			if (!all(gpl$text_sel)) {
				co <- co[gpl$text_sel, , drop=FALSE]
				text <- text[gpl$text_sel]
				col <- col[gpl$text_sel]
				size <- size[gpl$text_sel]
				labels = labels[gpl$text_sel]
			}
			
			
			
			sizeChar <- paste(round(size * 12), "px", sep="")
			colsize <- paste(col, sizeChar, sep="_^_")
			
			direction <- ifelse(gpl$text.just == "left", "right",
						 ifelse(gpl$text.just == "right", "left",
						 ifelse(gpl$text.just == "top", "bottom",
						 ifelse(gpl$text.just == "bottom", "top", "center"))))
			
				
			cs_set <- unique(colsize)
			
			clustering <- gpl$text.misc$clustering
			
			if (is.null(gpl$text.group)) {
				group_name <- NULL
			} else {
				group_name <- if (is.na(gpl$text.group)) {
					shp_name 
				} else {
					gpl$text.group
				}
				addOverlayGroup(group_name)
			}
			
			pane <- paneName(zi)

			ids <- submit_labels(labels, "text", pane, group_name, e)
			
			
			suppressWarnings({
				if (length(cs_set)==1) {
					lf <- lf %>% addLabelOnlyMarkers(lng = co[,1], lat = co[,2], label=text,
													 group=group_name, 
													 layerId = ids, 
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
														 layerId = ids[i], 
														 labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, direction = direction, 
														 							opacity=opacity,
														 							textsize=sizeChar[i],
														 							style=list(color=col[i])),
														 clusterOptions = clustering,
														 options = markerOptions(pane = pane))	
					}
				}
			})
			
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
			
			if (is.null(gpl$raster.group)) {
				group_name <- NULL
			} else {
				group_name <- if (is.na(gpl$raster.group)) {
					shp_name 
				} else {
					gpl$raster.group
				}
				addOverlayGroup(group_name)
			}
			
			pal <- na.omit(unique(gpl$raster))
			pal <- pal[substr(pal, 8,10)!="00"] ## remove transparant colors
			
			pane <- paneName(zi)
			
			layerId <- submit_labels(pane, "raster", pane, group_name, e)
			
			
			col_ids <- match(gpl$raster, pal)

			if (!is_regular_grid(shp) || has_rotate_or_shear(shp)) {
				shp <- sf::st_transform(sf::st_as_sf(shp), crs = 4326)
				
				res <- split_alpha_channel(pal, alpha)
				pal_col <- res$col
				pal_opacity <- if (length(res$opacity) == 0L) 0 else max(res$opacity)
				
				pal_col2 <- pal_col[col_ids]
				
				# TO DO: add layerId = layerId, was 1 ("tmap401"), but should be number of polygons
				lf <- lf %>% addPolygons(data=shp, stroke=FALSE, weight=0, color=NULL, fillColor = pal_col2, opacity=0, fillOpacity = pal_opacity, popup = NULL, options = pathOptions(interactive=FALSE, pane=pane), group=group_name)
				
			} else {
				shp[[1]] <- matrix(col_ids, ncol = ncol(shp))
				
				res <- split_alpha_channel(pal, alpha)
				pal_col <- res$col
				pal_opacity <- if (length(res$opacity) == 0L) 0 else max(res$opacity)
				
				
				lf <- lf %>% leafem::addStarsImage(shp, band = 1, colors = pal_col, opacity = pal_opacity, group = group_name, project = FALSE, layerId = layerId)
			}
			

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


			# add base layer(s)
			if (length(basemaps)) {
				for (i in 1:length(basemaps)) {
					bm <- unname(basemaps[i])
					bmname <- unname(group_names[i])
					
					if (bm %in% names(providers)) {
						lf <- lf %>% addProviderTiles(bm, group=bmname, options = tileOptions[[i]])
					} else {
						if (substr(bm, 1, 4) != "http" && gt$show.warnings) warning("basemap ", bm, "does not exist in the providers list nor does it seem a valid url", call. = FALSE)
						lf <- lf %>% addTiles(bm, group=bmname, options=tileOptions[[i]])
					}
				}
			}
			
			#lf = lf %>% addTiles()
			assign("lf", lf, envir = e)

			TRUE
		}
		
		e2 <- environment()
		
		fnames <- paste("plot", gpl$plot.order, sep="_")

		layer_selection <- unlist(mapply(function(fn, zi) {
			do.call(fn, list(zi = zi), envir = e2)
		}, fnames, zindex, SIMPLIFY = FALSE), use.names = FALSE)
			
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
				nitems <- length(gali$labels)
				revfun <- if (gali$reverse) rev else function(x)x
				palette_colors <- revfun(if (is.null(gali$col)) rep("grey50", nitems) else rep(gali$col, length.out=nitems))
				legend.palette <- do.call("process_color", c(list(col=palette_colors, alpha = gali$alpha), gt$pc))
				
				
				RGBA <- col2rgb(legend.palette, alpha = TRUE)
				col <- rgb(RGBA[1,], RGBA[2,], RGBA[3,], maxColorValue = 255)
				opacity <- unname(RGBA[4,1]/255) * alpha
				
				if (!is.null(gali$zindex)) {
					layerId <- legendName(gali$zindex)
				} else {
					layerId <- NULL
				}
				
				lf <- lf %>% addLegend(position=gt$view.legend.position,
									   group = gali$group,
									   colors = col,
									   labels = gali$labels,
									   title=gali$title, 
									   opacity=opacity,
									   layerId = layerId)
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
	
	if (!proxy) lf <- view_set_bounds(lf, gt)
	if (gt$mouse.show) lf = lf %>% leafem::addMouseCoordinates()
	
	lf$title <- gt$title
	
	assign("layerIds", layerIds, envir = .TMAP_CACHE)
	assign("bases", bases, envir = .TMAP_CACHE)
	assign("overlays", overlays, envir = .TMAP_CACHE)
	assign("overlays_tiles", overlays_tiles, envir = .TMAP_CACHE)
	
	lf	
}
