print_tmap <- function(x, vp=NULL, return.asp=FALSE, mode=getOption("tmap.mode"), show=TRUE, knit=FALSE, options=NULL, interactive_titles = TRUE, in.shiny = FALSE, lf = NULL, ...) {
	args <- list(...)
	scale.extra <- NULL
	title.snap.to.legend <- NULL
	
	proxy <- !is.null(lf)
	in.shiny <- in.shiny || proxy
	
	interactive <- (mode == "view") || proxy
	
	tmapOptions <- get("tmapOptions", envir = .TMAP_CACHE)
	
	show.messages <- tmapOptions$show.messages
	
	qtm_shortcut <- attr(x, "qtm_shortcut")
	
	if (!is.null(qtm_shortcut) && !proxy) {
		if (qtm_shortcut) {
			if (!interactive) {
				if (show.messages) message("Switching to view mode. Run tmap_mode(\"plot\") or simply ttm() to switch back to plot mode.")
				options(tmap.mode="view")
				interactive <- TRUE
			}
			
			if (!("tm_minimap" %in% names(x)) && !identical(tmapOptions$qtm.minimap, FALSE)) x <- c(x, tm_minimap())
		}
		if (interactive && !("tm_scale_bar" %in% names(x)) && tmapOptions$qtm.scalebar) x <- c(x, tm_scale_bar())
		if (!qtm_shortcut && interactive && !("tm_minimap" %in% names(x)) && identical(tmapOptions$qtm.minimap, TRUE)) x <- c(x, tm_minimap())
	}
	
	# reset symbol shape / shape just/anchor lists
	assign("shapeLib", list(), envir = .TMAP_CACHE)
	assign("justLib", list(), envir = .TMAP_CACHE)


	## process proxy
	if (proxy) {
		layerIds <- if (".layerIdsNew" %in% ls(envir = .TMAP_CACHE)) {
			get("layerIdsNew", envir = .TMAP_CACHE)
		} else {
			get("layerIds", envir = .TMAP_CACHE)
		}
		assign("layerIds", layerIds, envir = .TMAP_CACHE)
		

		overlays <- get("overlays", envir = .TMAP_CACHE)
		overlays_tiles <- get("overlays_tiles", envir = .TMAP_CACHE)
		
		#browser()
		
		typesList <- as.list(attr(layerIds, "types"))
		names(typesList) <- names(layerIds)
		
		groupsList <- as.list(attr(layerIds, "groups"))
		names(groupsList) <- names(layerIds)
		
		rem_lay_id <- which(names(x) == "tm_remove_layer")
		if (length(rem_lay_id) > 0L) {
			for (id in rem_lay_id) {
				z <- x[[id]]$zindex
				name <- paneName(z)
				legend <- legendName(z)
				
				if (typesList[[name]] == "raster") {
					lf <- lf %>% leaflet::removeImage(sort(unname(layerIds[[name]]))) %>%
						leaflet::removeControl(legend)
				} else {
					lf <- lf %>% leaflet::removeShape(sort(unname(layerIds[[name]]))) %>%
						leaflet::removeControl(legend)
				}
				
				layerIds[[name]] <- NULL
				typesList[[name]] <- NULL
				groupsList[[name]] <- NULL
			}
			attr(layerIds, "types") <- unlist(typesList, use.names = FALSE)
			attr(layerIds, "groups") <- unlist(groupsList, use.names = FALSE)
			assign("layerIdsNew", layerIds, envir = .TMAP_CACHE)
			
			overlays <- if (length(groupsList) == 0) character(0) else intersect(overlays, unlist(groupsList, use.names = FALSE))
			overlays_tiles <- if (length(groupsList) == 0) character(0) else intersect(overlays_tiles, unlist(groupsList, use.names = FALSE))
			assign("overlays", overlays, envir = .TMAP_CACHE)
			assign("overlays_tiles", overlays_tiles, envir = .TMAP_CACHE)
		}
		x <- x[!(names(x) %in% c("tm_remove_layer"))]
		if (length(x) == 0) {
			return(lf)
		}
	} else {
		suppressWarnings(rm(list = c("bases", "overlays", "overlays_tiles"), envir = .TMAP_CACHE))
	}
	
		
	
	x <- prearrange_element_order(x, add.basemap = !(is.null(tmapOptions$basemaps) || proxy), add.overlay = !(is.null(tmapOptions$overlays) || proxy))
	
	if (!any(names(x) %in% c("tm_fill", "tm_borders", "tm_lines", "tm_symbols", "tm_raster", "tm_text"))) {
		lf <- print_shortcut(x, interactive, in.shiny, args, knit)
		if (knit) {
			return(do.call("knit_print", c(list(x=lf), args, list(options=options))))
			#return(knit_print(lf, ..., options=options))
		} else {
			return(print(lf))
		}
	}
	
		
	## remove non-supported elements if interactive
	if (interactive) x <- x[supported_elem_view_mode(names(x))]
	
	
	## gather shape info
	gm <- gather_shape_info(x, interactive)

	## split data.frames from shape/raster objects, and determine shape types
	shps_dts <- mapply(preprocess_shapes, x[gm$shape.id], gm$shape.raster_facets_vars, MoreArgs = list(gm=gm, interactive=interactive), SIMPLIFY = FALSE)
	shps <- lapply(shps_dts, "[[", 1)
	datasets <- lapply(shps_dts, "[[", 2)
	types <- lapply(shps_dts, "[[", 3)

	## determine bounding box and aspect ratio of master shape
	mshp <- shps[[gm$shape.masterID]]
	gm$shape.bbx_cropped <- attr(mshp, "bbox")
	
	gm$shape.masp <-	get_asp_ratio(gm$shape.bbx_cropped, is.projected=attr(mshp, "projected"))


	# split x and datasets into multiple layers if shape(s) are geometrycollection 
	res <- order_x(x, shps, datasets, types, gm)

	x <- res$x
	gm <- res$gm
	shps <- res$shps
	
	datasets <- lapply(x[names(x)=="tm_shape"], "[[", "data")
	
	#datasets <- res$datasets
	
	## prepare viewport (needed to determine asp_ratio for facet layout)
	
	gt <- preprocess_gt(x, interactive=interactive, orig_crs = gm$shape.orig_crs)
	
	gm  <- c(gm, prepare_vp(vp, gm, interactive, gt))

	## process tm objects
	#  - get all non-layer elements, (tm_layout, tm_grid, ...)
	#  - process layer elements by calling process_layers (result is gp)
	#  		- determine grouped small multiples (specified by user with tm_facets(by=))
	#       - process layer functions by calling indivudual functions, like tm_fill
	#  - determine number of small multiples (nx)
	#  - process non-layer elements by calling process_meta (result is gmeta):
	#  		- determines number of rows and colums for small multiples
	#       - applies scale factor to all meta elements (tm_layout, tm_style, tm_compass, tm_scale_bar, tm_grid)
	#  - split gp into small multiples (result is gps)
	# si <- x[[s$shape.id[s$masterID]]]
	# si$data <- NULL
	# 
	# s <- c(s, si)
	
	
	result <- process_tm(x, gt, gm, interactive)
	gm <- c(result$gmeta, gm)
	gps <- result$gps
	gal <- result$gal
	nx <- result$nx
	nxl <- result$nxl
	data_by <- result$data_by
	
	gm$shape.shps_lengths <- vapply(datasets, function(d) if (is.null(d)) 0L else nrow(d), integer(1))

	## arranges aspect ratios, the grid layout of the map/facets, etc
	gm <- c(gm, determine_asp_ratios(gm, interactive))
	
	## process shapes (bbox and crop)
	shps <- process_shapes(shps, x[gm$shape.id], gm, data_by, allow.crop = FALSE, interactive=interactive) # allow.crop was !interactive
	gm <- c(gm, attr(shps, "info"))

	## further arranges the grid layout of the map/facets
	if (!interactive) gm <- process_facet_layout(gm)

	## check whether small multiples are split to layers
	as.layers <- (nx >= 2) && gm$as.layers && interactive

	if (in.shiny && !as.layers && nx > 1) {
		stop("Small multiples (facets) are not supported in Shiny. Workarounds: create multiple independent maps or specify as.layers = TRUE in tm_facets", call. = FALSE)
	}
		
		
	## create external legend and attributes objects
	g <- process_gps(gps, shps, x, gm, nx, nxl, interactive, return.asp)
	## return in case g is a number, i.e. the aspect ratio
	if (is.numeric(g)) return(g)
	
	shps <- g$shps
	nx <- g$nx
	

	## multiple datasets for each layer (when as.layers=TRUE in tm_facets)
	if (as.layers) {
		datasets <- datasets[g$layerids]
		gm$shp_name <- gm$shp_name[g$layerids]
	}
	
	## adds data to gps (needed for view mode)
	gps2 <- add_data_to_gps(g$gps, gm, datasets, g$matchIDs, interactive)
	
	## plot
	if (interactive) {
		sync <- gm$sync
		if (is.na(sync)) sync <- gm$shape.same_bbx
		
		lVargs <- list(ncol=gm$ncol,
					   sync=ifelse(sync, "all", "none"),
					   sync.cursor=sync,
					   no.initial.sync = TRUE)
		
		multi_shapes <- is.list(shps[[1]]) && !inherits(shps[[1]], "sf")
		showWarns <- c(TRUE, rep(FALSE, length(gps)-1))

		if (multi_shapes) {
			lfs <- mapply(view_tmap, gps2[1:nx], shps[1:nx], leaflet_id=1:nx, showWarns=showWarns, MoreArgs = list(gal = gal, in.shiny = in.shiny, lf = lf), SIMPLIFY = FALSE)
		} else {
			lfs <- mapply(view_tmap, gps2[1:nx], leaflet_id=1:nx, showWarns=showWarns, MoreArgs = list(shps=shps, gal = gal, in.shiny = in.shiny, lf = lf), SIMPLIFY = FALSE)
		}
		lf <- if (nx==1) lfs[[1]] else lfmv <- do.call(leafsync::latticeView, c(lfs, lVargs))
		
		lf2 <- if (interactive_titles) add_leaflet_titles(lf) else lf
		
		if (show) {
			save_last_map()
			if (knit) {
				kp <- get("knit_print", asNamespace("knitr"))
				return(do.call(kp, c(list(x=lf2), args, list(options=options))))
			} else {
				return(print(lf2))
			}
		} else lf2
	} else {
		if (show) {
			if (nx > 1) sasp <- gm$shape.dasp
			#  gridplot:   - makes small multiples grid
			#              - calls plot_all for grob trees
			#              - plot the grob trees
			#  plot_all:   - calls plot_map to create grob tree of map itself
			#              - calls legend_prepare and plot_legend to create grob tree of legend
			#              - creates grob tree for whole plot
			gridplot(gm, "plot_all", nx, g$gps, gal, shps, gm$shape.dasp, gm$shape.sasp, gm$shape.inner.margins, gm$shape.legend_pos, g$gp_leg, g$gp_attr)
			## if vp is specified, go 1 viewport up, else go to root viewport
			upViewport(n=as.integer(!is.null(vp)))
			save_last_map()
			invisible(list(shps=shps, gps=gps2))
		} else {
			list(shps=shps, gps=gps2)
		}
	}
}

supported_elem_view_mode <- function(nms) {
	if (get("tmapOptions", envir = .TMAP_CACHE)$show.messages) {
		if (any(nms=="tm_credits")) message("Credits not supported in view mode.")
		if (any(nms=="tm_logo")) message("Logo not supported in view mode.")
		if (any(nms=="tm_compass")) message("Compass not supported in view mode.")
		if (any(nms=="tm_xlab")) message("X-axis label not supported in view mode.")
		if (any(nms=="tm_ylab")) message("Y-axis label not supported in view mode.")
	}
	which(!(nms %in% c("tm_credits", "tm_logo", "tm_compass", "tm_xlab", "tm_ylab")))
}
