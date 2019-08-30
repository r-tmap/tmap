#' Draw thematic map
#' 
#' Draw thematic map. If the tmap mode is set to \code{"plot"} (see \code{\link{tmap_mode}}), the map is plot in the current graphics device. If the mode is set to \code{"view"}, the map is shown interactively as an htmlwidget.
#' 
#' @param x tmap object. A tmap object is created with \code{\link{qtm}} or by stacking \code{\link{tmap-element}}s.
#' @param vp \code{\link[grid:viewport]{viewport}} to draw the plot in. This is particularly useful for insets.
#' @param return.asp Logical that determines whether the aspect ratio of the map is returned. In that case, \code{\link[grid:grid.newpage]{grid.newpage()}} will be called, but without plotting of the map. This is used by \code{\link{tmap_save}} to determine the aspect ratio of the map.
#' @param mode the mode of tmap: \code{"plot"} (static) or \code{"view"} (interactive). See \code{\link{tmap_mode}} for details.
#' @param show logical that determines whether to show to map. Obviously \code{TRUE} by default, but \code{show=FALSE} can be useful for just obtaining the returned objects.
#' @param knit should \code{\link[knitr:knit_print]{knit_print}} be enabled, or the normal \code{\link[base:print]{print}} function?
#' @param options options passed on to knitprint
#' @param ... not used
#' @return If \code{mode=="plot"}, then a list is returned with the processed shapes and the metadata. If \code{mode=="view"}, a \code{\link[leaflet:leaflet]{leaflet}} object is returned (see also \code{\link{tmap_leaflet}})
#' @import tmaptools
#' @import sf
#' @importFrom units set_units as_units
#' @importFrom raster raster brick extent setValues ncell couldBeLonLat fromDisk crop projectRaster projectExtent colortable nlayers minValue maxValue getValues
#' @importMethodsFrom raster as.vector
#' @import RColorBrewer
#' @importFrom viridisLite viridis
#' @import grid
#' @import methods
#' @importFrom graphics par
#' @importFrom classInt classIntervals findCols
#' @importFrom grDevices col2rgb colorRampPalette dev.off dev.set dev.cur is.raster png rgb dev.size
#' @importFrom stats na.omit dnorm fft quantile rnorm runif 
#' @importFrom grDevices xy.coords colors
#' @importFrom utils capture.output data download.file head setTxtProgressBar tail txtProgressBar
#' @importMethodsFrom raster as.vector
#' @import leaflet
#' @importFrom htmlwidgets appendContent onRender
#' @importFrom htmltools tags HTML htmlEscape
#' @importFrom lwgeom st_make_valid
#' @import leafsync
#' @importFrom utils packageVersion
#' @export
#' @method print tmap
print.tmap <- function(x, vp=NULL, return.asp=FALSE, mode=getOption("tmap.mode"), show=TRUE, knit=FALSE, options=NULL, ...) {
	print_tmap(x=x, vp=vp, return.asp=return.asp, mode=mode, show=show, knit=knit, options=options, ...)
}

#' @rdname print.tmap
#' @rawNamespace
#' if(getRversion() >= "3.6.0") {
#'   S3method(knitr::knit_print, tmap)
#' } else {
#'   export(knit_print.tmap)
#' }
knit_print.tmap <- function(x, ..., options=NULL) {
	# @importFrom knitr knit_print
	print_tmap(x, knit=TRUE, options=options, ...)
}

#' Create a leaflet widget from a tmap object
#' 
#' Create a leaflet widget from a tmap object. An interactive map (see \code{\link{tmap_mode}}) is an automatically generated leaflet widget. With this function, this leaflet widget is obtained, which can then be changed or extended by using leaflet's own methods.
#'  
#' @param x tmap object. A tmap object is created with \code{\link{qtm}} or by stacking \code{\link{tmap-element}}s.
#' @param mode the mode of tmap, which is set to \code{"view"} in order to obtain the leaflet object. See \code{\link{tmap_mode}} for details.
#' @param show should the leaflet map be shown? \code{FALSE} by default
#' @param add.titles add titles to leaflet object
#' @param in.shiny is the leaflet output going to be used in shiny? If so, two features are not supported and therefore disabled: facets and colored backgrounds.
#' @param ... arguments passed on to \code{\link{print.tmap}}
#' @return \code{\link[leaflet:leaflet]{leaflet}} object
#' @example ./examples/tmap_leaflet.R
#' @seealso \code{\link{tmapOutput}} for tmap in Shiny, \code{\link{tmap_mode}}, \code{\link{tm_view}}, \code{\link{print.tmap}}
#' @export
tmap_leaflet <- function(x, mode="view", show = FALSE, add.titles = TRUE, in.shiny = FALSE, ...) {
  print.tmap(x, mode=mode, show=show, interactive_titles = add.titles, in.shiny = in.shiny, ...)
}

print_shortcut <- function(x, interactive, in.shiny, args, knit) {
	if (getOption("tmap.mode")=="plot") {
		stop("Either specify shp, or set mode to \"view\" with tmap_mode or ttm", call.=FALSE)	
	} else {
		xtiles <- which(names(x) == "tm_tiles")
		
		gt <- preprocess_gt(x, interactive=interactive)
		gt$shp_name <- rep("dummy", length(xtiles))
		gt$shape.units <- list(unit = get("tmapOptions", envir = .TMAP_CACHE)$unit)
		if (is.null(gt$bbox)) gt$bbox <- c(-190, -90, 180, 90)
		
		if (any(names(x) == "tm_scale_bar")) {
			gsbid <- which(names(x) == "tm_scale_bar")[1]
			gsb <- x[[gsbid]]
		} else {
			gsb <- NULL
		}
		gsb <- process_meta_scale_bar(gsb, interactive = TRUE, gt)

		if (any(names(x) == "tm_grid")) {
			ggid <- which(names(x) == "tm_grid")[1]
			gg <- x[[ggid]]
		} else {
			gg <- NULL
		}		
		gg <- process_meta_grid(gg, gt)

		gmmid <- which(names(x)=="tm_minimap")[1]
		gmm <- x[[gmmid]]
		gmm <- process_meta_minimap(gmm, interactive = TRUE, gt)
		
		gt <- c(gt, gsb, gg, gmm)
		
		
		#gt$scale.show <- FALSE
		#gt$shape.bbx <- x$tm_shortcut$bbx
		#gt$shape.center <- x$tm_shortcut$center
		
		x[xtiles] <- lapply(x[xtiles], function(xi) {
			xi <- process_tiles(xi, gt)
			xi$plot.order <- "tm_tiles"
			xi
		})
		
		if (gt$grid.show) x[[xtiles[1]]]$plot.order <- c("tm_tiles", "tm_grid")
		
		
		x[names(x) == "tm_shape"] <- NULL
		
		x <- x[!(names(x) %in% c("tm_layout", "tm_view", "tm_style", "tm_grid", "tm_facets", "tm_credits", "tm_logo", "tm_compass", "tm_scale_bar", "tm_xlab", "tm_ylab", "tm_minimap"))]
		
		x$tm_layout <- gt
		
		view_tmap(x, shps = list(dummy = NULL), in.shiny = in.shiny)
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


gather_shape_info <- function(x, interactive) {
	## identify shape blocks
	shape.id <- which(names(x)=="tm_shape")
	nshps <- length(shape.id)
	if (!nshps) stop("Required tm_shape layer missing.", call. = FALSE)
	
	## find "MAP_COLORING" values
	apply_map_coloring <- if ("tm_fill" %in% names(x)) {
		any(vapply(x[which(names(x)=="tm_fill")], function(i)identical(i$col[1],"MAP_COLORS"), logical(1)))
	} else FALSE
	
	## find master shape
	is_raster <- vapply(x[shape.id], function(xs) {
		!is.null(xs$shp) && inherits(xs$shp, c("Raster", "SpatialPixels", "SpatialGrid"))
	}, logical(1))
	is_master <- vapply(x[shape.id], "[[", logical(1), "is.master")
#	any_raster <- any(is_raster)
	masterID <- if (!length(which(is_master))) {
		which(is.na(is_master))[1]
	} else which(is_master)[1]
	is_raster_master <- is_raster[masterID]
	
	## find master projection (and set to longlat when in view mode)
	master_crs <- get_proj4(x[[shape.id[masterID]]]$projection, output = "crs")
	mshp_raw <- x[[shape.id[masterID]]]$shp
	if (is.null(master_crs)) master_crs <- get_projection(mshp_raw, output = "crs")
	orig_crs <- master_crs # needed for adjusting bbox in process_shapes
	if (interactive) {
		if (is.na(get_projection(mshp_raw, output = "crs")) && tmaptools::is_projected(mshp_raw)) {
			
			stop("The projection of the shape object ", x[[shape.id[masterID]]]$shp_name, " is not known, while it seems to be projected.", call.=FALSE)
		}
		master_crs <- .crs_longlat
	}
	
	## find master bounding box (unprocessed)
	bbx_raw <- bb(mshp_raw)
	
	## get raster and group by variable name (needed for eventual reprojection of raster shapes)
	raster_facets_vars <- lapply(1:nshps, function(i) {
		from <- shape.id[i] + 1
		to <- ifelse(i==nshps, length(x), shape.id[i+1]-1)
		fid <- which(names(x)[from:to]=="tm_facets")
		rid <- which(names(x)[from:to]=="tm_raster")
		
		if (length(rid)) {
			max.value <- x[[from-1+rid[1]]]$max.value
			is.RGB <- x[[from-1+rid[1]]]$is.RGB
			rgb.vars <- x[[from-1+rid[1]]]$rgb.vars
			to.Cat <- x[[from-1+rid[1]]]$style == "cat"
		} else {
			max.value <- NA
			is.RGB <- FALSE
			rgb.vars <- NULL
			to.Cat <- FALSE
		}

		res <- c(if (length(fid)) x[[from-1+fid[1]]]$by else NULL,
				 if (length(rid)) x[[from-1+rid[1]]]$col else NULL)
		if (is.null(res)) res <- NA
		attr(res, "max.value") <- max.value
		attr(res, "is.RGB") <- is.RGB
		attr(res, "rgb.vars") <- rgb.vars
		attr(res, "to.Cat") <- to.Cat
		res
	})
	
	## get arguments related to units (approx_areas)
	unit <- x[[shape.id[masterID]]]$unit
	if (is.null(unit)) unit <- get("tmapOptions", envir = .TMAP_CACHE)$unit
	if (unit == "metric") unit <- "km"
	if (unit == "imperial") unit <- "mi"
	
	# units_args <- x[[shape.id[masterID]]][c("unit", "orig", "to", "total.area")]
	# names(units_args)[names(units_args)=="unit"] <- "target"
	# units_args <- units_args[!sapply(units_args, is.null)]
	
	## get arguments related to bb
	bb_args <- x[[shape.id[masterID]]][intersect(names(x[[shape.id[masterID]]]), c("ext", "cx", "cy", "width", "height", "xlim", "ylim", "relative"))]
	bb_args$x <- x[[shape.id[masterID]]]$bbox
	
	## add other shape arguments
	# point.per <- x[[shape.id[masterID]]]$point.per
	# line.center <- x[[shape.id[masterID]]]$line.center
	
	list(shape.id=shape.id,
		 shape.nshps=nshps,
		 shape.apply_map_coloring=apply_map_coloring,
		 shape.is_raster_master=is_raster_master,
		 shape.masterID=masterID,
		 shape.master_crs=master_crs,
		 shape.orig_crs=orig_crs,
		 shape.bbx_raw=bbx_raw,
		 shape.unit=unit,
		 shape.bb_args=bb_args,
		 # shape.point.per=point.per,
		 # shape.line.center=line.center,
		 shape.raster_facets_vars=raster_facets_vars)
}

prepare_vp <- function(vp, gm, interactive, gt) {
	
	if (interactive) {
		devsize <- dev.size()
		dasp <- devsize[1] / devsize[2]
		iasp <- gm$shape.masp
		asp_ratio <- iasp / dasp
	} else {
		if (is.null(vp)) {
			grid.newpage()
		} else {
			if (is.character(vp)) seekViewport(vp) else pushViewport(vp)
		}
		
		## calculate device aspect ratio (needed for small multiples' nrow and ncol)
		inner.margins <- gt$inner.margins
		inner.margins <- if (is.na(inner.margins[1])) {
			if (gm$shape.is_raster_master) rep(0, 4) else rep(0.02, 4)
		} else rep(inner.margins, length.out=4)
		xmarg <- sum(inner.margins[c(2,4)])
		ymarg <- sum(inner.margins[c(1,3)])
		if (xmarg >= .8) stop("Inner margins too large", call. = FALSE)
		if (ymarg >= .8) stop("Inner margins too large", call. = FALSE)
		iasp <- gm$shape.masp * (1+(xmarg/(1-xmarg))) / (1+(ymarg/(1-ymarg)))
		dasp <- convertWidth(unit(1,"npc"), "inch", valueOnly=TRUE)/convertHeight(unit(1,"npc"), "inch", valueOnly=TRUE) # it may be different than dev.size, since vp can be defined
		asp_ratio <- iasp / dasp
	}
	list(shape.dasp = dasp,
		 shape.asp_ratio = asp_ratio)
}

determine_asp_ratios <- function(gm, interactive) {
	if (interactive) {
		dw <- 1
		dh <- 1
		tasp <- fasp <- 1
		lasp <- NA
		fpi <- NULL
	} else {
		dw <- convertWidth(unit(1-sum(gm$outer.margins[c(2,4)]),"npc"), "inch", valueOnly=TRUE)
		dh <- convertHeight(unit(1-sum(gm$outer.margins[c(1,3)]),"npc"), "inch", valueOnly=TRUE)
		
		fpi <- preprocess_facet_layout(gm, gm$legend.outside, dh, dw)
		
		# aspect ratio of total device
		tasp <- dw/dh
		
		# aspect ratio per facet
		fasp <- fpi$dsw / fpi$dsh #-  fpi$pSH - fpi$between.margin.in)
		
		# aspect ratio per facet minus extern legend
		#lasp <- fasp * (1-fpi$legmarx) / (1-fpi$legmary-fpi$attrmary-fpi$attrmary)
		# !! extern legend already calculated in dsw and dsh in preprocess_facet_layout (see #287)
		lasp <- fasp
	}
	list(shape.dw = dw,
		 shape.dh = dh,
		 shape.tasp = tasp,
		 shape.lasp = lasp,
		 shape.fpi = fpi)
}



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
		
		typesList <- as.list(attr(layerIds, "types"))
		names(typesList) <- names(layerIds)
		
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
			}
			attr(layerIds, "types") <- unlist(typesList)
			assign("layerIdsNew", layerIds, envir = .TMAP_CACHE)
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




add_leaflet_titles <- function(lf) {
	if (inherits(lf, "shiny.tag.list")) {
		ncld <- length(lf[[1]])
		lf[[1]] <- mapply(function(l, i) {
			title <- l$children[[1]]$title
			if (title!="") {
				l$children[[1]] <- l$children[[1]] %>% htmlwidgets::onRender(paste("
					function(el, x) {
						var tldiv = document.getElementsByClassName(\"leaflet-top leaflet-left\")[",i,"];
						var titlediv = document.createElement('div');
						titlediv.className = \"info legend leaflet-control\";
						titlediv.innerHTML = \"<b>", title, "</b>\";
						tldiv.insertBefore(titlediv, tldiv.childNodes[0]);
					}", sep="")
				)
			}
			l
		}, lf[[1]], 0:(ncld-1), SIMPLIFY = FALSE)
	} else {
		title <- lf$title
		if (title!="") {
			lf <- lf %>% htmlwidgets::onRender(paste("
						function(el, x) {
							var tldiv = document.getElementsByClassName(\"leaflet-top leaflet-left\")[0];
							var titlediv = document.createElement('div');
							titlediv.className = \"info legend leaflet-control\";
							titlediv.innerHTML = \"<b>", title, "</b>\";
							tldiv.insertBefore(titlediv, tldiv.childNodes[0]);
						}", sep="")
			)
		}
	}
	lf
}


