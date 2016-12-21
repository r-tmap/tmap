#' Draw thematic map
#' 
#' Draw thematic map. If the tmap mode is set to \code{"plot"} (see \code{\link{tmap_mode}}), the map is plot in the current graphics device. If the mode is set to \code{"view"}, the map is shown interactively as an htmlwidget.
#' 
#' @param x tmap object. A tmap object is created with \code{\link{qtm}} or by stacking \code{\link{tmap-element}}s.
#' @param vp \code{\link[grid:viewport]{viewport}} to draw the plot in. This is particularly useful for insets.
#' @param return.asp Logical that determines whether the aspect ratio of the map is returned. In that case, \code{\link[grid:grid.newpage]{grid.newpage()}} will be called, but without plotting of the map. This is used by \code{\link{save_tmap}} to determine the aspect ratio of the map.
#' @param mode the mode of tmap: \code{"plot"} (static) or \code{"view"} (interactive). See \code{\link{tmap_mode}} for details.
#' @param show logical that determines whether to show to map. Obviously \code{TRUE} by default, but \code{show=FALSE} can be useful for just obtaining the returned objects.
#' @param knit should \code{\link[knitr:knit_print]{knit_print}} be enabled, or the normal \code{\link[base:print]{print}} function?
#' @param options options passed on to knitprint
#' @param ... not used
#' @return If \code{mode=="plot"}, then a list is returned with the processed shapes and the metadata. If \code{mode=="view"}, a \code{\link[leaflet:leaflet]{leaflet}} object is returned (see also \code{\link{tmap_leaflet}})
#' @import tmaptools
#' @import sp
#' @importFrom raster raster brick extent setValues ncell couldBeLonLat fromDisk crop projectRaster projectExtent colortable nlayers minValue maxValue getValues
#' @importMethodsFrom raster as.vector
#' @import RColorBrewer
#' @import grid
#' @import methods
#' @importFrom classInt classIntervals findCols
#' @importFrom rgeos gIntersection gIntersects gBuffer gDifference gCentroid gUnaryUnion
#' @importFrom grDevices col2rgb colorRampPalette dev.off is.raster png rgb
#' @importFrom stats na.omit dnorm fft quantile rnorm runif 
#' @importFrom spdep poly2nb
#' @importFrom grDevices xy.coords colors
#' @importFrom graphics par
#' @importFrom rgdal getPROJ4VersionInfo SGDF2PCT CRSargs
#' @importFrom utils capture.output data download.file head setTxtProgressBar tail txtProgressBar
#' @importMethodsFrom raster as.vector
#' @importFrom geosphere distGeo
#' @import leaflet
#' @importFrom htmlwidgets appendContent
#' @importFrom htmltools tags HTML htmlEscape
#' @importFrom mapview latticeView
#' @export
#' @method print tmap
print.tmap <- function(x, vp=NULL, return.asp=FALSE, mode=getOption("tmap.mode"), show=TRUE, knit=FALSE, options=NULL, ...) {
	print_tmap(x=x, vp=vp, return.asp=return.asp, mode=mode, show=show, knit=knit, options=options, ...)
}

#' @rdname print.tmap
#' @export
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
#' @param ... arguments passed on to \code{\link{print.tmap}}
#' @return \code{\link[leaflet:leaflet]{leaflet}} object
#' @example ../examples/tmap_leaflet.R
#' @seealso \code{\link{tmap_mode}}, \code{\link{tm_view}}, \code{\link{print.tmap}}
#' @export
tmap_leaflet <- function(x, mode="view", show = FALSE, ...) {
  print.tmap(x, mode=mode, show=show, ...)
}

print_shortcut <- function(x, interactive) {
	if (getOption("tmap.mode")=="plot") {
		stop("Either specify shp, or set mode to \"view\" with tmap_mode or ttm", call.=FALSE)	
	} else {
		gt <- preprocess_gt(x, interactive=interactive)
		gt$bbx <- x$tm_shortcut$bbx
		gt$center <- x$tm_shortcut$center
		
		lf <-view_tmap(list(tm_layout=gt))
		
		if (knit) {
			return(do.call("knit_print", c(list(x=lf), list(...), list(options=options))))
			#return(knit_print(lf, ..., options=options))
		} else {
			return(print(lf))
		}
	}
}

supported_elem_view_mode <- function(nms) {
	if (any(nms=="tm_grid")) warning("Grid lines not supported in view mode.", call.=FALSE)
	if (any(nms=="tm_scale_bar")) warning("Scale bar not supported in view mode.", call.=FALSE)
	if (any(nms=="tm_credits")) warning("Credits not supported in view mode.", call.=FALSE)
	if (any(nms=="tm_logo")) warning("Logo not supported in view mode.", call.=FALSE)
	if (any(nms=="tm_compass")) warning("Compass not supported in view mode.", call.=FALSE)
	if (any(nms=="tm_xlab")) warning("X-axis label not supported in view mode.", call.=FALSE)
	if (any(nms=="tm_ylab")) warning("Y-axis label not supported in view mode.", call.=FALSE)
	
	which(!(nms %in% c("tm_grid", "tm_scale_bar", "tm_credits", "tm_logo", "tm_compass", "tm_xlab", "tm_ylab")))
}


gather_shape_info <- function(x, interactive) {
	## identify shape blocks
	shape.id <- which(names(x)=="tm_shape")
	nshps <- length(shape.id)
	if (!nshps) stop("Required tm_shape layer missing.", call. = FALSE)
	
	## find "MAP_COLORING" values
	apply_map_coloring <- if ("tm_fill" %in% names(x)) {
		any(sapply(x[which(names(x)=="tm_fill")], function(i)identical(i$col[1],"MAP_COLORS")))
	} else FALSE
	
	## find master shape
	is_raster <- sapply(x[shape.id], function(xs) {
		inherits(xs$shp, c("Raster", "SpatialPixels", "SpatialGrid"))	
	})
	is_master <- sapply(x[shape.id], "[[", "is.master")
	any_raster <- any(is_raster)
	masterID <- if (!length(which(is_master))) {
		if (any_raster) which(is_raster)[1] else 1
	} else which(is_master)[1]
	
	## find master projection
	master_CRS <- get_proj4(x[[shape.id[masterID]]]$projection, as.CRS = TRUE)
	mshp_raw <- x[[shape.id[masterID]]]$shp
	if (is.null(master_CRS)) master_CRS <- get_projection(mshp_raw, as.CRS = TRUE)
	orig_CRS <- master_CRS # needed for adjusting bbox in process_shapes
	if (interactive) {
		if (is.na(get_projection(mshp_raw, as.CRS = TRUE)) && is_projected(mshp_raw)) {
			
			stop("The projection of the shape object ", x[[shape.id[masterID]]]$shp_name, " is not known, while it seems to be projected.", call.=FALSE)
		}
		master_CRS <- .CRS_longlat
	}
	## find master bounding box (unprocessed)
	bbx_raw <- bb(mshp_raw)
	
	list(shape.id=shape.id,
		 nshps=nshps,
		 apply_map_coloring=apply_map_coloring,
		 any_raster=any_raster,
		 masterID=masterID,
		 master_CRS=master_CRS,
		 orig_CRS=orig_CRS,
		 bbx_raw=bbx_raw)
}

prepare_vp <- function(vp, s, interactive, x, masp) {
	if (interactive) {
		list(dasp = 1,
			 asp_ratio = 1)
	} else {
		if (is.null(vp)) {
			grid.newpage()
		} else {
			if (is.character(vp)) seekViewport(vp) else pushViewport(vp)
		}
		
		## calculate device aspect ratio (needed for small multiples' nrow and ncol)
		inner.margins <- if ("tm_layout" %in% names(x)) {
			x[[which(names(x)=="tm_layout")[1]]]$inner.margins
		} else NA
		inner.margins <- if (is.na(inner.margins[1])) {
			if (s$any_raster) rep(0, 4) else rep(0.02, 4)
		} else rep(inner.margins, length.out=4)
		xmarg <- sum(inner.margins[c(2,4)])
		ymarg <- sum(inner.margins[c(1,3)])
		if (xmarg >= .8) stop("Inner margins too large", call. = FALSE)
		if (ymarg >= .8) stop("Inner margins too large", call. = FALSE)
		iasp <- masp * (1+(xmarg/(1-xmarg))) / (1+(ymarg/(1-ymarg)))
		dasp <- convertWidth(unit(1,"npc"), "inch", valueOnly=TRUE)/convertHeight(unit(1,"npc"), "inch", valueOnly=TRUE)
		asp_ratio <- iasp / dasp
		list(dasp = dasp,
			 asp_ratio = asp_ratio)
	}
}

determine_asp_ratios <- function(gmeta, interactive) {
	if (interactive) {
		dw <- 1
		dh <- 1
		tasp <- fasp <- 1
		lasp <- NA
		fpi <- NULL
	} else {
		dw <- convertWidth(unit(1-sum(gmeta$margins[c(2,4)]),"npc"), "inch", valueOnly=TRUE)
		dh <- convertHeight(unit(1-sum(gmeta$margins[c(1,3)]),"npc"), "inch", valueOnly=TRUE)
		
		fpi <- preprocess_facet_layout(gmeta, gmeta$legend.outside, dh, dw)
		
		# aspect ratio of total device
		tasp <- dw/dh
		
		# aspect ratio per facet
		fasp <- fpi$dsw / fpi$dsh #-  fpi$pSH - fpi$between.margin.in)
		
		# aspect ratio per facet minus extern legend
		lasp <- fasp * (1-fpi$legmarx) / (1-fpi$legmary-fpi$attrmary)
	}
	list(dw = dw,
		 dh = dh,
		 tasp = tasp,
		 lasp = lasp,
		 fpi = fpi)
}


print_tmap <- function(x, vp=NULL, return.asp=FALSE, mode=getOption("tmap.mode"), show=TRUE, knit=FALSE, options=NULL, ...) {
	scale.extra <- NULL
	title.snap.to.legend <- NULL
	
	interactive <- (mode == "view")
	
	# reset symbol shape / shape just/anchor lists
	assign(".shapeLib", list(), envir = .TMAP_CACHE)
	assign(".justLib", list(), envir = .TMAP_CACHE)
	
	# shortcut mode: enabled with qtm() or qtm("My Street 1234, Home Town")
	if (names(x)[1]=="tm_shortcut") print_shortcut(x, interactive)
		
	## remove non-supported elements if interactive
	if (interactive) x <- x[supported_elem_view_mode(names(x))]
	
	## gather shape info
	s <- gather_shape_info(x, interactive)

	## get raster and group by variable name (needed for eventual reprojection of raster shapes)
	raster_facets_vars <- lapply(1:s$nshps, function(i) {
		from <- s$shape.id[i] + 1
		to <- ifelse(i==s$nshps, length(x), s$shape.id[i+1]-1)
		fid <- which(names(x)[from:to]=="tm_facets")
		rid <- which(names(x)[from:to]=="tm_raster")
		
		# if (length(rid)) {
		# 	if (is.na(x[[from-1+rid[1]]]$col[1])) return(NA)
		# }
		c(if (length(fid)) x[[from-1+fid[1]]]$by else NULL,
		  if (length(rid)) x[[from-1+rid[1]]]$col else NULL)
	})
	
	## split data.frames from shape/raster objects, and determine shape types
	shps_dts <- mapply(preprocess_shapes, x[s$shape.id], raster_facets_vars, MoreArgs = list(apply_map_coloring=s$apply_map_coloring, master_CRS=s$master_CRS, master_bbx=s$bbx_raw, interactive=interactive), SIMPLIFY = FALSE)
	shps <- lapply(shps_dts, "[[", 1)
	datasets <- lapply(shps_dts, "[[", 2)
	types <- lapply(shps_dts, "[[", 3)

	## determine bounding box and aspect ratio of master shape
	mshp <- shps[[s$masterID]]
	bbx <- attr(mshp, "bbox")
	masp <-	calc_asp_ratio(bbx[1,], bbx[2,], longlat=!attr(mshp, "projected"))

	## remove shapes from and add data to tm_shape objects
	x[s$shape.id] <- mapply(function(y, dataset, type){
		y$type <- type
		y$data <- dataset
		y$shp <- NULL
		y
	}, x[s$shape.id], datasets, types, SIMPLIFY=FALSE)
	
	## prepare viewport (needed to determine asp_ratio for facet layout)
	v <- prepare_vp(vp, s, interactive, x, masp)

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
	shp_info <- x[[s$shape.id[s$masterID]]][c("unit", "unit.size", "line.center.type")]
	shp_info$is_raster <- s$any_raster
	result <- process_tm(x, v$asp_ratio, shp_info, interactive)
	gmeta <- result$gmeta
	gps <- result$gps
	gal <- result$gal
	nx <- result$nx
	data_by <- result$data_by
	
	shps_lengths <- sapply(shps, length)

	## arranges aspect ratios, the grid layout of the map/facets, etc
	a <- determine_asp_ratios(gmeta, interactive)
	
	## process shapes (bbox and crop)
	shps <- process_shapes(shps, x[s$shape.id], gmeta, data_by, a$lasp, s$masterID, allow.crop = !interactive, raster.leaflet=interactive, master_CRS=s$master_CRS, interactive=interactive, orig_CRS=s$orig_CRS)
	
	p <- list(sasp = attr(shps, "sasp"),
			  bbx = attr(shps, "bbx"),
			  legend_pos = attr(shps, "legend_pos"),
			  diff_shapes = attr(shps, "diff_shapes"),
			  inner.margins.new = attr(shps, "inner.margins"))
	
	
	
	## further arranges the grid layout of the map/facets
	if (!interactive) gmeta <- do.call("process_facet_layout", c(list(gmeta, p$sasp, a$dh, a$dw), a$fpi))
		
	## create external legend and attributes objects
	g <- process_gps(gps, shps, x, gmeta, nx, p, a, s, v, masp, shps_lengths, interactive, return.asp)
	
	## adds data to gps (needed for view mode)
	gps2 <- add_data_to_gps(g$gps, s, datasets, g$matchIDs, interactive)
	
	
	
	
	## plot
	if (interactive) {
		lVargs <- list(ncol=gmeta$ncol,
					   sync=ifelse(gmeta$sync, "all", "none"),
					   sync.cursor=gmeta$sync,
					   no.initial.sync = TRUE)
		
		
		multi_shapes <- (is.list(shps[[1]]))
		if (multi_shapes) {
			lfs <- mapply(view_tmap, gps2, shps, SIMPLIFY = FALSE)
		} else {
			lfs <- lapply(gps2, view_tmap, shps)
		}
		lfs <<- lfs
		lf <- if (nx==1) lfs[[1]] else do.call(mapview::latticeView, c(lfs, lVargs))
		
		if (show) {
			save_last_map()
			if (knit) {
				return(do.call("knit_print", c(list(x=lf), list(...), list(options=options))))
			} else {
				return(print(lf))
			}
		} else lf
	} else {
		if (show) {
			if (nx > 1) sasp <- v$dasp
			#  gridplot:   - makes small multiples grid
			#              - calls plot_all for grob trees
			#              - plot the grob trees
			#  plot_all:   - calls plot_map to create grob tree of map itself
			#              - calls legend_prepare and plot_legend to create grob tree of legend
			#              - creates grob tree for whole plot
			gridplot(gmeta, "plot_all", nx, g$gps, gal, shps, v$dasp, p$sasp, p$inner.margins.new, p$legend_pos, g$gp_leg, g$gp_attr)
			## if vp is specified, go 1 viewport up, else go to root viewport
			upViewport(n=as.integer(!is.null(vp)))
			save_last_map()
			invisible(list(shps=shps, gps=gps2))
		} else {
			list(shps=shps, gps=gps2)
		}
	}
}
