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
#' @import leaflet
#' @importFrom htmlwidgets appendContent onRender
#' @importFrom htmltools tags HTML htmlEscape
#' @importFrom mapview latticeView
#' @importFrom utils packageVersion
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
#' @example ./examples/tmap_leaflet.R
#' @seealso \code{\link{tmap_mode}}, \code{\link{tm_view}}, \code{\link{print.tmap}}
#' @export
tmap_leaflet <- function(x, mode="view", show = FALSE, ...) {
  print.tmap(x, mode=mode, show=show, ...)
}

print_shortcut <- function(x, interactive, args, knit) {
	if (getOption("tmap.mode")=="plot") {
		stop("Either specify shp, or set mode to \"view\" with tmap_mode or ttm", call.=FALSE)	
	} else {
		gt <- preprocess_gt(x, interactive=interactive)
		gt$shape.bbx <- x$tm_shortcut$bbx
		gt$shape.center <- x$tm_shortcut$center
		
		view_tmap(list(tm_layout=gt))
	}
}

supported_elem_view_mode <- function(nms) {
	#if (any(nms=="tm_grid")) warning("Grid lines not supported in view mode.", call.=FALSE)
	if (any(nms=="tm_credits")) warning("Credits not supported in view mode.", call.=FALSE)
	if (any(nms=="tm_logo")) warning("Logo not supported in view mode.", call.=FALSE)
	if (any(nms=="tm_compass")) warning("Compass not supported in view mode.", call.=FALSE)
	if (any(nms=="tm_xlab")) warning("X-axis label not supported in view mode.", call.=FALSE)
	if (any(nms=="tm_ylab")) warning("Y-axis label not supported in view mode.", call.=FALSE)
	#if (any(nms=="tm_scale_bar")) warning("Scale bar not yet supported in view mode, it will be in the next version.", call.=FALSE)
	
	which(!(nms %in% c("tm_credits", "tm_logo", "tm_compass", "tm_xlab", "tm_ylab")))
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
	
	## find master projection (and set to longlat when in view mode)
	master_CRS <- get_proj4(x[[shape.id[masterID]]]$projection, as.CRS = TRUE)
	mshp_raw <- x[[shape.id[masterID]]]$shp
	if (is.null(master_CRS)) master_CRS <- get_projection(mshp_raw, as.CRS = TRUE)
	orig_CRS <- master_CRS # needed for adjusting bbox in process_shapes
	if (interactive) {
		if (is.na(get_projection(mshp_raw, as.CRS = TRUE)) && tmaptools::is_projected(mshp_raw)) {
			
			stop("The projection of the shape object ", x[[shape.id[masterID]]]$shp_name, " is not known, while it seems to be projected.", call.=FALSE)
		}
		master_CRS <- .CRS_longlat
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
			is.RGB <- x[[from-1+rid[1]]]$is.RGB 
		} else is.RGB <- FALSE

		res <- c(if (length(fid)) x[[from-1+fid[1]]]$by else NULL,
				 if (length(rid)) x[[from-1+rid[1]]]$col else NULL)
		if (is.null(res)) res <- NA
		attr(res, "is.RGB") <- is.RGB
		res
	})
	
	## get arguments related to units (approx_areas)
	units_args <- x[[shape.id[masterID]]][c("unit", "orig", "to", "total.area")]
	names(units_args)[names(units_args)=="unit"] <- "target"
	units_args <- units_args[!sapply(units_args, is.null)]
	
	## get arguments related to bb
	bb_args <- x[[masterID]][intersect(names(x[[masterID]]), c("ext", "cx", "cy", "width", "height", "xlim", "ylim", "relative"))]
	bb_args$x <- x[[masterID]]$bbox
	
	## add other shape arguments
	line.center.type <- x[[shape.id[masterID]]]$line.center.type

	list(shape.id=shape.id,
		 shape.nshps=nshps,
		 shape.apply_map_coloring=apply_map_coloring,
		 shape.any_raster=any_raster,
		 shape.masterID=masterID,
		 shape.master_CRS=master_CRS,
		 shape.orig_CRS=orig_CRS,
		 shape.bbx_raw=bbx_raw,
		 shape.units_args=units_args,
		 shape.bb_args=bb_args,
		 shape.line.center.type=line.center.type,
		 shape.raster_facets_vars=raster_facets_vars)
}

prepare_vp <- function(vp, gm, interactive, x) {
	
	if (interactive) {
		devsize <- par("din")
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
		inner.margins <- if ("tm_layout" %in% names(x)) {
			x[[which(names(x)=="tm_layout")[1]]]$inner.margins
		} else NA
		inner.margins <- if (is.na(inner.margins[1])) {
			if (gm$shape.any_raster) rep(0, 4) else rep(0.02, 4)
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
		lasp <- fasp * (1-fpi$legmarx) / (1-fpi$legmary-fpi$attrmary-fpi$attrmary)
	}
	list(shape.dw = dw,
		 shape.dh = dh,
		 shape.tasp = tasp,
		 shape.lasp = lasp,
		 shape.fpi = fpi)
}



print_tmap <- function(x, vp=NULL, return.asp=FALSE, mode=getOption("tmap.mode"), show=TRUE, knit=FALSE, options=NULL, ...) {
	args <- list(...)
	scale.extra <- NULL
	title.snap.to.legend <- NULL
	
	interactive <- (mode == "view")
	
	# reset symbol shape / shape just/anchor lists
	assign(".shapeLib", list(), envir = .TMAP_CACHE)
	assign(".justLib", list(), envir = .TMAP_CACHE)
	
	# shortcut mode: enabled with qtm() or qtm("My Street 1234, Home Town")
	if (names(x)[1]=="tm_shortcut") {
		lf <- print_shortcut(x, interactive, args, knit)
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

	## remove shapes from and add data to tm_shape objects
	x[gm$shape.id] <- mapply(function(y, dataset, type){
		y$type <- type
		y$data <- dataset
		y$shp <- NULL
		y
	}, x[gm$shape.id], datasets, types, SIMPLIFY=FALSE)
	
	## prepare viewport (needed to determine asp_ratio for facet layout)
	gm  <- c(gm, prepare_vp(vp, gm, interactive, x))

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
	result <- process_tm(x, gm, interactive)
	gm <- c(result$gmeta, gm)
	gps <- result$gps
	gal <- result$gal
	nx <- result$nx
	data_by <- result$data_by
	
	gm$shape.shps_lengths <- sapply(shps, length)

	## arranges aspect ratios, the grid layout of the map/facets, etc
	gm <- c(gm, determine_asp_ratios(gm, interactive))
	
	## process shapes (bbox and crop)
	shps <- process_shapes(shps, x[gm$shape.id], gm, data_by, allow.crop = !interactive, interactive=interactive)
	gm <- c(gm, attr(shps, "info"))

	## further arranges the grid layout of the map/facets
	if (!interactive) gm <- process_facet_layout(gm)

	#browser()
	#gm$units <- s$shape.units
	
	## create external legend and attributes objects
	g <- process_gps(gps, shps, x, gm, nx, interactive, return.asp)
	
	## return in case g is a number, i.e. the aspect ratio
	if (is.numeric(g)) return(g)
	
	## adds data to gps (needed for view mode)
	gps2 <- add_data_to_gps(g$gps, gm, datasets, g$matchIDs, interactive)
	
	## plot
	if (interactive) {
		lVargs <- list(ncol=gm$ncol,
					   sync=ifelse(gm$sync, "all", "none"),
					   sync.cursor=gm$sync,
					   no.initial.sync = TRUE)
		
		multi_shapes <- (is.list(shps[[1]]))
		showWarns <- c(TRUE, rep(FALSE, length(gps)-1))
		if (multi_shapes) {
			lfs <- mapply(view_tmap, gps2[1:nx], shps[1:nx], leaflet_id=1:nx, showWarns=showWarns, SIMPLIFY = FALSE)
		} else {
			lfs <- mapply(view_tmap, gps2[1:nx], leaflet_id=1:nx, showWarns=showWarns, MoreArgs = list(shps=shps), SIMPLIFY = FALSE)
		}
		lf <- if (nx==1) lfs[[1]] else lfmv <- do.call(mapview::latticeView, c(lfs, lVargs))
		
		lf2 <- add_leaflet_titles(lf)

		if (show) {
			save_last_map()
			if (knit) {
				kp <- get("knit_print", asNamespace("knitr"))
				return(do.call(kp, c(list(x=lf2), args, list(options=options))))
			} else {
				return(print(lf2))
			}
		} else lf
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


