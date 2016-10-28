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
#' @import sp
#' @importFrom raster raster brick extent setValues ncell couldBeLonLat fromDisk crop projectRaster projectExtent colortable nlayers minValue maxValue
#' @importMethodsFrom raster as.vector
#' @import RColorBrewer
#' @import grid
#' @import methods
#' @importFrom classInt classIntervals findCols
#' @importFrom rgeos gIntersection gIntersects gBuffer gDifference gCentroid
#' @importFrom grDevices col2rgb colorRampPalette dev.off is.raster png rgb
#' @importFrom stats na.omit dnorm fft quantile rnorm runif 
#' @importFrom spdep poly2nb
#' @importFrom grDevices xy.coords colors
#' @importFrom graphics par
#' @importFrom rgdal getPROJ4VersionInfo SGDF2PCT
#' @importFrom utils capture.output data download.file head setTxtProgressBar tail txtProgressBar
#' @importMethodsFrom raster as.vector
#' @importFrom geosphere distGeo
#' @import leaflet
#' @importFrom htmltools htmlEscape
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


print_tmap <- function(x, vp=NULL, return.asp=FALSE, mode=getOption("tmap.mode"), show=TRUE, knit=FALSE, options=NULL, ...) {
	#### General process of tmap:
	#  print.tmap: - puts shapes and shape data into right format
	#              - calls preprocess_shapes for preprocessing (i.e. reprojecting) shapes
	#              - calls process_tm for processing tm elements
	#              - calls process_shapes for processing shapes
	#              - calls (pre)process_facet_layout
	#              - calls plot function gridplot, that calls plot_all
	#  process_tm: - get all non-layer elements, (tm_layout, tm_grid, ...)
	#              - process layer elements by calling process_layers (result is gp)
	#              - determine number of small multiples (nx)
	#              - process non-layer elements by calling process_meta (result is gmeta)
	#              - split gp into small multiples (result is gps)
	#  process_layers: - determine grouped small multiples (specified by user with tm_facets(by=))
	#                  - process layer functions by calling indivudual functions, like tm_fill
	#  process_meta: - determines number of rows and colums for small multiples
	#                - applies scale factor to all meta elements (tm_layout, tm_style, tm_compass, tm_scale_bar, tm_grid)
	#  preprocess_shapes: - project shapes
	#  process_shapes: - determines bounding box(es)
	#                  - crop shapes
	#  (pre)process_facet_layout - arranges the grid layout of the map/facets, which is input for gridplot
	#  gridplot:   - makes small multiples grid
	#              - calls plot_all for grob trees
	#              - plot the grob trees
	#  plot_all:   - calls plot_map to create grob tree of map itself
	#              - calls legend_prepare and plot_legend to create grob tree of legend
	#              - creates grob tree for whole plot

	scale.extra <- NULL
	title.snap.to.legend <- NULL
	
	interactive <- (mode == "view")
	
	# reset symbol shape / shape just/anchor lists
	assign(".shapeLib", list(), envir = .TMAP_CACHE)
	assign(".justLib", list(), envir = .TMAP_CACHE)
	
	# shortcut mode: enabled with qtm() or qtm("My Street 1234, Home Town")
	if (names(x)[1]=="tm_shortcut") {
		if (getOption("tmap.mode")=="plot") {
			stop("Either specify shp, or set mode to \"view\" with tmap_mode or ttm", call.=FALSE)	
		} else {
			gt <- preprocess_gt(x, interactive=interactive)
			gt$bbx <- x$tm_shortcut$bbx
			gt$center <- x$tm_shortcut$center
			
			lf <-view_tmap(list(list(tm_layout=gt)))
			
			if (knit) {
				return(do.call("knit_print", c(list(x=lf), list(...), list(options=options))))
				#return(knit_print(lf, ..., options=options))
			} else {
				return(print(lf))
			}
		}
	}
	
	
	## remove non-supported elements if interactive
	if (interactive) {
		if (any(names(x)=="tm_facets")) {
			facetsby <- x[[which(names(x)=="tm_facets")[1]]]$by
			if (!is.null(facetsby)) warning("Facets are not supported in view mode yet. The data is not split by \"", facetsby, "\"", call.=FALSE)	
				
		} 
		if (any(names(x)=="tm_grid")) warning("Grid lines not supported in view mode.", call.=FALSE)
		if (any(names(x)=="tm_scale_bar")) warning("Scale bar not supported in view mode.", call.=FALSE)
		if (any(names(x)=="tm_credits")) warning("Credits not supported in view mode.", call.=FALSE)
		if (any(names(x)=="tm_logo")) warning("Logo not supported in view mode.", call.=FALSE)
		if (any(names(x)=="tm_compass")) warning("Compass not supported in view mode.", call.=FALSE)
		if (any(names(x)=="tm_xlab")) warning("X-axis label not supported in view mode.", call.=FALSE)
		if (any(names(x)=="tm_ylab")) warning("Y-axis label not supported in view mode.", call.=FALSE)
		
		x[names(x) %in% c("tm_grid", "tm_scale_bar", "tm_credits", "tm_logo", "tm_compass", "tm_xlab", "tm_ylab", "tm_facets")] <- NULL
	}
	
	
	
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
	if (interactive) master_CRS <- .CRS_longlat
	
	## find master bounding box (unprocessed)
	bbx_raw <- bb(mshp_raw)

	## get raster and group by variable name (needed for eventual reprojection of raster shapes)
	raster_facets_vars <- lapply(1:nshps, function(i) {
		from <- shape.id[i] + 1
		to <- ifelse(i==nshps, length(x), shape.id[i+1]-1)
		fid <- which(names(x)[from:to]=="tm_facets")
		rid <- which(names(x)[from:to]=="tm_raster")
		
		# if (length(rid)) {
		# 	if (is.na(x[[from-1+rid[1]]]$col[1])) return(NA)
		# }
		c(if (length(fid)) x[[from-1+fid[1]]]$by else NULL,
		  if (length(rid)) x[[from-1+rid[1]]]$col else NULL)
	})
	
	## split data.frames from shape/raster objects, and determine shape types
	shps_dts <- mapply(preprocess_shapes, x[shape.id], raster_facets_vars, MoreArgs = list(apply_map_coloring=apply_map_coloring, master_CRS=master_CRS, master_bbx=bbx_raw, interactive=interactive), SIMPLIFY = FALSE)
	
	shps <- lapply(shps_dts, "[[", 1)
	datasets <- lapply(shps_dts, "[[", 2)
	types <- lapply(shps_dts, "[[", 3)

	## determine aspect ratio of master shape
	mshp <- shps[[masterID]]
	
	bbx <- attr(mshp, "bbox")
	masp <-	calc_asp_ratio(bbx[1,], bbx[2,], longlat=!attr(mshp, "projected"))

	## remove shapes from and add data to tm_shape objects
	x[shape.id] <- mapply(function(y, dataset, type){
		y$type <- type
		y$data <- dataset
		y$shp <- NULL
		y
	}, x[shape.id], datasets, types, SIMPLIFY=FALSE)
	
	## prepare viewport
	if (interactive) {
		asp_ratio <- 1
		iasp <- 1
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
			if (any_raster) rep(0, 4) else rep(0.02, 4)
		} else rep(inner.margins, length.out=4)
		xmarg <- sum(inner.margins[c(2,4)])
		ymarg <- sum(inner.margins[c(1,3)])
		if (xmarg >= .8) stop("Inner margins too large", call. = FALSE)
		if (ymarg >= .8) stop("Inner margins too large", call. = FALSE)
		iasp <- masp * (1+(xmarg/(1-xmarg))) / (1+(ymarg/(1-ymarg)))
		dasp <- convertWidth(unit(1,"npc"), "inch", valueOnly=TRUE)/convertHeight(unit(1,"npc"), "inch", valueOnly=TRUE)
		asp_ratio <- iasp / dasp
	}
	
	## process tm objects
	shp_info <- x[[shape.id[masterID]]][c("unit", "unit.size", "line.center.type")]
	shp_info$is_raster <- any_raster
	result <- process_tm(x, asp_ratio, shp_info, interactive)
	gmeta <- result$gmeta

	gps <- result$gps
	gal <- result$gal
	nx <- result$nx
	data_by <- result$data_by
	## process shapes
	margins <- gmeta$outer.margins
	if (interactive) {
		dw <- 1
		dh <- 1
	} else {
		dw <- convertWidth(unit(1-sum(margins[c(2,4)]),"npc"), "inch", valueOnly=TRUE)
		dh <- convertHeight(unit(1-sum(margins[c(1,3)]),"npc"), "inch", valueOnly=TRUE)
	}
	shps_lengths <- sapply(shps, length)

	external_legend <- gmeta$legend.outside
	external_attr <- gmeta$attr.outside
	fpi <- preprocess_facet_layout(gmeta, external_legend, dh, dw)
	
	# aspect ratio of total device
	tasp <- dw/dh
	
	# aspect ratio per facet
	fasp <- fpi$dsw / fpi$dsh #-  fpi$pSH - fpi$between.margin.in)
	
	# aspect ratio per facet minus extern legend
	lasp <- fasp * (1-fpi$legmarx) / (1-fpi$legmary)
	
	shps <- process_shapes(shps, x[shape.id], gmeta, data_by, lasp, masterID, allow.crop = !interactive, raster.leaflet=interactive, master_CRS=master_CRS, interactive=interactive, orig_CRS=orig_CRS)
	
	sasp <- attr(shps, "sasp")
	bbx <- attr(shps, "bbx")

	gmeta <- do.call("process_facet_layout", c(list(gmeta, external_legend, sasp, dh, dw), fpi))
	gasp <- gmeta$gasp
	
	if (external_legend) {
		leg_ids <- seq(1, nx, by=gmeta$ncol * gmeta$nrow)
		gp_leg <- lapply(leg_ids, function(li) {
			gli <- gps[[li]]
			gli$tm_layout <- within(gli$tm_layout, {
				legend.only <- TRUE
				legend.width <- .9
				legend.height <- .9
				
				if (title.snap.to.legend) {
					title.size <- title.size / scale.extra
				} else {
					title <- ""
				}
				legend.title.size <- legend.title.size / scale.extra
				legend.text.size <- legend.text.size / scale.extra
				legend.hist.size <- legend.hist.size / scale.extra
				grid.show <- FALSE
				scale.show <- FALSE
				compass.show <- FALSE
				credits.show <- FALSE
				logo.show <- FALSE
			})
			gli
		})
		gps <- lapply(gps, function(gp) {
			gp$tm_layout$legend.show <- FALSE
			if (gp$tm_layout$title.snap.to.legend) gp$tm_layout$title <- ""
			gp
		})
	} else {
		gp_leg <- NULL
	}

	if (external_attr) {
		attr_ids <- seq(1, nx, by=gmeta$ncol * gmeta$nrow)
		gp_attr <- lapply(attr_ids, function(ai) {
			gai <- gps[[ai]]
			gai$tm_layout <- within(gai$tm_layout, {
				legend.only <- TRUE
				legend.show <- FALSE
				title <- ""
			})	
			gai
		})
			
		gps <- lapply(gps, function(gp) {
			gp$tm_layout$scale.show <- FALSE
			gp$tm_layout$compass.show <- FALSE
			gp$tm_layout$credits.show <- FALSE
			gp$tm_layout$logo.show <- FALSE
			gp
		})
	} else {
		gp_attr <- NULL
	}
	

	legend_pos <- attr(shps, "legend_pos")
	diff_shapes <- attr(shps, "diff_shapes")
	inner.margins.new <- attr(shps, "inner.margins")


	## show aspect ratios in design mode
	if (gmeta$design.mode && !interactive) {
		masterShapeName <- x[[masterID]]$shp_name
		showBrown <- gasp!=sasp
		showGreen <- !(!is.na(gmeta$asp) && gmeta$asp==0 && nx==1)
		pretext <- c("specified (asp argument of tm_layout)", "device (yellow)", "device without outer margins (green)",  "facets region (brown)", "frame (blue)", paste("master shape, ", masterShapeName, ", (red)", sep=""))
		posttext <- format(c(gmeta$asp, dasp, tasp, gasp, sasp, masp))
		if (!showBrown) {
			pretext <- pretext[-4]
			posttext <- posttext[-4]
		}
		if (!showGreen) {
			pretext <- pretext[-3]
			posttext <- posttext[-3]
		}
		
		lns <- nchar(pretext) + nchar(posttext)
		l <- max(max(nchar(pretext)) + max(nchar(posttext)) + 1, 25)
		medtext <- vapply(l-lns, function(i)paste(rep(" ", i), collapse=""), character(1))
		
		texts <- c(paste("----------------aspect ratios--", paste(rep("-", l-25), collapse=""), sep=""),
				   paste("|", pretext, medtext, posttext, "|"),
				   paste(rep("-", l+6), collapse=""))
		
		for (tx in texts) message(tx)
	}
	
	# shortcut used by save_tmap
	if (return.asp && !interactive) return(gasp)
	
	## shapes have been subset (diff_shapes) and cropped. Therefore, the corresponding aesthetics have to be subset accordingly:
	if (diff_shapes) {
		matchIDs <- lapply(shps, function(ss) lapply(ss, function(s) if (inherits(s, "Raster")) s[] else s$tmapID))
						   
		gps <- mapply(function(gp, masterID) {
			gp[1:nshps] <- mapply(function(gpl, indices, l) {
				gpl$npol <- length(indices)
				lapply(gpl, function(gplx) {
					if ((is.vector(gplx) || is.factor(gplx)) && length(gplx)==l) {
						gplx <- gplx[indices]	
					} else {
						gplx
					}
				})
			},  gp[1:nshps], masterID, shps_lengths, SIMPLIFY=FALSE)
			gp
		}, gps, matchIDs, SIMPLIFY=FALSE)
		
	} else {
		matchIDs <- lapply(shps, function(s) if (inherits(s, "Raster")) s[] else s$tmapID)

		gps <- lapply(gps, function(gp) {
			gp[1:nshps] <- mapply(function(gpl, indices, l) {
				gpl$npol <- length(indices)
				lapply(gpl, function(gplx) {
					if ((is.vector(gplx) || is.factor(gplx)) && length(gplx)==l) {
						gplx <- gplx[indices]	
					} else {
						gplx
					}
				})
			},  gp[1:nshps], matchIDs, shps_lengths, SIMPLIFY=FALSE)
			gp
		})
		
	}
	
	# append data to gps
	gps2 <- lapply(gps, function(gp) {
		gp[-length(gp)] <- mapply(function(gpl, dt) {
			dt$SHAPE_AREAS <- NULL
			
			if (interactive) {
				
				# add density values
				if (!is.na(gpl$xfill)) {
					if (gpl$fill.legend.hist.misc$densities) {
						densvar <- paste(gpl$xfill[1], "density", sep="_")
						dt[[densvar]] <- gpl$fill.legend.hist.misc$values
						gplxfill <- c(gpl$xfill, densvar)
					} else {
						gplxfill <- gpl$xfill
					}
				} else gplxfill <- NA
				
				if (is.null(gpl$fill.popup.vars) || identical(gpl$fill.popup.vars, FALSE)) {
					gpl$fill.popup.vars <- NA
				} else if (identical(gpl$fill.popup.vars, TRUE)) {
					gpl$fill.popup.vars <- names(dt)
				} else if (is.na(gpl$fill.popup.vars[1])) {
					gpl$fill.popup.vars <- if (is.na(gplxfill[1])) names(dt) else gplxfill
				} else {
					if (!all(gpl$fill.popup.vars %in% names(dt))) stop("Not all popup variables are contained in the data", call.=FALSE)
				}
				
				if (is.null(gpl$line.popup.vars) || identical(gpl$line.popup.vars, FALSE)) {
					gpl$line.popup.vars <- NA
				} else if (identical(gpl$line.popup.vars, TRUE)) {
					gpl$line.popup.vars <- names(dt)
				} else if (is.na(gpl$line.popup.vars[1])) {
					gpl$line.popup.vars <- unique(na.omit(c(gpl$xline, gpl$xlinelwd)))
					if (length(gpl$line.popup.vars) == 0) gpl$line.popup.vars <- names(dt)
				} else {
					if (!all(gpl$line.popup.vars %in% names(dt))) stop("Not all popup variables are contained in the data", call.=FALSE)
				}

				if (is.null(gpl$symbol.popup.vars) || identical(gpl$symbol.popup.vars, FALSE)) {
					gpl$symbol.popup.vars <- NA
				} else if (identical(gpl$symbol.popup.vars, TRUE)) {
					gpl$symbol.popup.vars <- names(dt)
				} else if (is.na(gpl$symbol.popup.vars[1])) {
					gpl$symbol.popup.vars <- unique(na.omit(c(gpl$xsize, gpl$xcol, gpl$xshape)))
					if (length(gpl$symbol.popup.vars) == 0) gpl$symbol.popup.vars <- names(dt)
				} else {
					if (!all(gpl$symbol.popup.vars %in% names(dt))) stop("Not all popup variables are contained in the data", call.=FALSE)
				}
				
				gpl$fill.names <- gpl$idnames$fill
				gpl$line.names <- gpl$idnames$line
				gpl$symbol.names <- gpl$idnames$symbol
			}
			
			gpl$data <- dt
			gpl
		}, gp[-length(gp)], datasets, SIMPLIFY = FALSE)
		gp
	})
	
	## plot
	if (interactive) {
		lf <- view_tmap(gps2, shps, bbx)
		if (show) {
			save_last_map()
			if (knit) {
				return(do.call("knit_print", c(list(x=lf), list(...), list(options=options))))
				#return(knit_print(lf, ..., options=options))
			} else {
				return(print(lf))
			}
		} else lf
	} else {
		if (show) {
			if (nx > 1) sasp <- dasp
			gridplot(gmeta, "plot_all", nx, gps, gal, shps, dasp, sasp, inner.margins.new, legend_pos, gp_leg, gp_attr)
			## if vp is specified, go 1 viewport up, else go to root viewport
			upViewport(n=as.integer(!is.null(vp)))
			save_last_map()
			invisible(list(shps=shps, gps=gps2))
		} else {
			list(shps=shps, gps=gps2)
		}
	}
}
