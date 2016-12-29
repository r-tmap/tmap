plot_map <- function(i, gp, gt, shps, bbx, proj, sasp) {
	nlayers <- length(gp)
	
	## bubble height needed to align with bubbles in legend
	lineInch <- convertHeight(unit(1, "lines"), "inch", valueOnly=TRUE)
	
	
	## grid lines
	## metaX and Y are X and Y margins for the meta plot (legend etc)
	if (gt$grid.show) {
		gridRes <- plot_grid(gt, scale=gt$scale, add.labels = gt$grid.labels.inside.frame)
		treeGridLines <- gridRes$treeGridLines
		treeGridLabels <- gridRes$treeGridLabels
		metaX <- gridRes$metaX
		metaY <- gridRes$metaY
	} else {
		treeGridLines <- NULL
		treeGridLabels <- NULL
		metaX <- 0
		metaY <- 0
	}


	## thematic map layers
	treeElements <- mapply(function(gpl, shp, k) {
		bbx <- attr(shp, "bbox")
		
		## obtain coordinates (to draw bubbles and text)
		if (inherits(shp, "Spatial")) {
			res <- get_sp_coordinates(shp, gpl, gt, bbx)
			co.npc <- res$co
			if (gt$shape.line.center.type[1]=="segment") {
				gpl <- res$gpl
				shp <- res$shp
			}	
			co.npc[,1] <- if (bbx[1, 2]-bbx[1,1]==0) .5 else {
				(co.npc[,1]-bbx[1,1]) / (bbx[1, 2]-bbx[1,1])	
			}
			co.npc[,2] <- if (bbx[2, 2]-bbx[2,1]==0) .5 else {
				(co.npc[,2]-bbx[2,1]) / (bbx[2, 2]-bbx[2,1])
			}
		} else {
			co.npc <- NA
		}

		plot_tm_fill <- function() {
			fill <- if (is.null(gpl$fill)) NA else gpl$fill
			col <- gpl$col
			grid.shape(shp, gp=gpar(fill=fill, col=col, lwd=gpl$lwd, lty=gpl$lty), bg.col=gt$bg.color, i, k)
		}
		
		plot_tm_lines <- function() {
			col <- gpl$line.col
			grid.shplines(shp, gp=gpar(col=col, lwd=gpl$line.lwd, lty=gpl$line.lty,
									   lineend="butt"), i, k)
		}
		
		plot_tm_symbols <- function() plot_symbols(co.npc, gpl, gt, lineInch, i, k)
		plot_tm_text <- function() plot_text(co.npc, gpl, gt, lineInch, just=gpl$text.just)
		
		
		plot_tm_grid <- function() treeGridLines
		
		plot_tm_raster <- function() {
			rast <- if (is.null(gpl$raster)) NA else gpl$raster
			bb_target <- attr(shp, "bbox")
			bb_real <- bbox(shp)
			
			if (all(abs(bb_real-bb_target)< 1e-3)) {
				width <- 1
				height <- 1
				cent <- rowMeans(bb_target)
			} else {
				width <- (bb_real[1,2] - bb_real[1,1]) / (bb_target[1,2] - bb_target[1,1])
				height <- (bb_real[2,2] - bb_real[2,1]) / (bb_target[2,2] - bb_target[2,1])
				cent <- rowMeans(bb_real)
			}
			
			x <- (cent[1] - bb_target[1,1]) / (bb_target[1,2] - bb_target[1,1])
			y <- (cent[2] - bb_target[2,1]) / (bb_target[2,2] - bb_target[2,1])
			#if (inherits(shp, "Spatial")) shp <- as(shp, "RasterLayer")
			rasterGrob(matrix(rast, ncol=shp@ncols, nrow=shp@nrows, byrow = TRUE), x=x, y=y, width=width, height=height, interpolate = gpl$raster.misc$interpolate)
		} 
		
		e <- environment()
		fnames <- paste("plot", gpl$plot.order, sep="_")
		grobs <- lapply(fnames, do.call, args=list(), envir=e)
		
		
		
		if ("plot_tm_text" %in% fnames) {
			tGrob <- grobs[[which(fnames=="plot_tm_text")]]
			if (!is.null(tGrob[[1]])) {
				tG <- tGrob[[1]]
				
				tGX <- convertX(tG$x, "npc", valueOnly = TRUE)
				tGY <- convertY(tG$y, "npc", valueOnly = TRUE)
				#tGWidth <- convertWidth(tG$width, "npc", valueOnly = TRUE)
				#tGHeight <- convertHeight(tG$height, "npc", valueOnly = TRUE)
				nt <- length(tGX)
				
				
				coords <- cbind(tGX, tGY)
				if (gpl$text.along.lines && "plot_tm_lines" %in% fnames) {
					lGrob <- grobs[[which(fnames=="plot_tm_lines")]]
					lShp <- polylineGrob2Lines(lGrob)
					
					lShps <- lapply(lShp@lines, function(l){
						SpatialLines(list(l), proj4string = lShp@proj4string)
					})
					
					pShp <- SpatialPoints(coords, proj4string = lShp@proj4string)
					ppShp <- as(gBuffer(pShp, width = .01, byid = TRUE), "SpatialLines")
					ppShps <- lapply(ppShp@lines, function(l){
						SpatialLines(list(l), proj4string = ppShp@proj4string)
					})
					iShps <- mapply(gIntersection, lShps, ppShps, MoreArgs = list(byid = FALSE))
					
					angles <- sapply(iShps, function(x) {
						if (is.null(x)) 0 else {
							if (inherits(x, "SpatialPoints")) {
								.get_direction_angle(x@coords)	
							} else if (inherits(x, "SpatialLines")) {
								.get_direction_angle(x@lines[[1]]@Lines[[1]]@coords)	
							} else 0
							
						}
					})
				} else angles <- rep(0, nt)
				

				rG <- if (any(angles!=0)) {
					.rectGrob2pathGrob(tGrob[[1]], angles)$rect
				} else tG
				
				rGX <- convertX(rG$x, "npc", valueOnly = TRUE)
				rGY <- convertY(rG$y, "npc", valueOnly = TRUE)
				rGWidth <- convertWidth(rG$width, "npc", valueOnly = TRUE)
				rGHeight <- convertHeight(rG$height, "npc", valueOnly = TRUE)
				
				
				# Automatic label placement (Simulated Annealing)
				if (gpl$text.auto.placement || identical(gpl$text.auto.placement, 0)) {
					el <- if (is.numeric(gpl$text.auto.placement)) gpl$text.auto.placement * .5 else 0
					textSizes <- gpl$text.size[gpl$text_sel]
					elX <- convertWidth(unit(textSizes, "lines"), "npc", valueOnly = TRUE) * el
					elY <- convertHeight(unit(textSizes, "lines"), "npc", valueOnly = TRUE) * el
					xy <- pointLabelGrid(rGX-elX*.5, rGY-elY*.5, rGWidth+elX, rGHeight+elY, xyAspect = sasp)
					dir <- atan2(xy$y - rGY, xy$x - rGX)
					shiftX <- (xy$x - rGX) + elX * cos(dir)
					shiftY <- (xy$y - rGY) + elY * sin(dir)
				} else {
					shiftX <- 0
					shiftY <- 0
				}
				rGX2 <- rGX + shiftX
				rGY2 <- rGY + shiftY

				sel <- rep(TRUE, nt)
				if (gpl$text.remove.overlap) {
					# Check for overlap
					for (i in 1:nt) {
						x <- rGX2[i]
						y <- rGY2[i]
						w <- rGWidth[i]
						h <- rGHeight[i]
						x1 <- x - w/2
						x2 <- x + w/2
						y1 <- y - h/2
						y2 <- y + h/2
						
						seli <- sel
						seli[i] <- FALSE
						xr <- rGX2[seli]
						yr <- rGY2[seli]
						wr <- rGWidth[seli]
						hr <- rGHeight[seli]
						xr1 <- xr - wr/2
						xr2 <- xr + wr/2
						yr1 <- yr - hr/2
						yr2 <- yr + hr/2
						
						coverx <- ((x1 > xr1) & (x1 < xr2)) | ((x2 > xr1) & (x2 < xr2))
						covery <- ((y1 > yr1) & (y1 < yr2)) | ((y2 > yr1) & (y2 < yr2))
						sel[i] <- !any(coverx & covery)
					}
				}
				
				# redo automatic labeling (with selection)
				if (gpl$text.auto.placement && (!all(sel))) {
					shiftX <- rep(0, nt)
					shiftY <- rep(0, nt)
					xy <- pointLabelGrid(rGX2[sel], rGY2[sel], rGWidth[sel], rGHeight[sel], xyAspect = sasp)
					shiftX[sel] <- xy$x - rGX2[sel]
					shiftY[sel] <- xy$y - rGY2[sel]
					rGX2 <- rGX2 + shiftX
					rGY2 <- rGY2 + shiftY
				}
				tGrob <- do.call("gList", lapply(tGrob, .editGrob, sel=sel, shiftX=shiftX, shiftY=shiftY, angles=angles))
				
				if (gpl$text.overwrite.lines && "plot_tm_lines" %in% fnames) {
					# Remove line where labels overlap
					lGrob <- grobs[[which(fnames=="plot_tm_lines")]]
					
					tShp <- .grob2Poly(tGrob[[1]])
					
					lShp <- polylineGrob2Lines(lGrob)
					
					shp$tempID <- 1:length(shp)
					ids <- as.character(shp$tempID)
					dShp <- gDifference(lShp, tShp, byid = TRUE, id=ids)
					ids <- as.character(shp$tempID) #bug in gDifference: ids changes
					
					matched <- match(get_IDs(dShp), ids)
					nonmatched <- match(setdiff(ids, get_IDs(dShp)),ids)
					
					dShp <- sbind(dShp, lShp[nonmatched, ])
					allmatched <- match(get_IDs(dShp),ids)
					lco <- coordinates(dShp)
					
					sel2 <- (!(1:length(shp)) %in% nonmatched)[sel]
					
					lGrobSel <- lGrob[allmatched]
					lGrob_new <- do.call("gList", mapply(function(lG, lC) {
						nr <- sapply(lC, nrow)
						coor <- do.call("rbind", lC)
						lG$x <- unit(coor[,1], "npc")
						lG$y <- unit(coor[,2], "npc")
						lG$id <- do.call("c", mapply(rep, 1:length(lC), nr, SIMPLIFY=FALSE))
						lG
					}, lGrobSel, lco, SIMPLIFY=FALSE))

					tGrob <- do.call("gList", lapply(tGrob, .editGrob, sel=sel2, shiftX=0, shiftY=0, angles=angles[sel]))
					
					grobs[[which(fnames=="plot_tm_lines")]] <- lGrob_new
				}

				
				
				# remove unused background
				grobs[[which(fnames=="plot_tm_text")]] <- if (is.na(tGrob[[1]]$gp$fill)) {
					tGrob[-1]
				} else tGrob
			}
		}
		
		
		
		items <- do.call("gList", args =  grobs)
		gTree(children=items)
	}, gp, shps, 1:nlayers, SIMPLIFY=FALSE)
	
	
	# cut map to projection boundaries (i.e. longlat bb -180 - 180, -90 to 90)
	if (gt$earth.boundary) {
		world_bb_sp2 <- tmaptools::bb_earth(projection = proj, earth.datum = gt$earth.datum, bbx = gt$earth.bounds)
		if (!is.null(world_bb_sp2)) {
			world_bb_co2 <- world_bb_sp2@polygons[[1]]@Polygons[[1]]@coords
			
			world_bb_co3 <- matrix(c((world_bb_co2[,1] - bbx[1,1])/(bbx[1,2]-bbx[1,1]),
									 (world_bb_co2[,2] - bbx[2,1])/(bbx[2,2]-bbx[2,1])), ncol=2)
			
			worldBB_bg <- if (!is.na(gt$frame)) NULL else pathGrob(x = world_bb_co3[,1], y = world_bb_co3[,2], id=rep(1,nrow(world_bb_co3)), gp=gpar(col=NA, fill=gt$bg.color))
			worldBB <- pathGrob(x = world_bb_co3[,1], y = world_bb_co3[,2], id=rep(1,nrow(world_bb_co3)), gp=gpar(col=gt$earth.boundary.color, fill=NA, lwd=gt$earth.boundary.lwd))
			
			worldBB_cut <- pathGrob(x = c(0, 0, 1, 1, rev(world_bb_co3[,1])), y = c(0, 1, 1, 0, rev(world_bb_co3[,2])), id=c(rep(1,4), rep(2,nrow(world_bb_co3))), gp=gpar(col=NA, fill=gt$space.color))
			
			if (any(world_bb_co3[,1]>0 & world_bb_co3[,1]< 1 & world_bb_co3[,2] > 0 & world_bb_co3[,2] < 1)) {
				grobWorldBB <- gTree(children=gList(worldBB_cut, worldBB))	
			} else {
				grobWorldBB <- NULL
			}
		} else {
			grobWorldBB <- NULL
			worldBB_bg <- NULL
		}
	} else {
		grobWorldBB <- NULL
		worldBB_bg <- NULL
	}
	
	
	
	grobsElemGrid <- do.call("gList", args = c(list(worldBB_bg), treeElements, list(grobWorldBB, treeGridLabels)))
	
	
	list(treeElemGrid=gTree(children=grobsElemGrid, name="mapElements"), lineInch=lineInch, metaX=metaX, metaY=metaY)
}



