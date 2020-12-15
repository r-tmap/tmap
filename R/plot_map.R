plot_map <- function(i, gp, gt, shps, bbx, proj, sasp) {
	nlayers <- length(gp)

	## bubble height needed to align with bubbles in legend
	lineInch <- convertHeight(unit(1, "lines"), "inch", valueOnly=TRUE)
	
	lineNatH <- convertHeight(unit(lineInch, "inch"), "npc", valueOnly=TRUE) * (bbx[4] - bbx[2])
	lineNatW <- convertWidth(unit(lineInch, "inch"), "npc", valueOnly=TRUE) * (bbx[3] - bbx[1])
	

	## grid lines
	## metaX and Y are X and Y margins for the meta plot (legend etc)
	if (gt$grid.show) {
		gridRes <- plot_grid(gt, scale=gt$scale, add.labels = gt$grid.labels.inside.frame & gt$grid.labels.show)
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
		if ((length(shp) == 0 || nrow(shp) == 0) && (!"tm_grid" %in% gpl$plot.order)) return(NULL)
		
		bbx <- attr(shp, "bbox")
		
		## obtain coordinates (to draw bubbles and text)
		if (inherits(shp, "sf")) {
			res <- get_sf_coordinates(shp, gpl)
			co.native <- res$co
			if (attr(shp, "point.per")=="segment") {
				gpl <- res$gpl
				shp <- res$shp
			}
		} else {
			co.native <- NA
		}

		plot_tm_tiles <- function() NULL
		
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
		
		plot_tm_symbols <- function() plot_symbols(co.native, gpl, gt, lineInch, lineNatH, lineNatW, i, k)
		plot_tm_text <- function() plot_text(co.native, gpl, gt, lineNatH, lineNatW, just=gpl$text.just, bbx=bbx)
		
		
		plot_tm_grid <- function() treeGridLines
		
		plot_tm_raster <- function() {
			rast <- if (is.null(gpl$raster)) NA else gpl$raster
			bb_target <- attr(shp, "bbox")
			bb_real <- sf::st_bbox(shp)
			
			if (is_regular_grid(shp)) {
				if (all(abs(bb_real-bb_target)< 1e-3)) {
					width <- 1
					height <- 1
					cent <- c(mean.default(c(bb_target[1], bb_target[3])), mean.default(c(bb_target[2], bb_target[4])))
				} else {
					width <- (bb_real[3] - bb_real[1]) / (bb_target[3] - bb_target[1])
					height <- (bb_real[4] - bb_real[2]) / (bb_target[4] - bb_target[2])
					cent <- c(mean.default(c(bb_real[1], bb_real[3])), mean.default(c(bb_real[2], bb_real[4])))
				}
				
				x <- (cent[1] - bb_target[1]) / (bb_target[3] - bb_target[1])
				y <- (cent[2] - bb_target[2]) / (bb_target[4] - bb_target[2])
				
				m <- matrix(rast, ncol=nrow(shp), nrow=ncol(shp), byrow = TRUE)
				
				y_is_neg <- all(diff(st_get_dimension_values(shp, "y")) < 0)
				if (!y_is_neg) {
					m <- m[nrow(m):1L, ]
				}

				rasterGrob(m, x=x, y=y, width=width, height=height, interpolate = gpl$raster.misc$interpolate)
			} else {
				s <- sf::st_as_sf(shp)
				grid.shape(s, gp=gpar(fill=rast, col=NA), bg.col=NA, i, k)
			}
		} 
		
		e <- environment()
		fnames <- paste("plot", gpl$plot.order, sep="_")
		grobs <- lapply(fnames, do.call, args=list(), envir=e)
		
		#browser()
		
		
		if ("plot_tm_text" %in% fnames) {
			tGrob <- grobs[[which(fnames=="plot_tm_text")]]
			if (!is.null(tGrob[[1]])) {
				tG <- tGrob[[1]]

				tGX <- convertX(tG$x, "native", valueOnly = TRUE)
				tGY <- convertY(tG$y, "native", valueOnly = TRUE)
				#tGWidth <- convertWidth(tG$width, "native", valueOnly = TRUE)
				#tGHeight <- convertHeight(tG$height, "native", valueOnly = TRUE)
				nt <- length(tGX)

				deltax <- bbx[3] - bbx[1]
				deltay <- bbx[4] - bbx[2]
				delta <- max(deltax, deltay)

				coords <- cbind(tGX, tGY)
				if (gpl$text.along.lines && "plot_tm_lines" %in% fnames) {
					lGrob <- grobs[[which(fnames=="plot_tm_lines")]]
					lShp <- polylineGrob2sfLines(lGrob)

					# lShps <- lapply(lShp@lines, function(l){
					# 	SpatialLines(list(l), proj4string = lShp@proj4string)
					# })
					
					pShp <- st_as_sf(as.data.frame(coords), coords = c("tGX", "tGY"))
					
					
					# plot(lShp)
					# plot(pShp, add=TRUE, pch=19)
					# 
					# plot(iShps[[2]], add=TRUE, col="red")
					
					pbShp <- st_cast(st_geometry(st_buffer(pShp, dist = delta / 100)), "MULTILINESTRING")

					# plot(pbShp, add=TRUE)
					
					iShps <- mapply(function(x,y) st_cast(st_intersection(x,y), "MULTIPOINT"), lShp, pbShp)

					angles <- vapply(iShps, function(x) {
						if (length(x) == 0) 0 else .get_direction_angle(st_coordinates(x)[,1:2, drop = FALSE])
					}, numeric(1))
				} else angles <- rep(0, nt)

				rG <- if (any(angles!=0)) {
					.rectGrob2pathGrob(tGrob[[1]], angles)$rect
				} else tG

				rGX <- convertX(rG$x, "native", valueOnly = TRUE)
				rGY <- convertY(rG$y, "native", valueOnly = TRUE)
				rGWidth <- convertWidth(rG$width, "native", valueOnly = TRUE)
				rGHeight <- convertHeight(rG$height, "native", valueOnly = TRUE)


				# Automatic label placement (Simulated Annealing)
				if (gpl$text.auto.placement || identical(gpl$text.auto.placement, 0)) {
					el <- if (is.numeric(gpl$text.auto.placement)) gpl$text.auto.placement * .5 else 0
					textSizes <- gpl$text.size[gpl$text_sel]
					elX <- convertWidth(unit(textSizes, "lines"), "native", valueOnly = TRUE) * el
					elY <- convertHeight(unit(textSizes, "lines"), "native", valueOnly = TRUE) * el
					xy <- plot_text_pointLabelGrid(rGX-elX*.5, rGY-elY*.5, rGWidth+elX, rGHeight+elY, xyAspect = sasp)
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
				#pushViewport(viewport(xscale = bbx[c(1,3)], yscale = bbx[c(2,4)]))
				# redo automatic labeling (with selection)
				if (gpl$text.auto.placement && (!all(sel))) {
					shiftX <- rep(0, nt)
					shiftY <- rep(0, nt)
					xy <- plot_text_pointLabelGrid(rGX2[sel], rGY2[sel], rGWidth[sel], rGHeight[sel], xyAspect = sasp)
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
					lShp <- sf::st_cast(polylineGrob2sfLines(lGrob), "LINESTRING")

					lShp$tempID <- 1:nrow(lShp)
					#ids <- as.character(shp$tempID)
					
					dShp <- sf::st_cast(suppressWarnings(st_difference(lShp, tShp)), "MULTILINESTRING")
					
					
					nonmatched <- setdiff(1:nrow(lShp), dShp$tempID)
					
					lShp$geometry[dShp$tempID] <- dShp$geometry
					

					sel2 <- ((1:nrow(lShp)) %in% dShp$tempID)[sel]
					
					lco <- st_coordinates(st_cast(lShp, do_split = TRUE))

					# # still doesn't work...
					# lGrob_new <- do.call("gList", mapply(function(i) {
					# 	coor <- lco[lco[,4] == i,]
					# 	lG = lGrob
					# 	lG$x <- unit(coor[,1], "native")
					# 	lG$y <- unit(coor[,2], "native")
					# 	lG$id <- coor[,3]
					# 	lG$id.lengths = NULL
					# 	lG$gp = structure(lapply(lG$gp, function(gp) {
					# 		gp = if (length(gp) == nrow(lShp)) gp[1:nrow(coor)] else gp
					# 	}), class = "gpar")
					# 	lG
					# }, 1:nrow(lShp), SIMPLIFY=FALSE))
					lGrob_new = lGrob

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
			
			world_bb_co2 <- sf::st_coordinates(world_bb_sp2)
			
			world_bb_co3 <- matrix(c((world_bb_co2[,1] - bbx[1])/(bbx[3]-bbx[1]),
									 (world_bb_co2[,2] - bbx[2])/(bbx[4]-bbx[2])), ncol=2)
			
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



