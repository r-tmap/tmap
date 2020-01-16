process_grid <- function(gt, bbx, proj, sasp) {
	grid.n.x <- grid.n.y <- grid.projection <- grid.is.projected <- grid.ndiscr <- NULL
	
	within(gt, { 
		if (!is.na(grid.projection)) {
			bbx_orig <- bbx
			bbx <- suppressWarnings(bb(bbx, current.projection = proj, projection = grid.projection))
		}
		
		## automatically determine number of grid lines
		if (is.na(grid.n.x) && !is.na(grid.n.y)) {
			grid.n.x <- grid.n.y * sasp
		} else if (!is.na(grid.n.x) && is.na(grid.n.y)) {
			grid.n.y <- grid.n.x / sasp
		} else if (is.na(grid.n.x) && is.na(grid.n.y)) {
			grid.n.lines <- 15 / (gt$scale / gt$scale.extra)
			grid.n.x <- round(sasp * (grid.n.lines/(1+sasp)))
			grid.n.y <- round(grid.n.lines / (1+sasp))
		}
		
		## find natural breaks
		grid.custom.x <- !is.na(grid.x[1])
		grid.custom.y <- !is.na(grid.y[1])
		
		if (!grid.custom.x) grid.x <- pretty(bbx[c(1,3)], n=grid.n.x)
		if (!grid.custom.y) grid.y <- pretty(bbx[c(2,4)], n=grid.n.y)

		## copy grid.x and y
		grid.x.orig <- grid.x
		grid.y.orig <- grid.y
		
		## crop
		grid.x <- grid.x[grid.x>bbx[1] & grid.x<bbx[3]]
		grid.y <- grid.y[grid.y>bbx[2] & grid.y<bbx[4]]
		
		## project grid lines
		if (!is.na(grid.projection)) {
			## add extra grid lines to make sure the warped grid is full
			if (grid.custom.x) {
				grid.x2 <- grid.x.orig
			} else {
				gnx2 <- floor(length(grid.x))
				if (gnx2==1) {
					grid.x2 <- grid.x
				} else if (gnx2>1) {
					grid.x2 <- c(rev(seq(grid.x[1], by=-diff(grid.x[1:2]), length.out = gnx2)),
								 grid.x[-c(1, length(grid.x))],
								 seq(grid.x[length(grid.x)], by=diff(grid.x[1:2]), length.out = gnx2))
				} else grid.x2 <- NA
			}
			if (grid.custom.y) {
				grid.y2 <- grid.y.orig
			} else {
				gny2 <- floor(length(grid.y))
				if (gny2==1) {
					grid.y2 <- grid.y
				} else if (gny2>1) {
					grid.y2 <- c(rev(seq(grid.y[1], by=-diff(grid.y[1:2]), length.out = gny2)),
								 grid.y[-c(1, length(grid.y))],
								 seq(grid.y[length(grid.y)], by=diff(grid.y[1:2]), length.out = gny2))
				} else grid.y2 <- NA
			}
			if (!grid.is.projected) {
				grid.x2[abs(grid.x2-180)<1e-9] <- 180
				grid.x2[abs(grid.x2- -180)<1e-9] <- -180
				grid.y2[abs(grid.y2-90)<1e-9] <- 90
				grid.y2[abs(grid.y2- -90)<1e-9] <- -90
				grid.x2 <- grid.x2[grid.x2>=-180 & grid.x2<=180]	
				grid.y2 <- grid.y2[grid.y2>=-90 & grid.y2<=90]	
			}
			
			## select which grid lines have labels
			grid.sel.x <- which(grid.x2 %in% grid.x)
			grid.sel.y <- which(grid.y2 %in% grid.y)
			
			gnx2 <- gny2 <- NULL
			
			## determine limits
			grid.x2.min <- min(min(grid.x2), bbx[1], na.rm=TRUE)
			grid.x2.max <- max(max(grid.x2), bbx[3], na.rm=TRUE)
			grid.y2.min <- min(min(grid.y2), bbx[2], na.rm=TRUE)
			grid.y2.max <- max(max(grid.y2), bbx[4], na.rm=TRUE)
			
			# create SLDF with straight grid lines in grid lines projection
			#bbx2 <- bb(bbx, ext=3)
			

			lnsList <- list(
				if (is.na(grid.x2[1])) NULL else st_multilinestring(lapply(grid.x2, function(x) {
					m <- matrix(c(rep(x,grid.ndiscr), seq(grid.y2.min, grid.y2.max, length.out=grid.ndiscr)), ncol=2)
				})),
				if (is.na(grid.y2[1])) NULL else st_multilinestring(lapply(grid.y2, function(y) {
					m <- matrix(c(seq(grid.x2.min, grid.x2.max, length.out=grid.ndiscr), rep(y,grid.ndiscr)), ncol=2)
				}))
			)
			
			
			# lnsList <- list(
			# 	if (is.na(grid.x2[1])) NULL else Lines(lapply(grid.x2, function(x) {
			# 		m <- matrix(c(rep(x,100), seq(grid.y2.min, grid.y2.max, length.out=100)), ncol=2)
			# 		Line(m)
			# 	}), ID="x"),
			# 	if (is.na(grid.y2[1])) NULL else Lines(lapply(grid.y2, function(y) {
			# 		m <- matrix(c(seq(grid.x2.min, grid.x2.max, length.out=100), rep(y,100)), ncol=2)
			# 		Line(m)
			# 	}), ID="y")
			# )
			
			lnsSel <- !vapply(lnsList, is.null, logical(1))
			if (!any(lnsSel)) {
				grid.co.x.lns <- numeric(0)
				grid.co.y.lns <- numeric(0)
			} else {
				
				lns <- st_sf(ID=c("x", "y")[lnsSel], geometry = st_sfc(lnsList[lnsSel], crs = grid.projection))
				
				# project it to current projection
				lns_proj <- sf::st_transform(lns, crs = proj)

				
				# extract and normalize coordinates
				
				if (lnsSel[1]) {
					xco <- st_coordinates(st_geometry(lns_proj)[1])
					grid.co.x.lns <- lapply(1L:length(st_geometry(lns_proj)[1][[1]]), function(i) {
						lco <- xco[xco[,3]==i, 1:2]
						lco[, 1] <- (lco[, 1]-bbx_orig[1]) / (bbx_orig[3] - bbx_orig[1])
						lco[, 2] <- (lco[, 2]-bbx_orig[2]) / (bbx_orig[4] - bbx_orig[2])
						lco
					})
					xco <- NULL
				} else {
					grid.co.x.lns <- numeric(0)
				}

				if (lnsSel[2]) {
					yco <- st_coordinates(st_geometry(lns_proj)[sum(lnsSel)])
					grid.co.y.lns <- lapply(1L:length(st_geometry(lns_proj)[sum(lnsSel)][[1]]), function(i) {
						lco <- yco[yco[,3]==i, 1:2]
						lco[, 1] <- (lco[, 1]-bbx_orig[1]) / (bbx_orig[3] - bbx_orig[1])
						lco[, 2] <- (lco[, 2]-bbx_orig[2]) / (bbx_orig[4] - bbx_orig[2])
						lco
					})
					yco <- NULL
				} else {
					grid.co.x.lns <- numeric(0)
				}
				
				# grid.co.x.lns <- if (lnsSel[1]) lapply(lns_proj@lines[[1]]@Lines, function(l) {
				# 	lco <- attr(l, "coords")
				# 	lco[, 1] <- (lco[, 1]-bbx_orig[1,1]) / (bbx_orig[1,2] - bbx_orig[1,1])
				# 	lco[, 2] <- (lco[, 2]-bbx_orig[2,1]) / (bbx_orig[2,2] - bbx_orig[2,1])
				# 	lco
				# }) else numeric(0)
				# grid.co.y.lns <- if (lnsSel[2]) lapply(lns_proj@lines[[sum(lnsSel)]]@Lines, function(l) {
				# 	lco <- attr(l, "coords")
				# 	lco[, 1] <- (lco[, 1]-bbx_orig[1,1]) / (bbx_orig[1,2] - bbx_orig[1,1])
				# 	lco[, 2] <- (lco[, 2]-bbx_orig[2,1]) / (bbx_orig[2,2] - bbx_orig[2,1])
				# 	lco
				# }) else numeric(0)
			}
			lns <- NULL
			lns_proj <- NULL
		} else {
			# normalize coordinates
			grid.co.x <- (grid.x-bbx[1]) / (bbx[3] - bbx[1])
			grid.co.y <- (grid.y-bbx[2]) / (bbx[4] - bbx[2])
			
		}
		
		## format grid labels
		
		
		grid.labels.x <- local({
			if (gt$grid.labels.cardinal) {
				xneg <- grid.x < 0
				xpos <- grid.x > 0
				
				xlab <- do.call("fancy_breaks", c(list(vec=abs(grid.x), intervals=FALSE), gt$grid.labels.format)) #format(grid.x, big.mark = ",")	
				
				xlab[xpos] <- paste0(xlab[xpos], "E")
				xlab[xneg] <- paste0(xlab[xneg], "W")
				xlab
			} else {
				do.call("fancy_breaks", c(list(vec=grid.x, intervals=FALSE), gt$grid.labels.format)) #format(grid.x, big.mark = ",")	
			}
		})
		
		
		grid.labels.y <- local({
			if (gt$grid.labels.cardinal) {
				yneg <- grid.y < 0
				ypos <- grid.y > 0
				
				ylab <- do.call("fancy_breaks", c(list(vec=abs(grid.y), intervals=FALSE), gt$grid.labels.format))
				
				ylab[ypos] <- paste0(ylab[ypos], "N")
				ylab[yneg] <- paste0(ylab[yneg], "S")
				ylab
			} else {
				do.call("fancy_breaks", c(list(vec=grid.y, intervals=FALSE), gt$grid.labels.format))
			}
		})
		
	})
}

plot_grid_labels_x <- function(gt, scale) {
	
	labelsx <- gt$grid.labels.x
	
	# find coordinates for projected grid labels
	if (!is.na(gt$grid.projection)) {
		glabelsx <- get_gridline_labels(lco=gt$grid.co.x.lns[gt$grid.sel.x], xax = 0)
		cogridx <- glabelsx$cogrid
		idsx <- glabelsx$ids
		labelsx <- labelsx[idsx]
	} else {
		cogridx <- gt$grid.co.x	
	}
	
	cex <- gt$grid.labels.size*scale
	
	spacerX <- convertHeight(unit(.5, "lines"), unitTo="npc", valueOnly=TRUE) * cex
	marginX <- convertHeight(unit(gt$grid.labels.margin.x, "lines"), unitTo="npc", valueOnly=TRUE) * cex

	just <- ifelse(gt$grid.labels.rot[1] == 90, "right", ifelse(gt$grid.labels.rot[1] == 270, "left", ifelse(gt$grid.labels.rot[1] == 180, "bottom", "top")))
	
	if (gt$grid.ticks) {
		ticks <- polylineGrob(x=rep(cogridx, each = 2), y = rep(c(1-spacerX*.5-marginX,1), length(cogridx)), id = rep(1:length(cogridx), each = 2), gp=gpar(col=gt$grid.col, lwd=gt$grid.lwd))	
	} else {
		ticks <- NULL
	}
	
	labels <- textGrob(labelsx, y=1-spacerX-marginX, x=cogridx, just=just, rot=gt$grid.labels.rot[1], gp=gpar(col=gt$grid.labels.col, cex=cex, fontface=gt$fontface, fontfamily=gt$fontfamily))
	
	gTree(children = gList(ticks, labels), name = "gridTicksLabelsX")
	
}

plot_grid_labels_y <- function(gt, scale) {
	labelsy <- gt$grid.labels.y

	# find coordinates for projected grid labels
	if (!is.na(gt$grid.projection)) {
		glabelsy <- get_gridline_labels(lco=gt$grid.co.y.lns[gt$grid.sel.y], yax = 0)
		cogridy <- glabelsy$cogrid
		idsy <- glabelsy$ids
		labelsy <- labelsy[idsy]
	} else {
		cogridy <- gt$grid.co.y
	}
	
	cex <- gt$grid.labels.size*scale

	spacerY <- convertWidth(unit(.5, "lines"), unitTo="npc", valueOnly=TRUE) * cex
	marginY <- convertWidth(unit(gt$grid.labels.margin.y, "lines"), unitTo="npc", valueOnly=TRUE) * cex
	
	just <- ifelse(gt$grid.labels.rot[2] == 90, "bottom", ifelse(gt$grid.labels.rot[2] == 270, "top", ifelse(gt$grid.labels.rot[2] == 180, "left", "right")))
	
	if (gt$grid.ticks) {
		ticks <- polylineGrob(x = rep(c(1-spacerY*.5-marginY, 1), length(cogridy)), y = rep(cogridy, each = 2), id = rep(1:length(cogridy), each = 2), gp=gpar(col=gt$grid.col, lwd=gt$grid.lwd))
	} else {
		ticks <- NULL
	}
	labels <- textGrob(labelsy, y=cogridy, x=1-spacerY-marginY, just=just, rot=gt$grid.labels.rot[2], gp=gpar(col=gt$grid.labels.col, cex=cex, fontface=gt$fontface, fontfamily=gt$fontfamily))
	gTree(children = gList(ticks, labels), name = "gridTicksLabelsY")
}



plot_grid <- function(gt, scale, add.labels) {
	
	
	if (gt$grid.labels.inside.frame && gt$grid.ticks) warning("Grid ticks are not supported when labels.inside.frame = TRUE", call. = FALSE)

	
	## might be confusing: gridx are grid lines for the x-axis, so they are vertical
	cogridx <- gt$grid.co.x
	cogridy <- gt$grid.co.y
	labelsx <- gt$grid.labels.x
	labelsy <- gt$grid.labels.y
	

	cex <- gt$grid.labels.size*scale
	
	selx <- (length(cogridx) > 0)
	sely <- (length(cogridy) > 0)
	
	# find margins due to grid labels
	if (add.labels) {
		if (!is.na(gt$frame)) {
			if (gt$frame.double.line) {
				fw <- 6 * convertWidth(unit(1, "points"), unitTo = "npc", valueOnly = TRUE) * gt$frame.lwd
				fh <- 6 * convertHeight(unit(1, "points"), unitTo = "npc", valueOnly = TRUE) * gt$frame.lwd
			} else {
				fw <- convertWidth(unit(1, "points"), unitTo = "npc", valueOnly = TRUE) * gt$frame.lwd
				fh <- convertHeight(unit(1, "points"), unitTo = "npc", valueOnly = TRUE) * gt$frame.lwd
			}
		} else {
			fw <- 0
			fh <- 0
		}
		
		if (gt$grid.labels.rot[1] %in% c(0, 180)) {
			labelsXw <- if (selx) max(text_height_npc(labelsx))  * cex + fh else 0
		} else {
			labelsXw <- if (selx) max(text_width_npc(labelsx, space=FALSE, to_height = TRUE))  * cex + fh else 0
		}
		
		if (gt$grid.labels.rot[2] %in% c(0, 180)) {
			labelsYw <- if (sely) max(text_width_npc(labelsy, space=FALSE))  * cex + fw else 0
		} else {
			labelsYw <- if (sely) max(text_height_npc(labelsy, to_width = TRUE))  * cex + fw else 0
		}
		
		spacerY <- convertWidth(unit(.5, "lines"), unitTo="npc", valueOnly=TRUE) * cex
		spacerX <- convertHeight(unit(.5, "lines"), unitTo="npc", valueOnly=TRUE) * cex
		marginY <- convertWidth(unit(gt$grid.labels.margin.y, "lines"), unitTo="npc", valueOnly=TRUE) * cex
		marginX <- convertWidth(unit(gt$grid.labels.margin.x, "lines"), unitTo="npc", valueOnly=TRUE) * cex
	} else {
		labelsXw <- labelsYw <- spacerX <- spacerY <- marginY <- marginX <- 0
	}	
	# find coordinates for projected grid labels
	if (!is.na(gt$grid.projection)) {
		glabelsx <- if (selx) get_gridline_labels(lco=gt$grid.co.x.lns[gt$grid.sel.x], xax = labelsXw + spacerX+marginX) else numeric(0)
		glabelsy <- if (sely) get_gridline_labels(lco=gt$grid.co.y.lns[gt$grid.sel.y], yax = labelsYw + spacerY+marginY) else numeric(0)

		cogridx <- glabelsx$cogrid
		cogridy <- glabelsy$cogrid
		
		
		idsx <- glabelsx$ids
		idsy <- glabelsy$ids
		
		labelsx <- labelsx[idsx]
		labelsy <- labelsy[idsy]
		
		cogridx_frst <- cogridx[sapply(1:max(idsx), function(i) which(i==idsx)[1])]
		cogridy_frst <- cogridy[sapply(1:max(idsy), function(i) which(i==idsy)[1])]
		
	} else {
		cogridx_frst <- cogridx
		cogridy_frst <- cogridy
	}
	
	# select grid labels to print
	selx2 <- if (selx) (cogridx >= labelsYw + spacerY + marginY & cogridx <= 1 - spacerY) else selx
	sely2 <- if (sely) (cogridy >= labelsXw + spacerX + marginX & cogridy <= 1 - spacerX) else sely
	
	# select grid lines to draw
	selx <- if (selx) (cogridx_frst >= labelsYw + spacerY + marginY & cogridx_frst <= 1) else selx
	sely <- if (sely) (cogridy_frst >= labelsXw + spacerX + marginX & cogridy_frst <= 1) else sely
	
	# crop projected grid lines, and extract polylineGrob ingredients
	if (!is.na(gt$grid.projection)) {
		lnsList <- list(
			if (any(selx)) st_multilinestring(gt$grid.co.x.lns) else NULL,
			if (any(sely)) st_multilinestring(gt$grid.co.y.lns) else NULL
		)
		lnsSel <- !vapply(lnsList, is.null, logical(1))
		if (!any(lnsSel)) {
			grid.co.x.lns <- numeric(0)
			grid.co.y.lns <- numeric(0)
		} else {
			lns <- st_sf(ID=c("x", "y")[lnsSel], geometry = st_sfc(lnsList[lnsSel], crs = 4326)) # trick for 0-1 coordinates
			sf_bbox <- tmaptools::bb_poly(bb(c(labelsYw + spacerY + marginY, labelsXw + spacerX + marginX, 1, 1)), projection = 4326)
			lns_crop <- suppressWarnings(suppressMessages(st_intersection(lns, sf_bbox)))
			if (any(selx)) {
				cogridxlns <- as.data.frame(st_coordinates(st_geometry(lns_crop)[1])[,1:3])
				names(cogridxlns) <- c("x", "y", "ID")
			} else {
				cogridxlns <- numeric(0)
			}
			
			if (any(sely)) {
				cogridylns <- as.data.frame(st_coordinates(st_geometry(lns_crop)[sum(lnsSel)])[,1:3])
				names(cogridylns) <- c("x", "y", "ID")
			} else {
				cogridylns <- numeric(0)
			}
		}
		
	}
	
	## process x-axis grid lines and labels
	if (any(selx)) {
		cogridx2 <- cogridx_frst[selx]
		cogridx3 <- cogridx[selx2]
		labelsx <- labelsx[selx2]
		
		if (!gt$grid.lines) {
			grobGridX <- NULL
		} else if (is.na(gt$grid.projection)) {
			grobGridX <- polylineGrob(x=rep(cogridx2, each=2), y=rep(c(labelsXw+spacerX+marginX,1), length(cogridx2)), 
									  id=rep(1:length(cogridx2), each=2), gp=gpar(col=gt$grid.col, lwd=gt$grid.lwd))
		} else {
			grobGridX <- polylineGrob(x=cogridxlns$x, y=cogridxlns$y, id=cogridxlns$ID, gp=gpar(col=gt$grid.col, lwd=gt$grid.lwd))
		}
		
		grobGridTextX <- if (add.labels && any(selx2)) {
			just <- ifelse(gt$grid.labels.rot[1] == 90, "right", ifelse(gt$grid.labels.rot[1] == 270, "left", ifelse(gt$grid.labels.rot[1] == 180, "bottom", "top")))

			textGrob(labelsx, y=labelsXw+spacerX*.5+marginX, x=cogridx3, just=just, rot=gt$grid.labels.rot[1], gp=gpar(col=gt$grid.labels.col, cex=cex, fontface=gt$fontface, fontfamily=gt$fontfamily))
		} else NULL
	} else {
		grobGridX <- NULL
		grobGridTextX <- NULL
	}
	
	
	## process y-axis grid lines and labels
	if (any(sely)) {
		cogridy2 <- cogridy_frst[sely]
		cogridy3 <- cogridy[sely2]
		labelsy <- labelsy[sely2]
		
		if (!gt$grid.lines) {
			grobGridY <- NULL
		} else if (is.na(gt$grid.projection)) {
			grobGridY <- polylineGrob(y=rep(cogridy2, each=2), x=rep(c(labelsYw+spacerY+marginY,1), length(cogridy2)), 
									  id=rep(1:length(cogridy2), each=2), gp=gpar(col=gt$grid.col, lwd=gt$grid.lwd))
		} else {
			grobGridY <- polylineGrob(x=cogridylns$x, y=cogridylns$y, id=cogridylns$ID, gp=gpar(col=gt$grid.col, lwd=gt$grid.lwd))
		}
		
		grobGridTextY <- if (add.labels && any(sely2)) {
			just <- ifelse(gt$grid.labels.rot[2] == 90, "bottom", ifelse(gt$grid.labels.rot[2] == 270, "top", ifelse(gt$grid.labels.rot[2] == 180, "left", "right")))

			textGrob(labelsy, x=labelsYw+spacerY*.5+marginY, y=cogridy3, just=just, rot=gt$grid.labels.rot[2], gp=gpar(col=gt$grid.labels.col, cex=gt$grid.labels.size*scale, fontface=gt$fontface, fontfamily=gt$fontfamily))
		} else NULL
	} else {
		grobGridY <- NULL
		grobGridTextY <- NULL
	}
	list(treeGridLines=gTree(children=gList(grobGridX, grobGridY), name="grid_lines"),
		 treeGridLabels=gTree(children=gList(grobGridTextX, grobGridTextY), name="grid_labels"),
		 metaX=labelsYw+spacerY,
		 metaY=labelsXw+spacerX)
	
}

get_gridline_labels <- function(lco, xax=NA, yax=NA) {
	k <- length(lco)
	d <- ifelse(!is.na(xax), 1, 2)
	
	
	lns <- st_sf(geometry=st_sfc(lapply(lco, function(l) {
		st_linestring(l)
	})), crs = 4326) # trick for 0-1 coordinates

	# lns <- SpatialLines(mapply(function(m, id){
	# 	Lines(list(Line(m)), ID=id)
	# }, lco, 1:k, SIMPLIFY=FALSE))
	
	
	
	if (!is.na(xax)) {
		ax <- st_sf(geometry=st_sfc(st_linestring(matrix(c(0, 1, xax, xax), nrow=2))), crs = 4326)
		#ax <- SpatialLines(list(Lines(Line(matrix(c(0, 1, xax, xax), nrow=2)), ID="base")))
	} else {
		ax <- st_sf(geometry=st_sfc(st_linestring(matrix(c(yax, yax, 0, 1), nrow=2))), crs = 4326)
		#ax <- SpatialLines(list(Lines(Line(matrix(c(yax, yax, 0, 1), nrow=2)), ID="base")))
	}
	gint <- suppressMessages(st_intersects(lns, ax, sparse = FALSE, prepared = FALSE))[,1]
	
	ins <- vapply(lco, function(m) {
		l <- m[1,]
		if (!is.na(xax)) {
			res <- l[1] >= 0 && l[1] <= 1 && l[2] >= xax && l[2] <= 1
		} else {
			res <- l[1] >= yax && l[1] <= 1 && l[2] >= 0 && l[2] <= 1
		}
		if (res) l[d] else -1
	}, numeric(1))
	cogrid <- ifelse(gint, 0, ins)
	ids <- 1:length(cogrid) # number of grid labels per grid line (could be 2 for warped grid lines)
	if (any(gint)) {
		cogrid <- as.list(cogrid)
		ids <- as.list(ids)
		
		wgint <- which(gint)
		gints <- suppressMessages(st_intersection(lns[gint, ], ax))

		# count number of intersections per coordinate
		gnrs <- vapply(st_geometry(gints), function(g) {
			nr <- nrow(g)
			if (is.null(nr)) 1L else nr
		}, integer(1))
		gints <- suppressWarnings(st_cast(x = st_cast(gints, "MULTIPOINT"), to = "POINT"))

		coor <- st_coordinates(gints)[,d]

		ids2 <- unlist(mapply(rep, 1:length(wgint), gnrs, SIMPLIFY = FALSE))
		j <- 1L
		for (i in which(gint)) {
			cogrid[[i]] <- unname(coor[ids2 == j])
			ids[[i]] <- rep(ids[[i]], gnrs[j])
			j <- j + 1L
		}
		
		cogrid <- unlist(cogrid)
		ids <- unlist(ids)
	}
	list(cogrid = cogrid, ids = ids)
}


plot_symbols <- function(co.native, g, gt, lineInch, lineNatH, lineNatW, i, k) {
	symbolH <- lineNatH * gt$scale
	symbolW <- lineNatW * gt$scale
	shapeLib <- get("shapeLib", envir = .TMAP_CACHE)
	justLib <- get("justLib", envir = .TMAP_CACHE)

	with(g, {
		npol <- nrow(co.native)
		if (length(symbol.size)!=npol) {
			if (length(symbol.size)!=1) warning("less symbol size values than objects", call. = FALSE)
			symbol.size <- rep(symbol.size, length.out=npol)
		}

		size.native.w <- convertWidth(unit(symbol.size, "inch"), "native", valueOnly = TRUE)
		size.native.h <- convertHeight(unit(symbol.size, "inch"), "native", valueOnly = TRUE)

		# determine justification per symbol
		just <- g$symbol.misc$just
		justs <- lapply(symbol.shape, function(ss) {
			if (!is.na(ss) && ss>999) {
				js <- justLib[[ss-999]]
				if (is.na(js[1])) just else js
			} else just
		})
		justs.x <- vapply(justs, "[[", numeric(1), 1)
		justs.y <- vapply(justs, "[[", numeric(1), 2)
		justx <- size.native.w * (justs.x-.5)
		justy <- size.native.h * (justs.y-.5)
		
		# adjust the coordinates
		co.native[, 1] <- co.native[, 1] + symbol.xmod * symbolW + justx * lineNatW * 2 / 3
		co.native[, 2] <- co.native[, 2] + symbol.ymod * symbolH + justy * lineNatH * 2 / 3
		
		sel <- !is.na(symbol.size) & !is.na(symbol.col) & !is.na(symbol.shape)
		
		# return NULL is no symbols are selected (see tm_facets example)
		if (!any(sel)) return(NULL)
		
		if (!all(sel)) {
			co.native <- co.native[sel, , drop=FALSE]
			symbol.size <- symbol.size[sel]
			symbol.col <- symbol.col[sel]
			symbol.shape <- symbol.shape[sel]
			npol <- sum(sel)
		}
		symbol.size <- symbol.size * lineInch
		
		if (length(symbol.size)!=1) {
			decreasing <- order(-symbol.size)
			co.native2 <- co.native[decreasing,,drop=FALSE]
			symbol.size2 <- symbol.size[decreasing]
			symbol.shape2 <- symbol.shape[decreasing]
			symbol.col2 <- symbol.col[decreasing]
		} else {
			co.native2 <- co.native
			symbol.size2 <- symbol.size
			symbol.shape2 <- symbol.shape
			symbol.col2 <- symbol.col
		}

		bordercol <- symbol.border.col
		idName <- paste("tm_symbols", i, k, sep="_")
		
		if (any(!is.na(symbol.shape2) & symbol.shape2>999)) {
			gpars <- get_symbol_gpar(x=symbol.shape2,
									 fill=symbol.col2,
									 col=bordercol,
									 lwd=symbol.border.lwd,
									 separate=TRUE)
			grobs <- lapply(1:npol, function(i) {
				if (!is.na(symbol.shape2[i]) && symbol.shape2[i]>999) {
					grbs <- if (is.na(bordercol)) {
						gList(shapeLib[[symbol.shape2[i]-999]])	
					} else {
						gList(shapeLib[[symbol.shape2[i]-999]], rectGrob(gp=gpar(fill=NA, col=bordercol, lwd=symbol.border.lwd)))	
					}
					gTree(children=grbs, vp=viewport(x=unit(co.native2[i,1], "native"), 
														  y=unit(co.native2[i,2], "native"),
														  width=unit(symbol.size2[i]*2/3, "inch"),
														  height=unit(symbol.size2[i]*2/3, "inch")))
				} else {
					pointsGrob(x=unit(co.native2[i,1], "native"), y=unit(co.native2[i,2], "native"),
							   size=unit(symbol.size2[i], "inch"),
							   pch=symbol.shape2[i],
							   gp=gpars[[i]])
				}
			})
			x <- gTree(children=do.call(gList, grobs), name=idName)
		} else {
			pointsGrob(x=unit(co.native2[,1], "native"), y=unit(co.native2[,2], "native"),
					   size=unit(symbol.size2, "inch"),
					   pch=symbol.shape2,
					   gp=get_symbol_gpar(x=symbol.shape2,
					   				   fill=symbol.col2,
					   				   col=bordercol,
					   				   lwd=symbol.border.lwd), 
					   name=idName)
		}
		
	})
}

plot_text <- function(co.native, g, gt, lineNatH, lineNatW, just=c("center", "center"), bbx = bbx, bg.margin=.10) {
	lineHnative <- lineNatH * gt$scale
	lineWnative <- lineNatW * gt$scale
	
	npol <- nrow(co.native)
	with(g, {
		if (!any(text_sel)) {
			#warning("No text to display. Check if all size values are smaller than lowerbound.size, or if all positions fall outside the plotting area.", call. = FALSE)
			return(NULL)
		}
		
		co.native[, 1] <- co.native[, 1] + text.xmod * lineWnative
		co.native[, 2] <- co.native[, 2] + text.ymod * lineHnative

		
		#grobText <- textGrob(text[text_sel], x=unit(co.native[text_sel,1], "native"), y=unit(co.native[text_sel,2], "native"), just=just, gp=gpar(col=text.color[text_sel], fontface=text.fontface, fontfamily=text.fontfamily))

		# grobText <- pointsGrob(x=unit(co.native[text_sel,1], "native"), y=unit(co.native[text_sel,2], "native"))
		
		grobText <- textGrob(text[text_sel], x=unit(co.native[text_sel,1], "native"), y=unit(co.native[text_sel,2], "native"), just=just, gp=gpar(col=text.color[text_sel], cex=text.size[text_sel], fontface=text.fontface, fontfamily=text.fontfamily))
		nlines <- rep(1, length(text))
		
		
		lineH <- npc_to_native(convertHeight(unit(text.size[text_sel], "lines"), "native", valueOnly=TRUE), scale = bbx[c(2,4)])
		lineW <- npc_to_native(convertWidth(unit(text.size[text_sel], "lines"), "native", valueOnly=TRUE), scale = bbx[c(1,3)])
		
		#		if (!is.na(text.bg.color)) {
		
		tGH <- npc_to_native(mapply(text[text_sel], text.size[text_sel], nlines[text_sel], FUN=function(x,y,z){
			convertHeight(grobHeight(textGrob(x, gp=gpar(cex=y, fontface=text.fontface, fontfamily=text.fontfamily))),"native", valueOnly=TRUE) * z/(z-0.25)}, USE.NAMES=FALSE), scale = bbx[c(2,4)])
		
		tGW <- npc_to_native(mapply(text[text_sel], text.size[text_sel], FUN=function(x,y){
			convertWidth(grobWidth(textGrob(x, gp=gpar(cex=y, fontface=text.fontface, fontfamily=text.fontfamily))),"native", valueOnly=TRUE)}, USE.NAMES=FALSE), scale = bbx[c(1,3)])
		

		justx <- .5 - just[1]
		justy <- .6 - just[2]
		
		tGX <- grobText$x + unit(tGW * justx, "native")
		tGY <- grobText$y + unit(tGH * justy, "native")
		
		tGH <- unit(tGH + lineH * bg.margin, "native")
		tGW <- unit(tGW + lineW * bg.margin, "native")
		grobTextBG <- rectGrob(x=tGX, y=tGY, width=tGW, height=tGH, gp=gpar(fill=text.bg.color, col=NA))
		# 		} else {
		# 			grobTextBG <- NULL
		# 		}
		
		if (text.shadow) {
			grobTextSh <- textGrob(text[text_sel], x=unit(co.native[text_sel,1]+lineW * .05, "native"), y=unit(co.native[text_sel,2]- lineH * .05, "native"), just=just, gp=gpar(col=text.shadowcol[text_sel], cex=text.size[text_sel], fontface=text.fontface, fontfamily=text.fontfamily))
		} else {
			grobTextSh <- NULL
		}
		
		gList(grobTextBG, grobTextSh, grobText)
	})
}

################!!!!! Functions below needed for Advanced text options !!!!####################

.grob2Poly <- function(g) {
	x <- convertX(g$x, unitTo = "native", valueOnly = TRUE)
	y <- convertY(g$y, unitTo = "native", valueOnly = TRUE)
	if (inherits(g, "rect")) {
		w <- convertWidth(g$width, unitTo = "native", valueOnly = TRUE)
		h <- convertHeight(g$height, unitTo = "native", valueOnly = TRUE)
		x1 <- x - .5*w
		x2 <- x + .5*w
		y1 <- y - .5*h
		y2 <- y + .5*h
		polys <- mapply(function(X1, X2, Y1, Y2) {
			st_polygon(list(cbind(c(X1, X2, X2, X1, X1),
						  c(Y2, Y2, Y1, Y1, Y2))))
		}, x1, x2, y1, y2, SIMPLIFY=FALSE)
		st_union(st_sfc(polys))
	} else if (inherits(g, "polygon")) {
		xs <- split(x, g$id)
		ys <- split(y, g$id)
		
		polys <- mapply(function(xi, yi) {
			co <- cbind(xi, yi)
			st_polygon(list(rbind(co, co[1,])))
		}, xs, ys, SIMPLIFY = FALSE)
		st_union(st_sfc(polys))
	} # else return(NULL)

}

polylineGrob2sfLines <- function(gL) {
	k <- length(gL)

	multiLines <- lapply(gL, function(gLi) {
		coords <- cbind(gLi$x, gLi$y)
		
		if (length(gLi$id.lengths) > 1) {
			ids <- unlist(mapply(rep, 1:length(gLi$id.lengths), gLi$id.lengths))
			coords <- mapply(cbind, split(as.numeric(gLi$x), ids), split(as.numeric(gLi$y), ids))
			st_multilinestring(coords)
		} else {
			st_linestring(coords)
		}
	})
	st_sf(geometry = st_sfc(multiLines))
}

npc_to_native <- function(x, scale) {
	x * (scale[2] - scale[1])# + scale[1]
}

native_to_npc_to_native <- function(x, scale) {
	#(x - scale[1]) / (scale[2] - scale[1])
	(x) / (scale[2] - scale[1])
}

.rectGrob2pathGrob <- function(rg, angles) {
	x <- convertX(rg$x, "inch", valueOnly=TRUE)
	y <- convertY(rg$y, "inch", valueOnly=TRUE)
	w <- convertWidth(rg$width, "inch", valueOnly=TRUE)
	h <- convertHeight(rg$height, "inch", valueOnly=TRUE)

	a <- atan2(h, w)
	as <- as.vector(vapply(a, function(a)c(a,pi-a, pi+a,-a), numeric(4)))

	as2 <- as + rep(angles * pi / 180, each=4)

	dst <- rep(sqrt((w/2)^2+(h/2)^2), each=4)

	xs <- rep(x, each=4) + cos(as2) * dst
	ys <- rep(y, each=4) + sin(as2) * dst

	xs2 <- convertX(unit(xs, "inch"), "npc")
	ys2 <- convertY(unit(ys, "inch"), "npc")

	id <- rep(1:length(x), each=4)

	w2 <- w + (h-w) * abs(sin(angles*pi/180))
	h2 <- h + (w-h) * abs(sin(angles*pi/180))

	w3 <- convertWidth(unit(w2, "inch"), "npc")
	h3 <- convertHeight(unit(h2, "inch"), "npc")

	list(poly=polygonGrob(xs2, ys2, id=id, gp=rg$gp),
		 rect=rectGrob(rg$x, rg$y, width = w3, height=h3))
}

.get_direction_angle <- function(co) {
	p1 <- co[1,]
	p2 <- co[nrow(co),]

	a <- atan2(p2[2] - p1[2], p2[1] - p1[1]) * 180 / pi
	if (a < 0) a <- a + 360
	a
}


.editGrob <- function(tg, sel, shiftX, shiftY, angles) {
	nt <- length(sel)
	angles <- rep(angles, length.out=nt)
	if (any(angles!=0)) {
		if (inherits(tg, "rect")) {
			tg <- .rectGrob2pathGrob(tg, angles)$poly
		}
	}
	tgx <- convertX(tg$x, "native", valueOnly = TRUE)
	tgy <- convertY(tg$y, "native", valueOnly = TRUE)

	if (inherits(tg, "polygon")) {
		sel4 <- rep(sel, each=4)
		tg$x <- unit(tgx + rep(shiftX, each=4), "native")[sel4]
		tg$y <- unit(tgy + rep(shiftY, each=4), "native")[sel4]
		tg$id <- rep(1:sum(sel), each=4)
	} else {
		tg$x <- unit(tgx + shiftX, "native")[sel]
		tg$y <- unit(tgy + shiftY, "native")[sel]
		if (inherits(tg, "rect")) {
			tg$height <- tg$height[sel]
			tg$width <- tg$width[sel]
		} else if (inherits(tg, "text")) {
			tg$label <- tg$label[sel]
			tg$rot <- angles[sel]
		}
	}
	tg$gp <- do.call("gpar", lapply(unclass(tg$gp)[names(tg$gp)!="font"], function(g) {
		if (length(g)==nt) g[sel] else g
	}))
	tg
}
