grid_nonoverlap <- function(x, s) {
	n = length(x)
	x = c(-x[1], x, 1 + (1 - x[n]))
	
	m_left = x[2:(n+1)] - x[1:n]
	m_right = x[3:(n+2)] - x[2:(n+1)]
	m = pmin(m_left, m_right)
	
	m > s
}

pretty30 = function(x, n, longlat) {
	p = pretty(x, n)
	if (!longlat) return(p)
	step = p[2] - p[1]
	if (step > 50) {
		x = p %/% 60 * 60
		seq(min(x), max(x), by = 60)
	} else if (step > 10) {
		x = p %/% 30 * 30
		seq(min(x), max(x), by = 30)
	} else {
		p
	}
}

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
		
		if (!grid.custom.x) grid.x <- pretty30(bbx[c(1,3)], n=grid.n.x, longlat = !is.na(grid.projection) || sf::st_is_longlat(proj))
		if (!grid.custom.y) grid.y <- pretty30(bbx[c(2,4)], n=grid.n.y, longlat = !is.na(grid.projection) || sf::st_is_longlat(proj))
		
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
				# grid.x2[abs(grid.x2-180)<1e-9] <- 180
				# grid.x2[abs(grid.x2- -180)<1e-9] <- -180
				# grid.y2[abs(grid.y2-90)<1e-9] <- 90
				# grid.y2[abs(grid.y2- -90)<1e-9] <- -90
				grid.x2 <- grid.x2[grid.x2>=-180 & grid.x2<=180]	
				grid.y2 <- grid.y2[grid.y2>=-90 & grid.y2<=90]	
			}
			gnx2 <- gny2 <- NULL
			
			## determine limits
			grid.x2.min <- min(min(grid.x2), bbx[1], na.rm=TRUE)
			grid.x2.max <- max(max(grid.x2), bbx[3], na.rm=TRUE)
			grid.y2.min <- min(min(grid.y2), bbx[2], na.rm=TRUE)
			grid.y2.max <- max(max(grid.y2), bbx[4], na.rm=TRUE)
			
			lnsSel <- c(length(grid.x2) && !is.na(grid.x2[1]),
						length(grid.y2) && !is.na(grid.y2[1]))
			
			if (lnsSel[1]) {
				lnsX = sf::st_sfc(lapply(grid.x2, function(x) {
					sf::st_linestring(matrix(c(rep(x,grid.ndiscr), seq(grid.y2.min, grid.y2.max, length.out=grid.ndiscr)), ncol=2))
				}), crs = grid.projection)
				lnsX_proj <- sf::st_transform(lnsX, crs = proj)
				lnsX_emp <- sf::st_is_empty(lnsX_proj)
				
				grid.x2 <- grid.x2[!lnsX_emp]
				lnsX_proj <- lnsX_proj[!lnsX_emp]
				xco <- st_coordinates(lnsX_proj)
				grid.co.x.lns <- lapply(unique(xco[,3]), function(i) {
					lco <- xco[xco[,3]==i, 1:2]
					lco[, 1] <- (lco[, 1]-bbx_orig[1]) / (bbx_orig[3] - bbx_orig[1])
					lco[, 2] <- (lco[, 2]-bbx_orig[2]) / (bbx_orig[4] - bbx_orig[2])
					lco
				})
				lnsX <- NULL
				lnsX_proj <- NULL
				lnsX_emp <- NULL
				
				grid.sel.x <- which(grid.x2 %in% grid.x)
			} else {
				grid.co.x.lns <- numeric(0)
			}
			
			if (lnsSel[2]) {
				lnsY = sf::st_sfc(lapply(grid.y2, function(y) {
					st_linestring(matrix(c(seq(grid.x2.min, grid.x2.max, length.out=grid.ndiscr), rep(y,grid.ndiscr)), ncol=2))
				}), crs = grid.projection)
				lnsY_proj <- sf::st_transform(lnsY, crs = proj)
				lnsY_emp <- sf::st_is_empty(lnsY_proj)
				
				grid.y2 <- grid.y2[!lnsY_emp]
				lnsY_proj <- lnsY_proj[!lnsY_emp]
				yco <- st_coordinates(lnsY_proj)
				grid.co.y.lns <- lapply(unique(yco[,3]), function(i) {
					lco <- yco[yco[,3]==i, 1:2]
					lco[, 1] <- (lco[, 1]-bbx_orig[1]) / (bbx_orig[3] - bbx_orig[1])
					lco[, 2] <- (lco[, 2]-bbx_orig[2]) / (bbx_orig[4] - bbx_orig[2])
					lco
				})
				lnsY <- NULL
				lnsY_proj <- NULL
				lnsY_emp <- NULL
				
				grid.sel.y <- which(grid.y2 %in% grid.y)
			}	else {
				grid.co.y.lns <- numeric(0)
			}


			grid.labels.show <- grid.labels.show & lnsSel # update needed for plot_n
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
	
	# remove overlapping grid labels
	widths = text_width_npc(labelsx) * cex
	selx2 = grid_nonoverlap(cogridx, widths)
	if (!any(selx2)) return(NULL)
	labelsx2 = labelsx[selx2]
	cogridx2 = cogridx[selx2]
	
	
	spacerX <- convertHeight(unit(.5, "lines"), unitTo="npc", valueOnly=TRUE) * cex
	marginX <- convertHeight(unit(gt$grid.labels.margin.x, "lines"), unitTo="npc", valueOnly=TRUE) * cex
	
	just <- ifelse(gt$grid.labels.rot[1] == 90, "right", ifelse(gt$grid.labels.rot[1] == 270, "left", ifelse(gt$grid.labels.rot[1] == 180, "bottom", "top")))
	
	if (gt$grid.ticks[1]) {
		ticks <- polylineGrob(x=rep(cogridx2, each = 2), y = rep(c(1-spacerX*.5-marginX,1), length(cogridx2)), id = rep(1:length(cogridx2), each = 2), gp=gpar(col=gt$grid.col, lwd=gt$grid.lwd))	
	} else {
		ticks <- NULL
	}
	
	labels <- textGrob(labelsx2, y=1-spacerX-marginX, x=cogridx2, just=just, rot=gt$grid.labels.rot[1], gp=gpar(col=gt$grid.labels.col, cex=cex, fontface=gt$fontface, fontfamily=gt$fontfamily))
	
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
	
	# remove overlapping grid labels
	heights = text_height_npc(labelsy) * cex
	sely2 = grid_nonoverlap(cogridy, heights)
	if (!any(sely2)) return(NULL)
	labelsy2 = labelsy[sely2]
	cogridy2 = cogridy[sely2]
	
	
	spacerY <- convertWidth(unit(.5, "lines"), unitTo="npc", valueOnly=TRUE) * cex
	marginY <- convertWidth(unit(gt$grid.labels.margin.y, "lines"), unitTo="npc", valueOnly=TRUE) * cex
	
	just <- ifelse(gt$grid.labels.rot[2] == 90, "bottom", ifelse(gt$grid.labels.rot[2] == 270, "top", ifelse(gt$grid.labels.rot[2] == 180, "left", "right")))

	if (gt$grid.ticks[2]) {
		ticks <- polylineGrob(x = rep(c(1-spacerY*.5-marginY, 1), length(cogridy2)), y = rep(cogridy2, each = 2), id = rep(1:length(cogridy2), each = 2), gp=gpar(col=gt$grid.col, lwd=gt$grid.lwd))
	} else {
		ticks <- NULL
	}
	labels <- textGrob(labelsy2, y=cogridy2, x=1-spacerY-marginY, just=just, rot=gt$grid.labels.rot[2], gp=gpar(col=gt$grid.labels.col, cex=cex, fontface=gt$fontface, fontfamily=gt$fontfamily))
	gTree(children = gList(ticks, labels), name = "gridTicksLabelsY")
}



plot_grid <- function(gt, scale, add.labels) {
	if (gt$grid.labels.inside.frame && any(gt$grid.ticks) && gt$show.warnings) warning("Grid ticks are not supported when labels.inside.frame = TRUE", call. = FALSE)
	
	
	## might be confusing: gridx are grid lines for the x-axis, so they are vertical
	cogridx <- gt$grid.co.x
	cogridy <- gt$grid.co.y
	labelsx <- gt$grid.labels.x
	labelsy <- gt$grid.labels.y
	
	
	cex <- gt$grid.labels.size*scale
	
	selx <- (length(cogridx) > 0)
	sely <- (length(cogridy) > 0)
	
	# find margins due to grid labels
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
	
	
	if (add.labels[1]) {
		if (gt$grid.labels.rot[1] %in% c(0, 180)) {
			labelsXw <- if (selx) max(text_height_npc(labelsx))  * cex + fh else 0
		} else {
			labelsXw <- if (selx) max(text_width_npc(labelsx, space=FALSE, to_height = TRUE))  * cex + fh else 0
		}
		spacerX <- convertHeight(unit(.5, "lines"), unitTo="npc", valueOnly=TRUE) * cex
		marginX <- convertWidth(unit(gt$grid.labels.margin.x, "lines"), unitTo="npc", valueOnly=TRUE) * cex
	} else {
		labelsXw <- spacerX <- marginX <- 0
	}
	
	
	if (add.labels[2]) {
		if (gt$grid.labels.rot[2] %in% c(0, 180)) {
			labelsYw <- if (sely) max(text_width_npc(labelsy, space=FALSE))  * cex + fw else 0
		} else {
			labelsYw <- if (sely) max(text_height_npc(labelsy, to_width = TRUE))  * cex + fw else 0
		}
		spacerY <- convertWidth(unit(.5, "lines"), unitTo="npc", valueOnly=TRUE) * cex
		marginY <- convertWidth(unit(gt$grid.labels.margin.y, "lines"), unitTo="npc", valueOnly=TRUE) * cex
	} else {
		labelsYw <- spacerY <- marginY <- 0
	}	
	
	# find coordinates for projected grid labels
	if (!is.na(gt$grid.projection)) {
		if (selx) {
			glabelsx <- get_gridline_labels(lco=gt$grid.co.x.lns[gt$grid.sel.x], xax = labelsXw + spacerX+marginX)
			cogridx <- glabelsx$cogrid
			idsx <- glabelsx$ids
			labelsx <- labelsx[idsx]
			cogridx_frst <- cogridx[sapply(1:max(idsx), function(i) which(i==idsx)[1])]
		}
		# } else {
		# 	glabelsx <- numeric(0)
		# }
		
		if (sely) {
			glabelsy <- get_gridline_labels(lco=gt$grid.co.y.lns[gt$grid.sel.y], yax = labelsYw + spacerY+marginY)
			cogridy <- glabelsy$cogrid
			idsy <- glabelsy$ids
			labelsy <- labelsy[idsy]
			cogridy_frst <- cogridy[sapply(1:max(idsy), function(i) which(i==idsy)[1])]
		}
		
	} else {
		cogridx_frst <- cogridx
		cogridy_frst <- cogridy
	}
	
	# select grid labels to print
	selx2 <- if (selx) (cogridx >= labelsYw + spacerY + marginY & cogridx <= 1 - spacerY) else selx
	sely2 <- if (sely) (cogridy >= labelsXw + spacerX + marginX & cogridy <= 1 - spacerX) else sely
	
	
	# remove overlapping grid labels
	widths = text_width_npc(labelsx) * cex
	heights = text_height_npc(labelsy) * cex
	
	
	selx2 = selx2 & grid_nonoverlap(cogridx, widths)
	sely2 = sely2 & grid_nonoverlap(cogridy, heights)
	

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
		
		grobGridTextX <- if (add.labels[1] && any(selx2)) {
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
		
		grobGridTextY <- if (add.labels[2] && any(sely2)) {
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
		
		ids2 <- unlist(mapply(rep, 1:length(wgint), gnrs, SIMPLIFY = FALSE), use.names = FALSE)
		j <- 1L
		for (i in which(gint)) {
			cogrid[[i]] <- unname(coor[ids2 == j])
			ids[[i]] <- rep(ids[[i]], gnrs[j])
			j <- j + 1L
		}
		
		cogrid <- unlist(cogrid, use.names = FALSE)
		ids <- unlist(ids, use.names = FALSE)
	}
	list(cogrid = cogrid, ids = ids)
}
