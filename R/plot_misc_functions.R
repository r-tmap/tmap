process_grid <- function(gt, bbx, proj, sasp) {
	grid.n.x <- grid.n.y <- grid.projection <- grid.is.projected <- NULL
	within(gt, { 
		if (!is.na(grid.projection)) {
			bbx_orig <- bbx
			bbx <- bb(bbx, current.projection = proj, projection = grid.projection)
		}
		
		## automatically determine number of grid lines
		if (is.na(grid.n.x) && !is.na(grid.n.y)) {
			grid.n.x <- grid.n.y * sasp
		} else if (!is.na(grid.n.x) && is.na(grid.n.y)) {
			grid.n.y <- grid.n.x / sasp
		} else if (is.na(grid.n.x) && is.na(grid.n.y)) {
			grid.n.lines <- 20 / gt$scale
			grid.n.x <- round(sasp * (grid.n.lines/(1+sasp)))
			grid.n.y <- round(grid.n.lines / (1+sasp))
		}
		
		## find natural breaks
		grid.custom.x <- !is.na(grid.x[1])
		grid.custom.y <- !is.na(grid.y[1])
		
		if (!grid.custom.x) grid.x <- pretty(bbx[1,], n=grid.n.x)
		if (!grid.custom.y) grid.y <- pretty(bbx[2,], n=grid.n.y)

		## copy grid.x and y
		grid.x.orig <- grid.x
		grid.y.orig <- grid.y
		
		## crop
		grid.x <- grid.x[grid.x>bbx[1,1] & grid.x<bbx[1,2]]
		grid.y <- grid.y[grid.y>bbx[2,1] & grid.y<bbx[2,2]]
		
		## project grid lines
		if (!is.na(grid.projection)) {
			## add extra grid lines to make sure the warped grid is full
			if (grid.custom.x) {
				grid.x2 <- grid.x.orig
			} else {
				gnx2 <- floor(length(grid.x))
				if (gnx2>0) {
					grid.x2 <- c(rev(seq(grid.x[1], by=-diff(grid.x[1:2]), length.out = gnx2)),
								 grid.x[-c(1, length(grid.x))],
								 seq(grid.x[length(grid.x)], by=diff(grid.x[1:2]), length.out = gnx2))
				} else grid.x2 <- NA
			}
			if (grid.custom.y) {
				grid.y2 <- grid.y.orig
			} else {
				gny2 <- floor(length(grid.y))
				if (gny2>0) {
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
			grid.x2.min <- min(min(grid.x2), bbx[1, 1], na.rm=TRUE)
			grid.x2.max <- max(max(grid.x2), bbx[1, 2], na.rm=TRUE)
			grid.y2.min <- min(min(grid.y2), bbx[2, 1], na.rm=TRUE)
			grid.y2.max <- max(max(grid.y2), bbx[2, 2], na.rm=TRUE)
			
			# create SLDF with straight grid lines in grid lines projection
			#bbx2 <- bb(bbx, ext=3)
			lnsList <- list(
				if (is.na(grid.x2[1])) NULL else Lines(lapply(grid.x2, function(x) {
					m <- matrix(c(rep(x,100), seq(grid.y2.min, grid.y2.max, length.out=100)), ncol=2)
					Line(m)
				}), ID="x"),
				if (is.na(grid.y2[1])) NULL else Lines(lapply(grid.y2, function(y) {
					m <- matrix(c(seq(grid.x2.min, grid.x2.max, length.out=100), rep(y,100)), ncol=2)
					Line(m)
				}), ID="y")
			)
			
			lnsSel <- !sapply(lnsList, is.null)
			if (!any(lnsSel)) {
				grid.co.x.lns <- numeric(0)
				grid.co.y.lns <- numeric(0)
			} else {
				lns <- SpatialLinesDataFrame(SpatialLines(lnsList[lnsSel], proj4string = get_proj4(grid.projection, as.CRS = TRUE)), data.frame(ID=c("x", "y")[lnsSel]), match.ID=FALSE)
				
				# project it to current projection
				lns_proj <- set_projection(lns, projection = proj)
				
				# extract and normalize coordinates
				grid.co.x.lns <- if (lnsSel[1]) lapply(lns_proj@lines[[1]]@Lines, function(l) {
					lco <- attr(l, "coords")
					lco[, 1] <- (lco[, 1]-bbx_orig[1,1]) / (bbx_orig[1,2] - bbx_orig[1,1])
					lco[, 2] <- (lco[, 2]-bbx_orig[2,1]) / (bbx_orig[2,2] - bbx_orig[2,1])
					lco
				}) else numeric(0)
				grid.co.y.lns <- if (lnsSel[2]) lapply(lns_proj@lines[[sum(lnsSel)]]@Lines, function(l) {
					lco <- attr(l, "coords")
					lco[, 1] <- (lco[, 1]-bbx_orig[1,1]) / (bbx_orig[1,2] - bbx_orig[1,1])
					lco[, 2] <- (lco[, 2]-bbx_orig[2,1]) / (bbx_orig[2,2] - bbx_orig[2,1])
					lco
				}) else numeric(0)
			}
			lns <- NULL
			lns_proj <- NULL
		} else {
			# normalize coordinates
			grid.co.x <- (grid.x-bbx[1,1]) / (bbx[1,2] - bbx[1,1])
			grid.co.y <- (grid.y-bbx[2,1]) / (bbx[2,2] - bbx[2,1])
			
		}
		
		## format grid labels
		grid.labels.x <- do.call("fancy_breaks", c(list(vec=grid.x, intervals=FALSE), gt$grid.labels.format)) #format(grid.x, big.mark = ",")
		grid.labels.y <- do.call("fancy_breaks", c(list(vec=grid.y, intervals=FALSE), gt$grid.labels.format)) #format(grid.y, big.mark = ",")
		
	})
}

plot_grid_labels_x <- function(gt, scale) {
	# find coordinates for projected grid labels
	if (!is.na(gt$grid.projection)) {
		cogridx <- get_gridline_labels(lco=gt$grid.co.x.lns[gt$grid.sel.x], xax = 0)
	} else {
		cogridx <- gt$grid.co.x	
	}
	labelsx <- gt$grid.labels.x
	
	cex <- gt$grid.labels.size*scale
	
	spacerX <- convertHeight(unit(.5, "lines"), unitTo="npc", valueOnly=TRUE) * cex
	marginX <- convertWidth(unit(gt$grid.labels.margin.x, "lines"), unitTo="npc", valueOnly=TRUE) * cex

	just <- ifelse(gt$grid.labels.rot[1] == 90, "right", ifelse(gt$grid.labels.rot[1] == 270, "left", ifelse(gt$grid.labels.rot[1] == 180, "bottom", "top")))
	
	textGrob(labelsx, y=1-spacerX-marginX, x=cogridx, just=just, rot=gt$grid.labels.rot[1], gp=gpar(col=gt$grid.labels.col, cex=cex, fontface=gt$fontface, fontfamily=gt$fontfamily))
	
}

plot_grid_labels_y <- function(gt, scale) {
	# find coordinates for projected grid labels
	if (!is.na(gt$grid.projection)) {
		cogridy <- get_gridline_labels(lco=gt$grid.co.y.lns[gt$grid.sel.y], yax = 0)
	} else {
		cogridy <- gt$grid.co.y
	}
	labelsy <- gt$grid.labels.y
	
	cex <- gt$grid.labels.size*scale

	spacerY <- convertWidth(unit(.5, "lines"), unitTo="npc", valueOnly=TRUE) * cex
	marginY <- convertWidth(unit(gt$grid.labels.margin.y, "lines"), unitTo="npc", valueOnly=TRUE) * cex
	
	just <- ifelse(gt$grid.labels.rot[2] == 90, "bottom", ifelse(gt$grid.labels.rot[2] == 270, "top", ifelse(gt$grid.labels.rot[2] == 180, "left", "right")))
	
	textGrob(labelsy, y=cogridy, x=1-spacerY-marginY, just=just, rot=gt$grid.labels.rot[2], gp=gpar(col=gt$grid.labels.col, cex=cex, fontface=gt$fontface, fontfamily=gt$fontfamily))
}


plot_grid <- function(gt, scale, add.labels) {
	
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
		cogridx <- if (selx) get_gridline_labels(lco=gt$grid.co.x.lns[gt$grid.sel.x], xax = labelsXw + spacerX+marginX) else numeric(0)
		cogridy <- if (sely) get_gridline_labels(lco=gt$grid.co.y.lns[gt$grid.sel.y], yax = labelsYw + spacerY+marginY) else numeric(0)
	}
	
	# select grid labels to print
	selx2 <- if (selx) (cogridx >= labelsYw + spacerY + marginY & cogridx <= 1 - spacerY) else selx
	sely2 <- if (sely) (cogridy >= labelsXw + spacerX + marginX & cogridy <= 1 - spacerX) else sely
	
	# select grid lines to draw
	selx <- if (selx) (cogridx >= labelsYw + spacerY + marginY & cogridx <= 1) else selx
	sely <- if (sely) (cogridy >= labelsXw + spacerX + marginX & cogridy <= 1) else sely
	
	# crop projected grid lines, and extract polylineGrob ingredients
	if (!is.na(gt$grid.projection)) {
		lnsList <- list(
			if (any(selx)) Lines(lapply(gt$grid.co.x.lns, function(m) {
				Line(m)
			}), ID="x") else NULL,
			if (any(sely)) Lines(lapply(gt$grid.co.y.lns, function(m) {
				Line(m)
			}), ID="y") else NULL
		)
		lnsSel <- !sapply(lnsList, is.null)
		if (!any(lnsSel)) {
			grid.co.x.lns <- numeric(0)
			grid.co.y.lns <- numeric(0)
		} else {
			lns <- SpatialLinesDataFrame(SpatialLines(lnsList[lnsSel]), data.frame(ID=c("x", "y")[lnsSel]), match.ID=FALSE)
			lns_crop <- raster::crop(lns, bb(c(labelsYw + spacerY + marginY, 1, labelsXw + spacerX + marginX, 1)))
			
			cogridxlns <- if (any(selx)) do.call("rbind", mapply(function(l, i) {
				co <- as.data.frame(attr(l, "coords"))
				co$ID <- i
				co
			}, lns_crop@lines[[1]]@Lines, 1:length(lns_crop@lines[[1]]@Lines), SIMPLIFY=FALSE)) else numeric(0)
			
			cogridylns <- if (any(sely)) do.call("rbind", mapply(function(l, i) {
				co <- as.data.frame(attr(l, "coords"))
				co$ID <- i
				co
			}, lns_crop@lines[[sum(lnsSel)]]@Lines, 1:length(lns_crop@lines[[sum(lnsSel)]]@Lines), SIMPLIFY=FALSE)) else numeric(0)
			
		}
		
	}
	
	## process x-axis grid lines and labels
	if (any(selx)) {
		cogridx2 <- cogridx[selx]
		cogridx3 <- cogridx[selx2]
		labelsx <- labelsx[selx2]
		
		if (is.na(gt$grid.projection)) {
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
		cogridy2 <- cogridy[sely]
		cogridy3 <- cogridy[sely2]
		labelsy <- labelsy[sely2]
		
		if (is.na(gt$grid.projection)) {
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
	lns <- SpatialLines(mapply(function(m, id){
		Lines(list(Line(m)), ID=id)
	}, lco, 1:k, SIMPLIFY=FALSE))
	if (!is.na(xax)) {
		ax <- SpatialLines(list(Lines(Line(matrix(c(0, 1, xax, xax), nrow=2)), ID="base")))
	} else {
		ax <- SpatialLines(list(Lines(Line(matrix(c(yax, yax, 0, 1), nrow=2)), ID="base")))
	}
	gint <- gIntersects(lns, ax, byid = TRUE)
	ins <- sapply(lco, function(m) {
		l <- m[1,]
		if (!is.na(xax)) {
			res <- l[1] >= 0 && l[1] <= 1 && l[2] >= xax && l[2] <= 1
		} else {
			res <- l[1] >= yax && l[1] <= 1 && l[2] >= 0 && l[2] <= 1
		}
		if (res) l[d] else -1
	})
	cogrid <- ifelse(gint, 0, ins)
	if (any(gint)) {
		gints <- gIntersection(lns[gint, ], ax, byid = TRUE)
		cogrid[gint] <- gints@coords[,d]
	}
	cogrid
}


plot_symbols <- function(co.npc, g, gt, lineInch, i, k) {
	symbolH <- convertHeight(unit(lineInch, "inch"), "npc", valueOnly=TRUE) * gt$scale
	symbolW <- convertWidth(unit(lineInch, "inch"), "npc", valueOnly=TRUE) * gt$scale
	shapeLib <- get(".shapeLib", envir = .TMAP_CACHE)
	justLib <- get(".justLib", envir = .TMAP_CACHE)

	with(g, {
		npol <- nrow(co.npc)
		if (length(symbol.size)!=npol) {
			if (length(symbol.size)!=1) warning("less symbol size values than objects", call. = FALSE)
			symbol.size <- rep(symbol.size, length.out=npol)
		}

		size.npc.w <- convertWidth(unit(symbol.size, "inch"), "npc", valueOnly = TRUE)
		size.npc.h <- convertHeight(unit(symbol.size, "inch"), "npc", valueOnly = TRUE)

		# determine justification per symbol
		just <- g$symbol.misc$just
		justs <- lapply(symbol.shape, function(ss) {
			if (!is.na(ss) && ss>999) {
				js <- justLib[[ss-999]]
				if (is.na(js[1])) just else js
			} else just
		})
		justs.x <- sapply(justs, "[[", 1)
		justs.y <- sapply(justs, "[[", 2)
		justx <- size.npc.w * (justs.x-.5)
		justy <- size.npc.h * (justs.y-.5)
		
		# adjust the coordinates
		co.npc[, 1] <- co.npc[, 1] + symbol.xmod * symbolW + justx * lineInch * 2 / 3
		co.npc[, 2] <- co.npc[, 2] + symbol.ymod * symbolH + justy * lineInch * 2 / 3
		
		sel <- !is.na(symbol.size) & !is.na(symbol.col) & !is.na(symbol.shape)
		
		# return NULL is no symbols are selected (see tm_facets example)
		if (!any(sel)) return(NULL)
		
		if (!all(sel)) {
			co.npc <- co.npc[sel, , drop=FALSE]
			symbol.size <- symbol.size[sel]
			symbol.col <- symbol.col[sel]
			symbol.shape <- symbol.shape[sel]
		}
		symbol.size <- symbol.size * lineInch
		
		if (length(symbol.size)!=1) {
			decreasing <- order(-symbol.size)
			co.npc2 <- co.npc[decreasing,,drop=FALSE]
			symbol.size2 <- symbol.size[decreasing]
			symbol.shape2 <- symbol.shape[decreasing]
			symbol.col2 <- symbol.col[decreasing]
		} else {
			co.npc2 <- co.npc
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
					gTree(children=grbs, vp=viewport(x=unit(co.npc2[i,1], "npc"), 
														  y=unit(co.npc2[i,2], "npc"),
														  width=unit(symbol.size2[i]*2/3, "inch"),
														  height=unit(symbol.size2[i]*2/3, "inch")))
				} else {
					pointsGrob(x=unit(co.npc2[i,1], "npc"), y=unit(co.npc2[i,2], "npc"),
							   size=unit(symbol.size2[i], "inch"),
							   pch=symbol.shape2[i],
							   gp=gpars[[i]])
				}
			})
			x <- gTree(children=do.call(gList, grobs), name=idName)
		} else {
			pointsGrob(x=unit(co.npc2[,1], "npc"), y=unit(co.npc2[,2], "npc"),
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

plot_text <- function(co.npc, g, gt, lineInch, just=c("center", "center"), bg.margin=.10) {
	lineHnpc <- convertHeight(unit(lineInch, "inch"), "npc", valueOnly=TRUE) * gt$scale
	lineWnpc <- convertWidth(unit(lineInch, "inch"), "npc", valueOnly=TRUE) * gt$scale
	
	npol <- nrow(co.npc)
	with(g, {
		if (!any(text_sel)) {
			warning("No text to display. Check if all size values are smaller than lowerbound.size, or if all positions fall outside the plotting area.", call. = FALSE)
			return(NULL)
		}
		
		co.npc[, 1] <- co.npc[, 1] + text.xmod * lineWnpc
		co.npc[, 2] <- co.npc[, 2] + text.ymod * lineHnpc
		
		grobText <- textGrob(text[text_sel], x=unit(co.npc[text_sel,1], "npc"), y=unit(co.npc[text_sel,2], "npc"), just=just, gp=gpar(col=text.color[text_sel], cex=text.size[text_sel], fontface=text.fontface, fontfamily=text.fontfamily))
		nlines <- rep(1, length(text))
		
		
		lineH <- convertHeight(unit(text.size[text_sel], "lines"), "npc", valueOnly=TRUE)
		lineW <- convertWidth(unit(text.size[text_sel], "lines"), "npc", valueOnly=TRUE)
		
		#		if (!is.na(text.bg.color)) {
		
		
		tGH <- mapply(text[text_sel], text.size[text_sel], nlines[text_sel], FUN=function(x,y,z){
			convertHeight(grobHeight(textGrob(x, gp=gpar(cex=y, fontface=text.fontface, fontfamily=text.fontfamily))),"npc", valueOnly=TRUE) * z/(z-0.25)}, USE.NAMES=FALSE)
		
		tGW <- mapply(text[text_sel], text.size[text_sel], FUN=function(x,y){
			convertWidth(grobWidth(textGrob(x, gp=gpar(cex=y, fontface=text.fontface, fontfamily=text.fontfamily))),"npc", valueOnly=TRUE)}, USE.NAMES=FALSE)
		tGX <- grobText$x + unit(ifelse(just[1]=="left", (tGW * .5), 
										ifelse(just[1]=="right", -(tGW * .5), 0)), "npc")
		tGY <- grobText$y + unit(tGH * ifelse(just[2]=="top", -.5, 
									   ifelse(just[2]=="bottom", .5, -.05)), "npc")
		
		tGH <- tGH + lineH * bg.margin
		tGW <- tGW + lineW * bg.margin
		grobTextBG <- rectGrob(x=tGX, y=tGY, width=tGW, height=tGH, gp=gpar(fill=text.bg.color, col=NA))
		# 		} else {
		# 			grobTextBG <- NULL
		# 		}
		
		if (text.shadow) {
			grobTextSh <- textGrob(text[text_sel], x=unit(co.npc[text_sel,1]+lineW * .05, "npc"), y=unit(co.npc[text_sel,2]- lineH * .05, "npc"), just=just, gp=gpar(col=text.shadowcol[text_sel], cex=text.size[text_sel], fontface=text.fontface, fontfamily=text.fontfamily))
		} else {
			grobTextSh <- NULL
		}
		
		gList(grobTextBG, grobTextSh, grobText)
	})
}





.grob2Poly <- function(g) {
	x <- convertX(g$x, unitTo = "npc", valueOnly = TRUE)
	y <- convertY(g$y, unitTo = "npc", valueOnly = TRUE)
	if (inherits(g, "rect")) {
		w <- convertWidth(g$width, unitTo = "npc", valueOnly = TRUE)
		h <- convertHeight(g$height, unitTo = "npc", valueOnly = TRUE)
		x1 <- x - .5*w
		x2 <- x + .5*w
		y1 <- y - .5*h
		y2 <- y + .5*h
		polys <- mapply(function(X1, X2, Y1, Y2) {
			Polygon(cbind(c(X1, X2, X2, X1, X1),
						  c(Y2, Y2, Y1, Y1, Y2)))
		}, x1, x2, y1, y2, SIMPLIFY=FALSE)
	} else if (inherits(g, "polygon")) {
		xs <- split(x, g$id)	
		ys <- split(y, g$id)	
		polys <- mapply(function(X, Y) {
			Polygon(cbind(X[c(1:4,1)],
						  Y[c(1:4,1)]))
		}, xs, ys, SIMPLIFY=FALSE)
	} else return(NULL)
	
	ids <- 1:length(polys)
	gUnaryUnion(SpatialPolygons(mapply(function(p, i) {
		Polygons(list(p), ID=i)
	}, polys, ids, SIMPLIFY=FALSE)))
}

polylineGrob2Lines <- function(gL) {
	k <- length(gL)
	
	SpatialLines(lapply(1:k, function(i) {
		g <- gL[[i]]
		x <- convertX(g$x, "npc", valueOnly = TRUE)
		y <- convertY(g$y, "npc", valueOnly = TRUE)
		id <- factor(g$id)
		xs <- split(x, f = id)
		ys <- split(y, f = id)
		ids <- levels(id)
		
		Lines(mapply(function(X, Y) {
			Line(cbind(X,Y))	
		}, xs, ys, SIMPLIFY=FALSE), ID=i)
	}))
}

.rectGrob2pathGrob <- function(rg, angles) {
	x <- convertX(rg$x, "inch", valueOnly=TRUE)
	y <- convertY(rg$y, "inch", valueOnly=TRUE)
	w <- convertWidth(rg$width, "inch", valueOnly=TRUE)
	h <- convertHeight(rg$height, "inch", valueOnly=TRUE)
	
	a <- atan2(h, w)
	
	as <- as.vector(sapply(a, function(a)c(a,pi-a, pi+a,-a)))
	
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
	tgx <- convertX(tg$x, "npc", valueOnly = TRUE)
	tgy <- convertY(tg$y, "npc", valueOnly = TRUE)
	
	if (inherits(tg, "polygon")) {
		sel4 <- rep(sel, each=4)
		tg$x <- unit(tgx + rep(shiftX, each=4), "npc")[sel4]
		tg$y <- unit(tgy + rep(shiftY, each=4), "npc")[sel4]
		tg$id <- rep(1:sum(sel), each=4)
	} else {
		tg$x <- unit(tgx + shiftX, "npc")[sel]
		tg$y <- unit(tgy + shiftY, "npc")[sel]
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
