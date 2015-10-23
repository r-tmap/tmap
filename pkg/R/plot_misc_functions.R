process_grid <- function(gt, bbx, proj, sasp) {
	grid.n.x <- grid.n.y <- grid.projection <- NULL
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
		grid.x <- pretty(bbx[1,], n=grid.n.x)
		grid.x <- grid.x[grid.x>bbx[1,1] & grid.x<bbx[1,2]]
		grid.y <- pretty(bbx[2,], n=grid.n.y)
		grid.y <- grid.y[grid.y>bbx[2,1] & grid.y<bbx[2,2]]
		
		## project grid lines
		if (!is.na(grid.projection)) {
			# create SLDF with straight grid lines in grid lines projection
			lns <- SpatialLinesDataFrame(SpatialLines(list(
				Lines(lapply(grid.x, function(x) {
					m <- matrix(c(rep(x,100), seq(bbx[2,1], bbx[2,2], length.out=100)), ncol=2)
					Line(m)
				}), ID="x"),
				Lines(lapply(grid.y, function(y) {
					m <- matrix(c(seq(bbx[1,1], bbx[1,2], length.out=100), rep(y,100)), ncol=2)
					Line(m)
				}), ID="y")
			), proj4string = CRS(get_proj4_code(grid.projection))), data.frame(ID=c("x", "y")), match.ID=FALSE)
			
			# project it to current projection
			lns_proj <- set_projection(lns, projection = proj)
			
			# extract and normalize coordinates
			grid.co.x.lns <- lapply(lns_proj@lines[[1]]@Lines, function(l) {
				lco <- attr(l, "coords")
				lco[, 1] <- (lco[, 1]-bbx_orig[1,1]) / (bbx_orig[1,2] - bbx_orig[1,1])
				lco[, 2] <- (lco[, 2]-bbx_orig[2,1]) / (bbx_orig[2,2] - bbx_orig[2,1])
				lco
			})
			grid.co.y.lns <- lapply(lns_proj@lines[[2]]@Lines, function(l) {
				lco <- attr(l, "coords")
				lco[, 1] <- (lco[, 1]-bbx_orig[1,1]) / (bbx_orig[1,2] - bbx_orig[1,1])
				lco[, 2] <- (lco[, 2]-bbx_orig[2,1]) / (bbx_orig[2,2] - bbx_orig[2,1])
				lco
			})
			lns <- NULL
			lns_proj <- NULL
		} else {
			# normalize coordinates
			grid.co.x <- (grid.x-bbx[1,1]) / (bbx[1,2] - bbx[1,1])
			grid.co.y <- (grid.y-bbx[2,1]) / (bbx[2,2] - bbx[2,1])
			
		}
		
		## format grid labels
		grid.labels.x <- format(grid.x, big.mark = ",")
		grid.labels.y <- format(grid.y, big.mark = ",")
		
	})
}

plot_grid_labels_x <- function(gt, scale) {
	# find coordinates for projected grid labels
	if (!is.na(gt$grid.projection)) {
		cogridx <- sapply(gt$grid.co.x.lns, function(i){
			i[which(i[,2] > 0)[1], 1]
		})
	} else {
		cogridx <- gt$grid.co.x	
	}
	
	labelsx <- gt$grid.labels.x
	
	textGrob(labelsx, y=1, x=cogridx, just="top", gp=gpar(col=gt$grid.labels.col, cex=gt$grid.labels.size*scale, fontface=gt$fontface, fontfamily=gt$fontfamily))
	
}

plot_grid_labels_y <- function(gt, scale) {
	# find coordinates for projected grid labels
	if (!is.na(gt$grid.projection)) {
		cogridy <- sapply(gt$grid.co.y.lns, function(i){
			i[which(i[,1] > 0)[1], 2]
		})
	} else {
		cogridy <- gt$grid.co.y
	}
	
	labelsy <- gt$grid.labels.y
	
	textGrob(labelsy, y=cogridy, x=1, just="right", gp=gpar(col=gt$grid.labels.col, cex=gt$grid.labels.size*scale, fontface=gt$fontface, fontfamily=gt$fontfamily))
}


plot_grid <- function(gt, scale, add.labels) {
	## might be confusing: gridx are grid lines for the x-axis, so they are vertical
	cogridx <- gt$grid.co.x
	cogridy <- gt$grid.co.y
	labelsx <- gt$grid.labels.x
	labelsy <- gt$grid.labels.y
	
	cex <- gt$grid.labels.size*scale
	
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
		labelsYw <- max(convertWidth(stringWidth(labelsy), "npc", valueOnly=TRUE))  * cex + fw
		labelsXw <- max(convertHeight(stringHeight(labelsx), "npc", valueOnly=TRUE))  * cex + fh
		spacerY <- convertWidth(unit(.75, "lines"), unitTo="npc", valueOnly=TRUE) * cex
		spacerX <- convertHeight(unit(.75, "lines"), unitTo="npc", valueOnly=TRUE) * cex
	} else {
		labelsXw <- labelsYw <- spacerX <- spacerY <- 0
	}	
	
	# find coordinates for projected grid labels
	if (!is.na(gt$grid.projection)) {
		cogridx <- sapply(gt$grid.co.x.lns, function(i){
			i[which(i[,2] > (labelsXw + spacerX))[1], 1]
		})
		cogridy <- sapply(gt$grid.co.y.lns, function(i){
			i[which(i[,1] > (labelsYw + spacerY))[1], 2]
		})
	}
	
	# select grid labels to print
	selx <- cogridx >= labelsYw + spacerY & cogridx <= 1 - spacerY
	sely <- cogridy >= labelsXw + spacerX & cogridy <= 1 - spacerX
	
	# crop projected grid lines, and extract polylineGrob ingredients
	if (!is.na(gt$grid.projection)) {
		lns <- SpatialLinesDataFrame(SpatialLines(list(
			Lines(lapply(gt$grid.co.x.lns, function(m) {
				Line(m)
			}), ID="x"),
			Lines(lapply(gt$grid.co.y.lns, function(m) {
				Line(m)
			}), ID="y")
		)), data.frame(ID=c("x", "y")), match.ID=FALSE)
		lns_crop <- raster::crop(lns, bb(c(labelsYw + spacerY, labelsXw + spacerX, 1, 1)))
		
		cogridxlns <- do.call("rbind", mapply(function(l, i) {
			co <- as.data.frame(attr(l, "coords"))
			co$ID <- i
			co
		}, lns_crop@lines[[1]]@Lines, 1:length(lns_crop@lines[[1]]@Lines), SIMPLIFY=FALSE))
		
		cogridylns <- do.call("rbind", mapply(function(l, i) {
			co <- as.data.frame(attr(l, "coords"))
			co$ID <- i
			co
		}, lns_crop@lines[[2]]@Lines, 1:length(lns_crop@lines[[2]]@Lines), SIMPLIFY=FALSE))
	}
	
	## process x-axis grid lines and labels
	if (any(selx)) {
		cogridx <- cogridx[selx]
		labelsx <- labelsx[selx]
		
		if (is.na(gt$grid.projection)) {
			grobGridX <- polylineGrob(x=rep(cogridx, each=2), y=rep(c(labelsXw+spacerX,1), length(cogridx)), 
									  id=rep(1:length(cogridx), each=2), gp=gpar(col=gt$grid.col, lwd=gt$grid.lwd))
		} else {
			grobGridX <- polylineGrob(x=cogridxlns$x, y=cogridxlns$y, id=cogridxlns$ID, gp=gpar(col=gt$grid.col, lwd=gt$grid.lwd))
		}
		
		grobGridTextX <- if (add.labels) {
			textGrob(labelsx, y=labelsXw+spacerX*.5, x=cogridx, just="top", gp=gpar(col=gt$grid.labels.col, cex=cex, fontface=gt$fontface, fontfamily=gt$fontfamily))
		} else NULL
	} else {
		grobGridX <- NULL
		grobGridTextX <- NULL
	}
	
	
	## process y-axis grid lines and labels
	if (any(sely)) {
		cogridy <- cogridy[sely]
		labelsy <- labelsy[sely]
		
		if (is.na(gt$grid.projection)) {
			grobGridY <- polylineGrob(y=rep(cogridy, each=2), x=rep(c(labelsYw+spacerY,1), length(cogridy)), 
									  id=rep(1:length(cogridy), each=2), gp=gpar(col=gt$grid.col, lwd=gt$grid.lwd))
		} else {
			grobGridY <- polylineGrob(x=cogridylns$x, y=cogridylns$y, id=cogridylns$ID, gp=gpar(col=gt$grid.col, lwd=gt$grid.lwd))
		}
		
		grobGridTextY <- if (add.labels) {
			textGrob(labelsy, x=labelsYw+spacerY*.5, y=cogridy, just="right", gp=gpar(col=gt$grid.labels.col, cex=gt$grid.labels.size*scale, fontface=gt$fontface, fontfamily=gt$fontfamily))
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

plot_bubbles <- function(co.npc, g, gt, lineInch, i, k) {
	bubbleH <- convertHeight(unit(lineInch, "inch"), "npc", valueOnly=TRUE) * gt$scale
	bubbleW <- convertWidth(unit(lineInch, "inch"), "npc", valueOnly=TRUE) * gt$scale
	
	with(g, {
		co.npc[, 1] <- co.npc[, 1] + bubble.xmod * bubbleW
		co.npc[, 2] <- co.npc[, 2] + bubble.ymod * bubbleH
		npol <- nrow(co.npc)
		if (length(bubble.size)!=npol) {
			if (length(bubble.size)!=1) warning("less bubble size values than objects")
			bubble.size <- rep(bubble.size, length.out=npol)
		}
		
		bubble.size <- bubble.size * lineInch / 2
		
		cols <- rep(bubble.col, length.out=npol)
		if (length(bubble.size)!=1) {
			decreasing <- order(-bubble.size)
			co.npc2 <- co.npc[decreasing,]
			bubble.size2 <- bubble.size[decreasing]
			cols2 <- if (length(cols)==1) cols else cols[decreasing]
		} else {
			co.npc2 <- co.npc
			bubble.size2 <- bubble.size
			cols2 <- cols
		}
		
		
		bordercol <- bubble.border.col
		idName <- paste("tm_bubbles", i, k, sep="_")
		
		
		circleGrob(x=unit(co.npc2[,1], "npc"), y=unit(co.npc2[,2], "npc"),
				   r=unit(bubble.size2, "inch"),
				   gp=gpar(col=bordercol, lwd=bubble.border.lwd, fill=cols2), name=idName)
	})
}

plot_text <- function(co.npc, g, gt, lineInch, just=c("center", "center"), bg.margin=.10) {
	lineHnpc <- convertHeight(unit(lineInch, "inch"), "npc", valueOnly=TRUE) * gt$scale
	lineWnpc <- convertWidth(unit(lineInch, "inch"), "npc", valueOnly=TRUE) * gt$scale
	
	npol <- nrow(co.npc)
	with(g, {
		if (!any(text_sel)) {
			warning("No text to display. Check if all size values are smaller than lowerbound.size, or if all positions fall outside the plotting area.")
			return(NULL)
		}
		
		co.npc[, 1] <- co.npc[, 1] + text.xmod * lineWnpc
		co.npc[, 2] <- co.npc[, 2] + text.ymod * lineHnpc
		
		grobText <- textGrob(text[text_sel], x=unit(co.npc[text_sel,1], "npc"), y=unit(co.npc[text_sel,2], "npc"), just=just, gp=gpar(col=text.fontcolor[text_sel], cex=text.size[text_sel], fontface=text.fontface, fontfamily=text.fontfamily))
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
		tGY <- grobText$y + unit(ifelse(just[2]=="top", -(tGH * .5), 
										ifelse(just[2]=="bottom", tGH * .5, 0)), "npc")
		
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
