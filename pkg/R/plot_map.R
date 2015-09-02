plot_map <- function(i, gp, gt, shps, bbx, proj) {
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
			if (inherits(shp, "SpatialLines")) {
				co <- gCentroid(shp, byid=TRUE)@coords
			} else {
				co <- coordinates(shp) # prefered over gCentroid since coordinates correspond to first (normally largest) polygon of each object
			}
			co.npc <- co
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
			col <- process_color(gpl$col, alpha=gpl$alpha, sepia.intensity=gt$sepia.intensity, saturation=gt$saturation)
			grid.shape(shp, gp=gpar(fill=fill, col=col, lwd=gpl$lwd, lty=gpl$lty), bg.col=gt$bg.color, i, k)
		}
		
		plot_tm_lines <- function() {
			col <- process_color(gpl$line.col, alpha=gpl$line.alpha, sepia.intensity=gt$sepia.intensity, saturation=gt$saturation)
			grid.shplines(shp, gp=gpar(col=col, lwd=gpl$line.lwd, lty=gpl$line.lty,
									   lineend="butt"), i, k)
		}
		
		plot_tm_bubbles <- function() plot_bubbles(co.npc, gpl, gt, lineInch, i, k)
		plot_tm_text <- function() plot_text(co.npc, gpl, gt, lineInch)
		
		
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
			rasterGrob(matrix(rast, ncol=shp@ncols, nrow=shp@nrows, byrow = TRUE), x=x, y=y, width=width, height=height)
		} 
		
		e <- environment()
		fnames <- paste("plot", gpl$plot.order, sep="_")
		grobs <- lapply(fnames, do.call, args=list(), envir=e)
		items <- do.call("gList", args =  grobs)
		gTree(children=items)
	}, gp, shps, 1:nlayers, SIMPLIFY=FALSE)
	
	
	# cut map to projection boundaries (i.e. longlat bb -180 - 180, -90 to 90)
	if (gt$earth.boundary) {
		world_bb_co <- matrix(c(
			rep(-180, 181), seq(-179, 179), rep(180, 181), seq(179, -179),
			seq(-90, 90), rep(90, 359), seq(90, -90), rep(-90, 359)), ncol=2)
		
		world_bb_sp <- SpatialPolygons(list(Polygons(list(Polygon(coords=world_bb_co)), ID="world_bb")), proj4string=CRS(get_proj4_code(gt$earth.datum)))
		world_bb_sp2 <- set_projection(world_bb_sp, projection = proj)
		world_bb_co2 <- world_bb_sp2@polygons[[1]]@Polygons[[1]]@coords
		
		world_bb_co3 <- matrix(c((world_bb_co2[,1] - bbx[1,1])/(bbx[1,2]-bbx[1,1]),
								 (world_bb_co2[,2] - bbx[2,1])/(bbx[2,2]-bbx[2,1])), ncol=2)
		
		worldBB <- pathGrob(x = world_bb_co3[,1], y = world_bb_co3[,2], id=rep(1,nrow(world_bb_co3)), gp=gpar(col=gt$earth.boundary.color, fill=NA, lwd=gt$earth.boundary.lwd))
		
		worldBB_cut <- pathGrob(x = c(0, 0, 1, 1, rev(world_bb_co3[,1])), y = c(0, 1, 1, 0, rev(world_bb_co3[,2])), id=c(rep(1,4), rep(2,nrow(world_bb_co3))), gp=gpar(col=NA, fill=gt$space.color))
		
		if (any(world_bb_co3[,1]>0 & world_bb_co3[,1]< 1 & world_bb_co3[,2] > 0 & world_bb_co3[,2] < 1)) {
			grobWorldBB <- gTree(children=gList(worldBB_cut, worldBB))	
		} else {
			grobWorldBB <- NULL
		}
	} else {
		grobWorldBB <- NULL
	}
	
	
	
	grobsElemGrid <- do.call("gList", args = c(treeElements, list(grobWorldBB, treeGridLabels)))

	
	list(treeElemGrid=gTree(children=grobsElemGrid, name="mapElements"), lineInch=lineInch, metaX=metaX, metaY=metaY)
}


process_grid <- function(gt, bbx, proj, sasp) {
	grid.n.x <- grid.n.y <- NULL
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
		if (gt$draw.frame) {
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
	list(treeGridLines=gTree(children=gList(grobGridX, grobGridY)),
		 treeGridLabels=gTree(children=gList(grobGridTextX, grobGridTextY)),
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

		if (!is.na(text.bg.color)) {
			
			
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
		} else {
			grobTextBG <- NULL
		}
		
		if (text.shadow) {
			grobTextSh <- textGrob(text[text_sel], x=unit(co.npc[text_sel,1]+lineW * .05, "npc"), y=unit(co.npc[text_sel,2]- lineH * .05, "npc"), just=just, gp=gpar(col=text.shadowcol[text_sel], cex=text.size[text_sel], fontface=text.fontface, fontfamily=text.fontfamily))
		} else {
			grobTextSh <- NULL
		}
		
		gList(grobTextBG, grobTextSh, grobText)
	})
}



plot_all <- function(i, gp, shps.env, dasp, sasp, inner.margins.new, legend_pos) {
	gt <- gp$tm_layout

	shps <- get("shps", envir=shps.env)
	bbx <- attr(shps[[1]], "bbox")
	proj <- attr(shps[[1]], "proj4string")@projargs
	
	## in case of small multiples, get i'th shape
	if (any(gt$shp_nr!=0) && (gt$drop.shapes || gt$free.coords)) {
		shps <- shps[[i]]
	}
	
	if (gt$grid.show) gt <- process_grid(gt, bbx, proj, sasp)
	
	
	
	
	gp[c("tm_layout")] <- NULL
	
	if (!gt$legend.only) {
		## calculate width and height of the shape based on the device asp ratio (dasp) and the shape aspect ratio (sasp)
		margins <- gt$outer.margins
		mar.y <- sum(margins[c(1,3)])
		mar.x <- sum(margins[c(2,4)])
		
		height <- 1 - mar.y
		width <- 1 - mar.x
		if (dasp > sasp) {
			width <- width * (sasp/dasp)
		} else {
			height <- height * (dasp/sasp)
		}
		
		## calculate outer margins
		margin.left <- (1 - width) * ifelse(mar.x==0, .5, (margins[2]/mar.x))
		margin.top <- (1 - height) * ifelse(mar.y==0, .5, (margins[3]/mar.y))
		

		## background rectangle (whole device)
		if (!gt$draw.frame) {
			grobBG <- rectGrob(gp=gpar(fill=gt$bg.color, col=NA))
		} else {
			grobBG <- NULL
		}

		if (gt$design.mode) {
			grobBG <- rectGrob(gp=gpar(fill="yellow", col="yellow"))
		}
		
		## create a 3x3 grid layout with the shape to be drawn in the middle cell
		gridLayoutMap <- viewport(layout=grid.layout(3, 3, 
													 heights=unit(c(margin.top, height, 1), 
													 			 c("npc", "npc", "null")), 
													 widths=unit(c(margin.left, width, 1), 
													 			c("npc", "npc", "null"))),
								  name="maingrid")
		pushViewport(gridLayoutMap)

		## the thematic map with background
		treeMap <- cellplot(2, 2, name="aspvp", e={
			## background rectangle (inside frame)
			if (gt$draw.frame) {
				grobBGframe <- rectGrob(gp=gpar(fill=gt$bg.color, col=NA), name="mapBG")
			} else {
				grobBGframe <- NULL
			}
			
			if (gt$design.mode) {
				grobBGframe <- rectGrob(gp=gpar(fill="blue", col="blue"), name="mapBG")
				
				aspWidth <- 1-sum(inner.margins.new[c(2,4)])
				aspHeight <- 1-sum(inner.margins.new[c(1,3)])
				grobAsp <- rectGrob(x = (inner.margins.new[2]+1-inner.margins.new[4])/2, y=(inner.margins.new[1]+1-inner.margins.new[3])/2, width=aspWidth, height=aspHeight, gp=gpar(fill="red", col="red"), name="aspRect")
			} else {
				grobAsp <- NULL
			}
			
			## the thematic map
			res <- plot_map(i, gp, gt, shps, bbx, proj)
			treeElemGrid <- res$treeElemGrid
			lineInch <- res$lineInch
			metaX <- res$metaX
			metaY <- res$metaY
			gList(grobBGframe, grobAsp, treeElemGrid)
		})
		
		## background rectangle (whole device), in case a frame is drawn and outer.bg.color is specified
		treeBG <- if (!is.null(gt$outer.bg.color) && gt$draw.frame) {
			cellplot(1:3,1:3, e=rectGrob(gp=gpar(col=gt$outer.bg.color, fill=gt$outer.bg.color)), name="mapBG")
		} else NULL
		treeFrame <- cellplot(2,2, e={
			if (gt$draw.frame) {
				pH <- convertHeight(unit(1, "points"), unitTo = "npc", valueOnly = TRUE)*gt$frame.lwd
				pW <- convertWidth(unit(1, "points"), unitTo = "npc", valueOnly = TRUE)*gt$frame.lwd
				if (gt$frame.double.line) {
					gList(
						rectGrob(width = 1-4*pW, height=1-4*pH, gp=gpar(col=gt$bg.color, fill=NA, lwd=5*gt$frame.lwd, lineend="square")),
						rectGrob(gp=gpar(col="#000000", fill=NA, lwd=3*gt$frame.lwd, lineend="square")),
						rectGrob(width = 1-8*pW, height=1-8*pH, gp=gpar(col="#000000", fill=NA, lwd=gt$frame.lwd, lineend="square")))
				} else {
					rectGrob(gp=gpar(col="#000000", fill=NA, lwd=gt$frame.lwd, lineend="square"))
				}
				
			} else rectGrob(gp=gpar(col=gt$bg.color, fill=NA))
		}, name="mapFrame")
		
		treeGridLabels <- if (gt$grid.show && !gt$grid.labels.inside.frame) {
			gTree(children=gList(
				cellplot(3,2, e=plot_grid_labels_x(gt, scale=gt$scale), name="gridLabelsX"),
				cellplot(2,1, e=plot_grid_labels_y(gt, scale=gt$scale), name="gridLabelsY")), name="gridLabels")
		} else NULL
		
		
		
		treeMapX <- gTree(children=gList(grobBG, gTree(children=gList(treeBG, treeMap, treeFrame, treeGridLabels), vp=gridLayoutMap, name="outer_map")), name="BG")
		
		upViewport()
	} else {
		## bubble height needed to align with bubbles in legend
		lineInch <- convertHeight(unit(1, "lines"), "inch", valueOnly=TRUE) * gt$legend.text.size
		treeMapX <- NULL
		metaX <- 0
		metaY <- 0
	}
	
	## prepare legend items
	leg <- legend_prepare(gp, gt, lineInch)
	
	## legend, title, and other thinks such as compass
	if (!is.null(leg) || gt$title!="" || gt$credits.show || gt$scale.show || gt$compass.show) {
		if (!gt$legend.only) {
			vpLeg <- vpPath("maingrid", "aspvp")
			d <- downViewport(vpLeg)
			grobLegendBG <- NULL
		} else {
			vpLeg <- current.viewport()
			grobLegendBG <- rectGrob(gp=gpar(fill=gt$bg.color, col=NA))
		}
		treeMeta <- meta_plot(gt, leg, legend_pos, bbx, metaX, metaY)
		treeMetaX <- gTree(children=gList(grobLegendBG, treeMeta))
		
		if (!gt$legend.only) {
			treeMapX <- addGrob(treeMapX, child=treeMetaX, gPath=gPath("outer_map", "aspvp"))
			upViewport(d)
		} else {
			treeMapX <- treeMetaX
		}
		
	}
	
	treeMapX
}