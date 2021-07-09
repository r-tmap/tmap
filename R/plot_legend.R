legend_subplot <- function(x, id, gt, histWidth) {
	legend.type <- x$legend.type
	cols <- if (legend.type=="hist") 1 else c(1,2)
	list(cellplot(id, cols, e={
		lineHeight <- convertHeight(unit(1, "lines"), unitTo="npc", valueOnly=TRUE)
		res <- if (legend.type=="hist") {
			plot_legend_hist(x, gt$legend.hist.size, lineHeight, scale=gt$scale, m=.25, attr.color=gt$attr.color, legend.hist.bg.color = gt$legend.hist.bg.color)
		} else if (legend.type=="TITLE") {
			legend_title(x, gt, is.main.title=TRUE, lineHeight, m=.1)
		} else if (legend.type=="title") {
			legend_title(x, gt, is.main.title=FALSE, lineHeight, m=.1)
		} else if (legend.type=="spacer") {
			list(NULL, 0)
		} else if (x$legend.is.portrait) {
			legend_portr(x, gt, lineHeight, m=.25)
		} else {
			legend_landsc(x, gt, lineHeight, m=.25)
		}
		legGrob <- res[[1]]
		legWidth <- res[[2]]
		if (legend.type=="hist") legWidth <- histWidth
		if (gt$design.mode) {
			gTree(children=gList(rectGrob(gp=gpar(fill="#CCCCCCCC")), legGrob))	
		} else legGrob
	}), legWidth=legWidth)
}


legend_subplot2 <- function(x, id, rel_height, gt, histWidth, titleRow) {
	if (is.null(x)) return(list(NULL, 0))
	legend.type <- x$legend.type
	
	if (id<0) {
		row <- 1
		col <- 1:(-id)
	} else {
		row <- (if ((id %% 2)==1) 1 else 2) + titleRow
		col <- ((id-1) %/% 2) + 1
	}
	
	list(cellplot(row, col, e={
		pushViewport(viewport(height=rel_height, y=1-.5*rel_height))
		lineHeight <- convertHeight(unit(1, "lines"), unitTo="npc", valueOnly=TRUE)
		res <- if (legend.type=="hist") {
			plot_legend_hist(x, gt$legend.hist.size, lineHeight, scale=gt$scale, m=.25, attr.color=gt$attr.color, legend.hist.bg.color = gt$legend.hist.bg.color)
		} else if (legend.type=="TITLE") {
			legend_title(x, gt, is.main.title=TRUE, lineHeight, m=.1)
		} else if (legend.type=="title") {
			legend_title(x, gt, is.main.title=FALSE, lineHeight, m=.1)
		} else if (legend.type=="spacer") {
			list(NULL, 0)
		} else if (x$legend.is.portrait) {
			legend_portr(x, gt, lineHeight, m=.25)
		} else {
			legend_landsc(x, gt, lineHeight, m=.25)
		}
		v <- current.viewport()
		upViewport(1)
		legGrob <- gTree(children=gList(res[[1]]), vp=v)
		legWidth <- res[[2]]
		if (legend.type=="hist") legWidth <- histWidth
		if (gt$design.mode) {
			gTree(children=gList(rectGrob(gp=gpar(fill="#CCCCCCCC")), legGrob))	
		} else legGrob
	}), legWidth=legWidth)
}

legend_title <- function(x, gt, is.main.title, lineHeight, m) {
	size <- ifelse(is.main.title, gt$title.size, gt$legend.title.size)
	
	fontface <- ifelse(is.main.title, gt$title.fontface, gt$legend.title.fontface)
	fontfamily <- ifelse(is.main.title, gt$title.fontfamily, gt$legend.title.fontfamily)
	color <- ifelse(is.main.title, gt$title.color, gt$legend.title.color)
	
	title <- x$title
	nlines <- number_text_lines(x$title)
	my <- lineHeight * size * m
	mx <- convertWidth(convertHeight(unit(my, "npc"), "inch"), "npc", TRUE)
	
	w <- text_width_npc(title)
	newsize <- min(size, 5/(lineHeight*nlines*6), (1-2*mx)/w)
	
	list(textGrob(title, x=mx, y=6/12 , just=c("left", "center"), gp=gpar(col=color, cex=newsize, fontface=fontface, fontfamily=fontfamily)), legWidth=2*mx+w*newsize)
}


legend_portr <- function(x, gt, lineHeight, m) {
	
	legend.text.size <- gt$legend.text.size
	with(x, {
		is.cont <- (nchar(legend.palette[1])>20)
		
		my <- lineHeight * legend.text.size * m
		mx <- convertWidth(convertHeight(unit(my, "npc"), "inch"), "npc", TRUE)
		s <- 1.25 ## for text only
		s2 <- 4/3 ## for symbols only
		r <- 1-2*my
		
		brks <- attr(legend.labels, "brks")
		align <- attr(legend.labels, "align")
		
		if (legend.type=="symbol.size") {
			nitems <- length(legend.labels)
			hs <- convertHeight(unit(legend.sizes, "inch"), "npc", valueOnly=TRUE) / s2
			lhs <- pmax(hs*s, legend.text.size * lineHeight)
			if (sum(lhs)>r+1e-6) {
				clipID <- which(cumsum(lhs) > r)[1]
				hs <- hs[1:(clipID-1)]
				lhs <- lhs[1:(clipID-1)]
				legend.labels <- legend.labels[1:(clipID-1)]
				nitems <- length(legend.labels)
			}
		} else if (legend.type=="text.size") {
			nitems <- length(legend.labels)
			hs <- convertHeight(unit(legend.sizes, "lines"), "npc", valueOnly=TRUE)
			lhs <- pmax(hs*s, legend.text.size * lineHeight)
			if (sum(lhs)>r+1e-6) {
				clipID <- which(cumsum(lhs) > r)[1]
				hs <- hs[1:(clipID-1)]
				lhs <- lhs[1:(clipID-1)]
				legend.labels <- legend.labels[1:(clipID-1)]
				legend.text <- legend.text[1:(clipID-1)]
				nitems <- length(legend.labels)
			}
		} else {
			nitems <- length(legend.labels)
			lhs <- hs <- rep(r / nitems, nitems)
		}
		
		if (legend.type %in% c("symbol.col", "symbol.shape") && !is.cont) {
			bmax <- convertHeight(unit(legend.sizes, "inch"), "npc", valueOnly=TRUE) / s2
			hs <- pmin(hs/s*symbol.normal.size, bmax)
		}
		
		
		if (legend.type=="text.col" && !is.cont) {
			cex <- pmin(convertHeight(unit(hs/s, "npc"), "lines", valueOnly = TRUE), legend.sizes)
			ws <- text_width_npc(legend.text, space = FALSE) * cex
		} else if  (legend.type=="text.size") {
			cex <- legend.sizes #pmin(convertHeight(unit(hs/s, "npc"), "lines", valueOnly = TRUE))
			ws <- text_width_npc(legend.text, space=FALSE) * cex
		} else {
			ws <- convertWidth(convertHeight(unit(hs, "npc"), "inch"), "npc", TRUE)
		}
		wsmax <- max(ws)
		
		ys <- 1 - my - cumsum(lhs) + lhs/2
		size <- pmin(lhs / lineHeight, legend.text.size)
		
		hsi <- convertHeight(unit(hs, "npc"), "inch", valueOnly=TRUE) * s2
		
		wstext <- text_width_npc(legend.labels, space = TRUE)
		newsize <- pmin(size, (1-wsmax-4*mx) / wstext)
		
		if (gt$show.messages) {
			numstring <- format(floor(newsize * 100) / 100)
			if (all((newsize / size) < .95)) {
				message("Legend labels were too wide. The labels have been resized to ", paste(numstring, collapse = ", "), ". Increase legend.width (argument of tm_layout) to make the legend wider and therefore the labels larger.")
			} else if (any((newsize / size) < .95)) {
				message("Some legend labels were too wide. These labels have been resized to ", paste(numstring[(newsize / size) < .95], collapse = ", "), ". Increase legend.width (argument of tm_layout) to make the legend wider and therefore the labels larger.")
			}
		}
		
		
		
		
		grobLegendItem <- if (is.cont) {
			fill <- legend.palette
			xs <- mx+ws/2
			
			# process fill colors
			fill_list <- strsplit(fill, split = "-", fixed=TRUE)
			fill_list <- lapply(fill_list, function(i) {
				i[i=="NA"] <- NA
				i
			})
			fill_len <- vapply(fill_list, length, integer(1))
			fill2 <- unlist(fill_list, use.names = FALSE)
			
			# process x,y,w,h
			xs2 <- unlist(mapply(rep, xs, fill_len, SIMPLIFY = FALSE), use.names = FALSE)
			ws2 <- unlist(mapply(rep, ws, fill_len, SIMPLIFY = FALSE), use.names = FALSE)
			
			ys2 <- unlist(mapply(function(y, h, k) {
				seq(y+h/2, y-h/2, length.out=k*2+1)[seq(2, k*2, by=2)]
			}, ys, hs, fill_len, SIMPLIFY = FALSE), use.names = FALSE)
			hs2 <- unlist(mapply(function(h, k) rep(h/k, k), hs, fill_len, SIMPLIFY = FALSE), use.names = FALSE)
			
			rectGrob(x=xs2, 
					 y=ys2, 
					 width= ws2, 
					 height= hs2,
					 gp=gpar(fill=fill2, col=NA))
		} else if (legend.type %in% c("fill", "raster")) {
			fill <- legend.palette
			col <- ifelse(legend.type =="fill", border.col, NA)
			if (legend.type=="raster") lwd <- NA
			rectGrob(x=mx+ws/2, 
					 y=ys, 
					 width= ws, 
					 height= hs,
					 gp=gpar(fill=fill, col=col, lwd=lwd))
		} else if (legend.type %in% c("symbol.size", "symbol.col", "symbol.shape")) {
			cols <- legend.palette
			shapes <- legend.shapes
			shapes <- rep(shapes, length.out=nitems)
			if (any(!is.na(shapes) & shapes>999)) {
				shapeLib <- get("shapeLib", envir = .TMAP_CACHE)
				
				gpars <- get_symbol_gpar(x=shapes,
										 fill=cols,
										 col=symbol.border.col,
										 lwd=symbol.border.lwd,
										 separate=TRUE)
				grobs <- lapply(1:nitems, function(i) {
					if (!is.na(shapes[i]) & shapes[i]>999) {
						grbs <- if (is.na(symbol.border.col)) {
							gList(shapeLib[[shapes[i]-999]])
						} else {
							gList(shapeLib[[shapes[i]-999]], rectGrob(gp=gpar(fill=NA, col=symbol.border.col, lwd=symbol.border.lwd)))	
						}
						gTree(children=grbs, vp=viewport(x=unit(mx+wsmax/2, "npc"), 
														 y=ys[i]+symbol_legend_y_correction(shapes[i]),
														 width=unit(hsi[i]*2/3, "inch"),
														 height=unit(hsi[i]*2/3, "inch")))
					} else {
						pointsGrob(x=unit(mx+wsmax/2, "npc"), 
								   y=ys[i]+symbol_legend_y_correction(shapes[i]),
								   size=unit(hsi[i], "inch"),
								   pch=shapes[i],
								   gp=gpars[[i]])
					}
				})
				gTree(children=do.call(gList, grobs))				
			} else {
				pointsGrob(x=rep(mx+wsmax/2, nitems),
						   y=ys+symbol_legend_y_correction(shapes),
						   size=unit(hsi, "inch"),
						   pch=shapes,
						   gp=get_symbol_gpar(x=shapes,
						   				   fill=cols,
						   				   col=symbol.border.col,
						   				   lwd=symbol.border.lwd))
			}
		} else if (legend.type %in% c("text.size", "text.col")) {
			cols <- legend.palette
			textGrob(legend.text,
					 x=mx, 
					 y=ys,
					 just=c("left", "center"),
					 gp=gpar(cex=cex, col=cols,
					 		fontface = fontface,
					 		fontfamily = fontfamily))
		} else if (legend.type %in% c("line.col", "line.lwd")) {
			lwds <- if (legend.type == "line.col") line.legend.lwd else legend.lwds
			cols <- legend.palette
			polylineGrob(x=mx+ c(0,1)*rep(ws, each=2),
						 y=rep(ys, each=2), 
						 id=rep(1:nitems, each=2),
						 gp=gpar(col=cols, 
						 		lwd=lwds,
						 		lty=line.legend.lty,
						 		lineend="butt"))
		}
		
		
		if (is.null(brks)) {
			if (is.na(align) || align=="left") {
				x2 <- mx*2+wsmax
				just <- c("left", "center")
			} else if (align=="right") {
				x2 <- mx*2+wsmax + (max(wstext) * newsize)
				just <- c("right", "center")
			} else {
				x2 <- mx*2+wsmax + (max(wstext) * newsize * .5)
				just <- c("center", "center")
			}
			
			grobLegendText <- textGrob(legend.labels, x=x2,
									   y=ys, just=just, gp=gpar(col=gt$legend.text.color, cex=newsize, fontface=gt$legend.text.fontface, fontfamily=gt$legend.text.fontfamily))
		} else {
			splits <- split_legend_labels(legend.labels, brks)
			
			xsplits <- apply(attr(wstext, "cw") * newsize, 2, max)
			xsplitscs <- cumsum(xsplits)
			
			legend.labels2 <- unlist(splits, use.names = FALSE)
			
			if (is.na(align) || align=="left") {
				x2 <- mx*2+wsmax + rep(c(0, xsplitscs[-3]), times=length(legend.labels))
				just <- c("left", "center")
			} else if (align=="right") {
				x2 <- mx*2+wsmax + rep(c(0, xsplitscs[-3]), times=length(legend.labels)) + xsplits
				just <- c("right", "center")
			} else {
				x2 <- mx*2+wsmax + rep(c(0, xsplitscs[-3]), times=length(legend.labels)) + xsplits / 2
				just <- c("center", "center")
			}
			
			y2 <- rep(ys, each=3)
			grobLegendText <- textGrob(legend.labels2, x=x2,
									   y=y2, just=just, gp=gpar(col=gt$legend.text.color, cex=newsize, fontface=gt$legend.text.fontface, fontfamily=gt$legend.text.fontfamily))
		}
		
		legWidth <- mx*4+wsmax+max(wstext*newsize)
		
		list(gList(grobLegendItem, grobLegendText), legWidth=legWidth)
	})
}

## design from top to bottom: .25 margin, 1.5 items, 1.25 text, .25 margin
legend_landsc <- function(x, gt, lineHeight, m) {
	legend.text.size <- gt$legend.text.size
	with(x, {
		is.cont <- (nchar(legend.palette[1])>20)
		#grid.rect()
		s <- 1.25 ## for text only
		s2 <- 4/3
		
		if (lineHeight*legend.text.size * 3.25 > 1) {
			legend.text.size <- 1/(lineHeight * 3.25)
		}
		
		my <- lineHeight * legend.text.size * m
		mx <- convertWidth(convertHeight(unit(my, "npc"), "inch"), "npc", TRUE)
		rx <- 1-2*mx
		ry <- 1-2*my
		
		
		nitems <- length(legend.labels)
		# delete too high 
		if (legend.type=="symbol.size") {
			hs <- convertHeight(unit(legend.sizes, "inch"), "npc", valueOnly=TRUE) / s2
		} else if (legend.type=="text.size") {
			hs <- convertHeight(unit(legend.sizes, "lines"), "npc", valueOnly=TRUE)
		} else {
			hs <- rep(1.5*lineHeight*legend.text.size, nitems)
		}
		
		if (legend.type=="text.col" && !is.cont) {
			cex <- pmin(convertHeight(unit(hs/s, "npc"), "lines", valueOnly = TRUE), text.max.size)
			ws <- text_width_npc(legend.text, space=FALSE) * cex
		} else if  (legend.type=="text.size") {
			cex <- legend.sizes #pmin(convertHeight(unit(hs/s, "npc"), "lines", valueOnly = TRUE))
			ws <- text_width_npc(legend.text, space=FALSE) * cex
		} else {
			ws <- convertWidth(convertHeight(unit(hs, "npc"), "inch"), "npc", TRUE)
		}
		
		labelsws <- text_width_npc(legend.labels) * legend.text.size
		
		
		if (legend.type=="text.col" && !is.cont) {
			cex <- pmin(convertHeight(unit(hs/s, "npc"), "lines", valueOnly = TRUE), legend.sizes)
			textws <- text_width_npc(legend.text, space=FALSE) * cex
			labelsws <- pmax(labelsws, textws)
		} else if  (legend.type=="text.size") {
			cex <- legend.sizes
			textws <- text_width_npc(legend.text, space=FALSE) * cex
			labelsws <- pmax(labelsws, textws)
		}
		
		maxlabelsws <- max(labelsws)
		
		ws <- rep(maxlabelsws, nitems)
		if (sum(ws)>rx && legend.type!="text.size") {
			ratio <- (sum(ws)/rx)
			ws <- ws / ratio
			legend.text.size <- legend.text.size / ratio
			
			numstring <- format(floor(legend.text.size * 100) / 100)
			
			if (gt$show.messages) message("Legend labels were too wide. Therefore, legend.text.size has been set to ", numstring, ". Increase legend.width (argument of tm_layout) to make the legend wider and therefore the labels larger.")
		}
		
		wsmax <- rx/nitems
		align <- attr(legend.labels, "align")
		
		
		if (legend.type=="symbol.size") {
			symbolws <- convertWidth(unit(legend.sizes, "inch"), "npc", valueOnly=TRUE) / s2
			ws <- pmax(ws, symbolws*1.1)
			
			# delete too wide 
			if (sum(ws)>rx) {
				clipID2 <- which(cumsum(ws)>rx)[1]
				nitems <- clipID2 - 1
				legend.labels <- legend.labels[1:nitems]
				if (length(legend.palette)>1) legend.palette <- legend.palette[1:nitems]
				legend.sizes <- legend.sizes[1:nitems]
				hs <- hs[1:nitems]
				ws <- ws[1:nitems]
				if (gt$show.messages) message("The legend is too narrow to place all symbol sizes.")
			}
		} else if (legend.type=="text.size") {
			#textws <- convertWidth(unit(legend.sizes, "lines"), "npc", valueOnly=TRUE)
			#ws <- pmax(ws, textws*1.1)
			
			# delete too wide 
			if (sum(ws)>rx) {
				clipID2 <- which(cumsum(ws)>rx)[1]
				nitems <- clipID2 - 1
				legend.labels <- legend.labels[1:nitems]
				if (length(legend.palette)>1) legend.palette <- legend.palette[1:nitems]
				legend.sizes <- legend.sizes[1:nitems]
				hs <- hs[1:nitems]
				ws <- ws[1:nitems]
			}
		}
		
		xs <- mx + cumsum(ws) - ws/2
		
		if (legend.type %in% c("symbol.col", "symbol.shape")) {
			bmax <- convertHeight(unit(legend.sizes, "inch"), "npc", valueOnly=TRUE) / s2
			hs <- pmin(hs/s*symbol.normal.size, bmax)

		} else if (legend.type=="text.col") {
			bmax <- convertHeight(unit(legend.sizes, "lines"), "npc", valueOnly=TRUE)
			hs <- pmin(hs, bmax)
		}
		
		hsmax <- max(hs)
		hsi <- convertHeight(unit(hs, "npc"), "inch", valueOnly=TRUE) * s2

		grobLegendItem <- if (is.cont) {
			fill <- legend.palette
			xtraWidth <- ws[1]/2
			ys <- 1-my-hs/2
			
			# process fill colors
			fill_list <- strsplit(fill, split = "-", fixed=TRUE)
			fill_list <- lapply(fill_list, function(i) {
				i[i=="NA"] <- NA
				i
			})
			fill_len <- vapply(fill_list, length, integer(1))
			fill2 <- unlist(fill_list, use.names = FALSE)
			
			# process x,y,w,h
			ys2 <- unlist(mapply(rep, ys, fill_len, SIMPLIFY = FALSE), use.names = FALSE)
			hs2 <- unlist(mapply(rep, hs, fill_len, SIMPLIFY = FALSE), use.names = FALSE)
			
			xs2 <- unlist(mapply(function(x, w, k) {
				seq(x-w/2, x+w/2, length.out=k*2+1)[seq(2, k*2, by=2)]
			}, xs, ws, fill_len, SIMPLIFY = FALSE), use.names = FALSE)
			ws2 <- unlist(mapply(function(w, k) rep(w/k, k), ws, fill_len, SIMPLIFY = FALSE), use.names = FALSE)
			
			rectGrob(x=xs2, 
					 y=ys2, 
					 width= ws2, 
					 height= hs2,
					 gp=gpar(fill=fill2, col=NA))
		} else if (legend.type %in% c("fill", "raster")) {
			fill <- legend.palette
			xtraWidth <- ws[1]/2
			col <- ifelse(legend.type =="fill", border.col, NA)
			if (legend.type=="raster") lwd <- NA
			rectGrob(x=xs, 
					 #y=1-my-hs/2, 
					 y=1 * lineHeight*legend.text.size + 2*my + hs/2,
					 width= ws, 
					 height= hs,
					 gp=gpar(fill=fill, col=col, lwd=lwd))
		} else if (legend.type %in% c("symbol.size", "symbol.col", "symbol.shape")) {
			cols <- legend.palette
			
			shapes <- legend.shapes
			shapes <- rep(shapes, length.out=nitems)
			
			symbolR <- unit(hsi, "inch")
			xtraWidth <- convertWidth(max(symbolR), "npc", valueOnly=TRUE)/2/s2
			
			if (any(!is.na(shapes) & shapes>999)) {
				shapeLib <- get("shapeLib", envir = .TMAP_CACHE)
				
				gpars <- get_symbol_gpar(x=shapes,
										 fill=cols,
										 col=symbol.border.col,
										 lwd=symbol.border.lwd,
										 separate=TRUE)
				grobs <- lapply(1:nitems, function(i) {
					if (!is.na(shapes[i]) && shapes[i]>999) {
						grbs <- if (is.na(symbol.border.col)) {
							gList(shapeLib[[shapes[i]-999]])
						} else {
							gList(shapeLib[[shapes[i]-999]], rectGrob(gp=gpar(fill=NA, col=symbol.border.col, lwd=symbol.border.lwd)))	
						}
						
						gTree(children=grbs, vp=viewport(x=xs[i], 
																					 y=1-my-hsmax/2+symbol_legend_y_correction(shapes[i]),
																					 width=unit(symbolR[i], "inch")*(2/3),
																					 height=unit(symbolR[i], "inch")*(2/3)))
					} else {
						pointsGrob(x=xs[i], 
								   y=1 * lineHeight*legend.text.size + 1.5*my + hsmax/2+symbol_legend_y_correction(shapes[i]),
								   size=unit(symbolR[i], "inch"),
								   pch=shapes[i],
								   gp=gpars[[i]])
					}
				})
				gTree(children=do.call(gList, grobs))				
			} else {
				#rectGrob(gp=gpar(fill="pink"))
				pointsGrob(x=xs,
						   y=1 * lineHeight*legend.text.size + 1.5*my + hsmax/2+symbol_legend_y_correction(shapes),
						   size=symbolR,
						   pch=shapes,
						   gp=get_symbol_gpar(x=shapes,
						   					fill=cols,
						   					col=symbol.border.col,
						   					lwd=symbol.border.lwd))
			}
		} else if (legend.type %in% c("text.size", "text.col")) {
			
			cols <- legend.palette
			xtraWidth <- convertWidth(unit(hsi, "inch"), "npc", valueOnly=TRUE)
			textGrob(legend.text,
					 x=xs, y=1-my-hsmax/2,
					 just=c("center", "center"),
					 gp=gpar(cex=cex, col=cols,
					 		fontface = fontface,
					 		fontfamily = fontfamily))
		} else if (legend.type %in% c("line.col", "line.lwd")) {
			lwds <- if (legend.type == "line.col") line.legend.lwd else legend.lwds
			cols <- legend.palette
			xtraWidth <- convertWidth(unit(lwds[min(length(lwds), nitems)], "points"), "npc", valueOnly=TRUE)/2
			polylineGrob(x=rep(xs, each=2), 
						 y=1-my-c(0,1)*rep(hs, each=2),
						 id=rep(1:nitems, each=2),
						 gp=gpar(col=cols, 
						 		lwd=lwds,
						 		lty=line.legend.lty,
						 		lineend="butt"))
		}
		
		if (is.na(align) || align=="center") {
			x2 <- xs
			just <- c("center", "top")
		} else if (align=="right") {
			x2 <- xs + ws/2
			just <- c("right", "top")
		} else {
			x2 <- xs - ws/2
			just <- c("left", "top")
		}
		if (gt$design.mode) {
			grobLegendTextBg <- rectGrob(x = .5, y = .5*(my+lineHeight*legend.text.size), height = my+lineHeight*legend.text.size, width = 1, gp = gpar(fill = "orange"))
		} else {
			grobLegendTextBg <- NULL
		}
		
		grobLegendText <- textGrob(legend.labels, x=x2,
								   y=.5*my+lineHeight*legend.text.size, just=just, gp=gpar(col=gt$legend.text.color, cex=legend.text.size, fontface=gt$legend.text.fontface, fontfamily=gt$legend.text.fontfamily))
		
		legWidth <- mx*2+xs[length(xs)]+max(xtraWidth, labelsws[nitems]*legend.text.size/2)
		
		list(gList(grobLegendItem, grobLegendTextBg, grobLegendText), legWidth=legWidth)
	})
}


