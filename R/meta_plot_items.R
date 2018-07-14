legend_subplot <- function(x, id, gt, histWidth) {
	legend.type <- x$legend.type
	cols <- if (legend.type=="hist") 1 else c(1,2)
	list(cellplot(id, cols, e={
		lineHeight <- convertHeight(unit(1, "lines"), unitTo="npc", valueOnly=TRUE)
		res <- if (legend.type=="hist") {
			legend_hist(x, gt$legend.hist.size, lineHeight, scale=gt$scale, m=.25, attr.color=gt$attr.color, legend.hist.bg.color = gt$legend.hist.bg.color)
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
			legend_hist(x, gt$legend.hist.size, lineHeight, scale=gt$scale, m=.25, attr.color=gt$attr.color, legend.hist.bg.color = gt$legend.hist.bg.color)
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
	title <- x$title
	nlines <- number_text_lines(x$title)
	my <- lineHeight * size * m
	mx <- convertWidth(convertHeight(unit(my, "npc"), "inch"), "npc", TRUE)
	
	w <- text_width_npc(title)
	newsize <- min(size, 5/(lineHeight*nlines*6), (1-2*mx)/w)
	
	
	list(textGrob(title, x=mx, y=6/12 , just=c("left", "center"), gp=gpar(col=gt$legend.text.color, cex=newsize, fontface=gt$fontface, fontfamily=gt$fontfamily)), legWidth=2*mx+w*newsize)
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
			fill2 <- unlist(fill_list)
			
			# process x,y,w,h
			xs2 <- unlist(mapply(rep, xs, fill_len, SIMPLIFY = FALSE))
			ws2 <- unlist(mapply(rep, ws, fill_len, SIMPLIFY = FALSE))
			
			ys2 <- unlist(mapply(function(y, h, k) {
				seq(y+h/2, y-h/2, length.out=k*2+1)[seq(2, k*2, by=2)]
			}, ys, hs, fill_len, SIMPLIFY = FALSE))
			hs2 <- unlist(mapply(function(h, k) rep(h/k, k), hs, fill_len, SIMPLIFY = FALSE))
			
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
				shapeLib <- get(".shapeLib", envir = .TMAP_CACHE)
				
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
					 gp=gpar(cex=cex, col=cols))
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
									   y=ys, just=just, gp=gpar(col=gt$legend.text.color, cex=newsize, fontface=gt$fontface, fontfamily=gt$fontfamily))
		} else {
			splits <- split_legend_labels(legend.labels, brks)
			
			xsplits <- apply(attr(wstext, "cw") * newsize, 2, max)
			xsplitscs <- cumsum(xsplits)
			
			legend.labels2 <- unlist(splits)
			
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
									   y=y2, just=just, gp=gpar(col=gt$legend.text.color, cex=newsize, fontface=gt$fontface, fontfamily=gt$fontfamily))
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
			# 			nofit <- which(hs>(ry-1.25*lineHeight*legend.text.size))
			# 			
			# 			if (length(nofit)) {
			# 				clipID <- nofit[1]
			# 				nitems <- clipID - 1
			# 				legend.labels <- legend.labels[1:nitems]
			# 				if (length(legend.palette)>1) legend.palette <- legend.palette[1:nitems]
			# 				legend.sizes <- legend.sizes[1:nitems]
			# 				hs <- hs[1:nitems]
			# 			}
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
			fill2 <- unlist(fill_list)
			
			# process x,y,w,h
			ys2 <- unlist(mapply(rep, ys, fill_len, SIMPLIFY = FALSE))
			hs2 <- unlist(mapply(rep, hs, fill_len, SIMPLIFY = FALSE))
			
			xs2 <- unlist(mapply(function(x, w, k) {
				seq(x-w/2, x+w/2, length.out=k*2+1)[seq(2, k*2, by=2)]
			}, xs, ws, fill_len, SIMPLIFY = FALSE))
			ws2 <- unlist(mapply(function(w, k) rep(w/k, k), ws, fill_len, SIMPLIFY = FALSE))
			
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
				shapeLib <- get(".shapeLib", envir = .TMAP_CACHE)
				
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
			# circleGrob(x=xs, y=1-my-hsmax/2, r=symbolR,
			# 		   gp=gpar(fill=cols,
			# 		   		col=symbol.border.col,
			# 		   		lwd=symbol.border.lwd))
		} else if (legend.type %in% c("text.size", "text.col")) {
			
			cols <- legend.palette
			xtraWidth <- convertWidth(unit(hsi, "inch"), "npc", valueOnly=TRUE)
			textGrob(legend.text,
					 x=xs, y=1-my-hsmax/2,
					 just=c("center", "center"),
					 gp=gpar(cex=cex, col=cols))
		} else if (legend.type %in% c("line.col", "line.lwd")) {
			lwds <- if (legend.type == "line.col") line.legend.lwd else legend.lwds
			cols <- legend.palette
			xtraWidth <- convertWidth(unit(lwds[nitems], "points"), "npc", valueOnly=TRUE)/2
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
								   y=.5*my+lineHeight*legend.text.size, just=just, gp=gpar(col=gt$legend.text.color, cex=legend.text.size, fontface=gt$fontface, fontfamily=gt$fontfamily))
		
		legWidth <- mx*2+xs[length(xs)]+max(xtraWidth, labelsws[nitems]*legend.text.size/2)
		
		list(gList(grobLegendItem, grobLegendTextBg, grobLegendText), legWidth=legWidth)
	})
}


plot_scale <- function(gt, just, xrange, crop_factor) {
	light <- do.call("process_color", c(list(gt$scale.color.light, alpha=1), gt$pc))
	dark <- do.call("process_color", c(list(gt$scale.color.dark, alpha=1), gt$pc))

	unit <- gt$shape.units$unit
	unit.size <- 1/gt$shape.units$to
	
	if (is.na(unit.size)) {
		warning("Unable to determine shape coordinate units. Please check if the \"+units\" part of the projection is present. Otherwise, specify coords.unit or unit.size")
	} else if (!gt$shape.units$projected && ((gt$shape.bbx[4]-gt$shape.bbx[2]) > 30)) {
		if (gt$show.messages) message("Scale bar set for latitude ", gsub("long@lat(.+)$", "\\1", unit), " and will be different at the top and bottom of the map.")
	}
	
	xrange2 <- xrange/unit.size
	
	if (is.null(gt$scale.breaks)) {
		ticks2 <- pretty(c(0, xrange2*crop_factor), 4)
	} else {
		ticks2 <- gt$scale.breaks
	}
	ticks2Labels <- format(ticks2, trim=TRUE)
	ticksWidths <- text_width_npc(ticks2Labels)
	
	labels <- c(ticks2Labels, unit)
	
	n <- length(ticks2)
	ticks3 <- ticks2*unit.size / xrange
	
	widths <- ticks3[2] - ticks3[1]
	size <- min(gt$scale.size, widths/max(ticksWidths))
	x <- ticks3[1:(n-1)] + .5*ticksWidths[1]*size
	
	lineHeight <- convertHeight(unit(1, "lines"), "npc", valueOnly=TRUE) * size
	#my <- lineHeight / 2
	#mx <- convertWidth(convertHeight(unit(my, "npc"), "inch"), "npc", TRUE)
	
	unitWidth <- text_width_npc(unit) * size
	width <- widths * (n-1) + .5*ticksWidths[1]*size + .5*ticksWidths[n]*size+ unitWidth   #widths * n 
	
	xtext <- x[1] + c(ticks3, ticks3[n] + .5*ticksWidths[n]*size + .5*unitWidth)# + widths*.5 + unitWidth*.5) #+ position[1]
	
	x <- just-just*width+x
	xtext <- just-just*width+xtext
	
	# if (just=="right") {
	# 	x <- 1-width+x
	# 	xtext <- 1-width+xtext
	# } else if (just=="center") {
	# 	x <- .5-.5*width+x
	# 	xtext <- .5-.5*width+xtext
	# }
	
	grobBG <- if (gt$design.mode) rectGrob(gp=gpar(fill="orange")) else NULL
	
	gTree(children=gList(
		grobBG,
		rectGrob(x=x, y=1.5*lineHeight, width = widths, height=lineHeight*.5, just=c("left", "bottom"), gp=gpar(col=dark, fill=c(light, dark), lwd=gt$scale.lwd)),
		textGrob(label=labels, x = xtext, y = lineHeight, just=c("center", "center"), gp=gpar(col=gt$attr.color, cex=size, fontface=gt$fontface, fontfamily=gt$fontfamily))), name="scale_bar")
	
	
}

plot_logo <- function(gt, just, id) {
	lineHeight <- convertHeight(unit(1, "lines"), "npc", valueOnly=TRUE)
	
	
	grobBG <- if (gt$design.mode) rectGrob(gp=gpar(fill="orange")) else NULL
	
	files <- gt$logo.file[[id]]
	heights <- gt$logo.height[[id]]
	widths <- gt$logo.width[[id]]
	margin <- gt$logo.margin[id]
	halign <- gt$logo.halign[id]
	
	n <- length(files)
	xs <- c(0, cumsum(widths))[-(n+1)] + (1:n)*margin +  (widths / 2)

	# translate to number from 0 (bottom) to 1 (top)
	halign_num <- ifelse(is_num_string(halign), as.numeric(halign), ifelse(halign=="top", 1, ifelse(halign=="bottom", 0, 0.5)))
	
	# rescale to -1 to 1
	halign_num <- (halign_num-.5) * 2
	
	h <- convertHeight(unit(1, "npc"), "lines", valueOnly=TRUE)
	
	ys <- h/2 + (max(heights) - heights)/2 * halign_num
	
		
	heights_npc <- convertHeight(unit(heights, "lines"), "npc", valueOnly=TRUE)
	widths_npc <- convertWidth(unit(widths, "lines"), "npc", valueOnly=TRUE)
	xs_npc <- convertX(unit(xs, "lines"), "npc", valueOnly=TRUE)
	ys_npc <- convertY(unit(ys, "lines"), "npc", valueOnly=TRUE)
	
	heights_in <- convertHeight(unit(heights, "lines"), "inch", valueOnly=TRUE)
	widths_in <- convertHeight(unit(widths, "lines"), "inch", valueOnly=TRUE)
	
	
	grobsLogo <- do.call(gList, c(list(grobBG=grobBG), mapply(function(f, h, w, x, y, hin, win) {
		grobLogo <- pngGrob(f, fix.borders = T, n=2, height.inch=hin, target.dpi=96)
		
		rdim <- dim(grobLogo$raster)
		grobLogo$raster <- matrix(do.call("process_color", c(list(as.vector(grobLogo$raster), alpha=1), gt$pc)), nrow = rdim[1], ncol=rdim[2])
		# correct width to logo asp
		win2 <- hin * 1/do.call("/", as.list(rdim))
		w2 <- convertWidth(unit(win2, "inch"), "npc", valueOnly=TRUE)
		grobLogo$x <- unit(x, "npc")
		grobLogo$y <- unit(y, "npc")
		grobLogo$width <- unit(w2, "npc")
		grobLogo$height <- unit(h, "npc")
		grobLogo
	}, files, heights_npc, widths_npc, xs_npc, ys_npc, heights_in, widths_in, SIMPLIFY = FALSE)))
	
	
	
	gTree(children=grobsLogo)
	
}

plot_cred <- function(gt, just, id) {
	lineHeight <- convertHeight(unit(1, "lines"), "npc", valueOnly=TRUE)
	
	my <- lineHeight / 2
	mx <- convertWidth(convertHeight(unit(my, "npc"), "inch"), "npc", TRUE)
	
	# number of lines
	txt <- gt$credits.text[id]
	nlines <- number_text_lines(txt)
	
	size <- min((1-2*mx) / text_width_npc(txt, space=FALSE), gt$credits.size[id])
	
	width <- (text_width_npc(txt, space=FALSE)+1*mx) * size
	#height <- lineHeight * (nlines) * size
	
	x <- just - just*width
	tx <- mx*.5*size + just - just*width
	
	# x <- if (just=="left") {
	# 	0	
	# } else if (just=="center") {
	# 	.5-width/2
	# } else 1-width #-mx*size
	# 
	# tx <- if (just=="left") {
	# 	mx*.5*size 
	# } else if (just=="center") {
	# 	.5-width/2+mx*.5*size
	# } else {
	# 	1-width+mx*.5*size
	# }
		
	
	if (gt$credits.align[id]=="center") {
		x <- x + width/2
		tx <- tx + width/2
	} else if (gt$credits.align[id]=="right") {
		x <- x + width
		tx <- tx + width
	}
	
	
	grobBG <- if (gt$design.mode) rectGrob(gp=gpar(fill="orange")) else NULL
	
	col <- do.call("process_color", c(list(gt$credits.col[id], alpha=gt$credits.alpha[id]), gt$pc))
	
	gTree(children=gList(grobBG,
						 if (!is.na(gt$credits.bg.color[id])) {
						 	bg.col <- do.call("process_color", c(list(gt$credits.bg.color[id], alpha=gt$credits.bg.alpha[id]), gt$pc))
						 	rectGrob(x=x, width=width, just="left", gp=gpar(col=NA, fill=bg.col))
						 } else {
						 	NULL
						 }, textGrob(label=txt, x = tx, y =.5, just=c(gt$credits.align[id], "center"), gp=gpar(cex=size, col=col, fontface=gt$credits.fontface[id], fontfamily=gt$credits.fontfamily[id]))), name="credits")
}


plot_compass <- function(gt, just) {
	u <- 1/(gt$compass.nlines)
	#vpComp <- viewport(x=u, y=u, height=1-2*u, width=1-2*u, just=c("left", "bottom"))
	
	light <- do.call("process_color", c(list(gt$compass.color.light, alpha=1), gt$pc))
	dark <- do.call("process_color", c(list(gt$compass.color.dark, alpha=1), gt$pc))
	
	if (gt$compass.type=="4star") {
		s <- c(.5, .5, .57, .5, .5, .43, 0, .5, .43, 1, .5, .57)
		x <- list(rep.int(s, 2))
		y <- list(s[c(10:12, 10:12, 1:3, 1:3, 7:9, 7:9, 4:6, 4:6)])
		id <- rep(1:8, each=3)
		fill <- c(dark, light, dark, light, light, dark, light, dark)
	} else if (gt$compass.type=="8star") {
		s <- c(.5, .5, .56, .5, .5, .44, 0, .5, .38, 1, .5, .62)
		s2 <- c(.5, .62, .7, .5, .56, .7, .5, .38, .3, .5, .44, .3)
		x <- list(c(rep.int(s, 2), rep.int(s2, 2)))
		y <- list(c(s[c(10:12, 10:12, 1:3, 1:3, 7:9, 7:9, 4:6, 4:6)], s2[c(4:6, 1:3, 10:12, 7:9, 10:12, 7:9, 4:6, 1:3)]))
		id <- rep(1:16, each=3)
		fill <- c(dark, light, dark, light, light, dark, light, dark)
	} else if (gt$compass.type=="8star") {
		s <- c(.5, .5, .56, .5, .5, .44, 0, .5, .38, 1, .5, .62)
		s2 <- c(.5, .62, .7, .5, .56, .7, .5, .38, .3, .5, .44, .3)
		x <- list(c(rep.int(s, 2), rep.int(s2, 2)))
		y <- list(c(s[c(10:12, 10:12, 1:3, 1:3, 7:9, 7:9, 4:6, 4:6)], s2[c(4:6, 1:3, 10:12, 7:9, 10:12, 7:9, 4:6, 1:3)]))
		id <- rep(1:16, each=3)
		fill <- c(dark, light, dark, light, light, dark, light, dark)
	} else if (gt$compass.type=="arrow") {
		x <- list(c(.5, .7, .5, .5, .3, .5))
		y <- list(c(1, 0, .2, 1, 0, .2))
		id <- rep(1:2, each=3)
		fill <- c(dark, light)
	} else if (gt$compass.type=="radar") {
		cr <- c(.45, .42, .2, .17, .1)
		LWD <- round(convertWidth(unit(.01, "npc"), "points", valueOnly=TRUE)) * gt$compass.lwd
		
		cd <- seq(1/8, 15/8, by=.25) * pi
		cd2 <- seq(1/4, 7/4, by=.5) * pi
		cd3 <- seq(0, 1.75, by=.25) * pi
		
		x <- list(.5,
				  unlist(lapply(.5 + sin(cd) * cr[1], c, .5)),
				  .5 + c(0, cr[1]-.005, 0, -cr[1]+.005, 0, 0, 0, 0),
				  unlist(lapply(.5 + sin(cd2) * cr[1], c, .5)),
				  .5 + unlist(mapply(c, sin(cd3) * cr[4], sin(cd3) * cr[5], SIMPLIFY=FALSE)))
		
		y <- list(.5,
				  unlist(lapply(.5 + cos(cd) * cr[1], c, .5)),
				  .5 + c(0, 0, 0, 0, 0, cr[1]-.005, 0, -cr[1]+.005),
				  unlist(lapply(.5 + cos(cd2) * cr[1], c, .5)),
				  .5 + unlist(mapply(c, cos(cd3) * cr[4], cos(cd3) * cr[5], SIMPLIFY=FALSE)))
		
	} else if (gt$compass.type=="rose") {
		cr <- c(.45, .42, .2, .17, .1)
		LWD <- convertWidth(unit(.01, "npc"), "points", valueOnly=TRUE) * gt$compass.lwd
		cd <- seq(1/8, 15/8, by=.25) * pi
		cd2 <- seq(1/4, 7/4, by=.5) * pi
		cd3 <- seq(0, 1.75, by=.25) * pi
		
		b <- cr[4]
		a <- 0.4142136 * b # 1/16th circleL
		s <- c(.5, .5, .5+a, .5, .5, .5-a, 0, .5, .5-b, 1, .5, .5+b)
		s2 <- c(.5, .5+b, .78, .5, .5+a, .78, .5, .5-b, .22, .5, .5-a, .22)
		
		id <- rep(1:16, each=3)
		fill <- c(dark, light, dark, light, light, dark, light, dark)
		
		
		x <- list(.5,
				  unlist(lapply(.5 + sin(cd) * cr[1], c, .5)),
				  .5 + unlist(mapply(c, sin(cd3) * cr[4], sin(cd3) * cr[5], SIMPLIFY=FALSE)),
				  c(rep.int(s, 2), rep.int(s2, 2)))
		
		y <- list(.5,
				  unlist(lapply(.5 + cos(cd) * cr[1], c, .5)),
				  .5 + unlist(mapply(c, cos(cd3) * cr[4], cos(cd3) * cr[5], SIMPLIFY=FALSE)),
				  c(s[c(10:12, 10:12, 1:3, 1:3, 7:9, 7:9, 4:6, 4:6)], s2[c(4:6, 1:3, 10:12, 7:9, 10:12, 7:9, 4:6, 1:3)]))
		
	}
	
	
	# rescale
	resc <- function(a) (a-.5)*(gt$compass.size/gt$compass.nlines) + .5
	
	x <- lapply(x, resc)
	y <- lapply(y, resc)
	if (gt$compass.type %in% c("radar", "rose")) cr <- cr * (gt$compass.size/gt$compass.nlines)
	
	
	#x <- (x-.5)*(gt$compass.size/(gt$compass.nlines)) + .5
	#y <- (y-.5)*(gt$compass.size/(gt$compass.nlines)) + .5
	
	if (gt$compass.north!=0) {
		drotate <- gt$compass.north/180*pi
		
		xy <- mapply(function(a,b){
			d <- atan2(b-.5, a-.5)
			r <- sqrt((a-.5)^2 + (b-.5)^2)
			
			list(x=r * sin(d+drotate) + .5,
				 y=r * cos(d+drotate) + .5)
		}, x, y, SIMPLIFY=FALSE)
		x <- lapply(xy, "[", 1)
		y <- lapply(xy, "[", 2)
	} else drotate <- 0
	
	
	# shift
	if (gt$compass.show.labels==1) {
		x <- lapply(x, function(a) a - (u/2) * sin(drotate))
		y <- lapply(y, function(b) b - (u/2) * cos(drotate))
	}
	
	
	
	grobBG <- if (gt$design.mode) rectGrob(gp=gpar(fill="orange")) else NULL
	
	grobLabels <- if (gt$compass.show.labels==0) {
		NULL
	} else {
		selection <- if (gt$compass.show.labels==1) {
			c(TRUE, rep.int(FALSE, 7))
		} else if (gt$compass.show.labels==2) {
			rep.int(c(TRUE, FALSE), 4)
		} else rep.int(TRUE, 8)
		
		labels <- gt$compass.cardinal.directions[c(1, 1, 2, 3, 3, 3, 4, 1)]
		labels[c(2,4,6,8)] <- paste(labels[c(2,4,6,8)], gt$compass.cardinal.directions[c(2, 2, 4, 4)], sep="")
		labels <- labels[selection]
		
		lr <- (1-u)/2
		ld <- (seq(0, 1.75, by=.25) * pi)[selection]
		
		lx <- lr * sin(ld+drotate) + .5
		ly <- lr * cos(ld+drotate) + .5
		textGrob(labels, x=lx, y=ly, just=c("center", "center"), rot=-drotate/pi*180, gp=gpar(col=gt$attr.color, cex=gt$compass.fontsize, fontface=gt$fontface, fontfamily=gt$fontfamily))
	}
	
	grobComp <- if (gt$compass.type %in% c("arrow", "4star", "8star")) {
		polygonGrob(x=x[[1]], y=y[[1]], id=id, gp=gpar(fill=fill, lwd=gt$compass.lwd, col=dark))
	} else if (gt$compass.type=="radar") {
		gTree(children = gList(
			circleGrob(x=x[[1]], y=y[[1]], r = cr[1], gp=gpar(lwd=2*LWD, col=dark, fill=light)),
			polylineGrob(x=x[[2]], y=y[[2]], id=rep(1:8, each=2), gp=gpar(lwd=1*LWD, col=dark)),
			polylineGrob(x=x[[3]], y=y[[3]], id=rep(1:4, each=2), gp=gpar(lwd=2*LWD, col=dark)),
			polylineGrob(x=x[[4]], y=y[[4]], id=rep(1:4, each=2), gp=gpar(lwd=1*LWD, col=dark)),
			circleGrob(x=x[[1]], y=y[[1]], r = cr[2], gp=gpar(lwd=1*LWD, col=dark, fill=NA)),
			circleGrob(x=x[[1]], y=y[[1]], r = cr[3], gp=gpar(lwd=2*LWD, col=dark, fill=light)),
			circleGrob(x=x[[1]], y=y[[1]], r = cr[4], gp=gpar(lwd=1*LWD, col=NA, fill=dark)),
			circleGrob(x=x[[1]], y=y[[1]], r = cr[5], gp=gpar(lwd=1*LWD, col=NA, fill=light)),
			polylineGrob(x=x[[5]], y=y[[5]], id=rep(1:8, each=2), gp=gpar(lwd=2*LWD, col=light))))
	} else if (gt$compass.type=="rose") {
		gTree(children = gList(
			circleGrob(x=x[[1]], y=y[[1]], r = cr[1], gp=gpar(lwd=2*LWD, col=dark, fill=light)),
			polygonGrob(x=x[[4]], y=y[[4]], id=id, gp=gpar(lwd=1*LWD, fill=fill)),
			polylineGrob(x=x[[2]], y=y[[2]], id=rep(1:8, each=2), gp=gpar(lwd=1*LWD, col=dark)),
			circleGrob(x=x[[1]], y=y[[1]], r = cr[2], gp=gpar(lwd=1*LWD, col=dark, fill=NA)),
			circleGrob(x=x[[1]], y=y[[1]], r = cr[3], gp=gpar(lwd=2*LWD, col=dark, fill=light)),
			circleGrob(x=x[[1]], y=y[[1]], r = cr[4], gp=gpar(lwd=1*LWD, col=NA, fill=dark)),
			circleGrob(x=x[[1]], y=y[[1]], r = cr[5], gp=gpar(lwd=1*LWD, col=NA, fill=light)),
			polylineGrob(x=x[[3]], y=y[[3]], id=rep(1:8, each=2), gp=gpar(lwd=2*LWD, col=light))))
	}
	
	
	gTree(children=gList(grobBG, grobComp, grobLabels), name="compass")
}

