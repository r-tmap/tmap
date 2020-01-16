gridplot <- function(gmeta, fun, nx, gps, gal, shps, dasp, sasp, inner.margins.new, legend_pos, gp_leg, gp_attr) {
	mfrow <- gmeta$nrow
	mfcol <- gmeta$ncol

	
	np <- gmeta$np
	pp <- gmeta$pp
	
	
	## panels
	panel.mode <- gmeta$panel.mode
	panel.names <- gmeta$panel.names
	
	## number of grid rows and colums
	ncl <- length(gmeta$colws)
	nrw <- length(gmeta$rowhs)
	
	
	# check if first not-null shape is not a sf or raster
	multi_shapes <- !inherits(shps[[which(!vapply(shps, is.null, logical(1)))[1]]], c("sf", "Raster"))
	masterID <- gmeta$shape.masterID
	
	if (multi_shapes) {
		bbxproj <- lapply(shps, function(s) {
			s2 <- s[[masterID]]
			if (is.null(s2)) NULL else list(bbx = bb(s2), proj = sf::st_crs(s2))
		})
	} else {
		bbxproj <- list(bbx = attr(shps[[masterID]], "bbox"), proj = sf::st_crs(shps[[masterID]]))
	}
	
	external_grid_labels <- gmeta$grid.show && !gmeta$grid.labels.inside.frame
	
	
	## create a large grid tree per page, and draw it
	treeMlts <- lapply(1:np, function(k) {
		if (k!=1) {
			grid.newpage()
		}
		

		## in order to keep aspect ratio while resizing
		if (dasp > 1) {
			cw <- dasp
			ch <- 1
		} else {
			ch <- 1/dasp
			cw <- 1
		}
		vpContainer <- viewport(width = unit(cw, "snpc"), height = unit(ch, "snpc"))
		pushViewport(vpContainer)

		## background rect
		grobBG <- if (gmeta$design.mode) {
			rectGrob(gp=gpar(fill="yellow", col=NA), name="bg_rect")
		} else if (is.na(gmeta$frame) && !gmeta$earth.boundary) {
			rectGrob(gp=gpar(fill=gmeta$bg.color, col=NA), name="bg_rect")
		} else if (is.na(gmeta$frame) && gmeta$earth.boundary) {
			rectGrob(gp=gpar(fill=gmeta$space.color, col=NA), name="bg_rect")
		} else if (!is.null(gmeta$outer.bg.color) && !is.na(gmeta$frame)) {
			rectGrob(gp=  gpar(col=gmeta$outer.bg.color, fill=gmeta$outer.bg.color), name="bg_rect")
		} else NULL
		
		## set grid layout
		vpGrid <- viewport(layout=grid.layout(nrw, ncl, 
											  widths=unit(gmeta$colws, "npc"), 
											  heights=unit(gmeta$rowhs, "npc")), name = "multiples_grid")
		pushViewport(vpGrid)
		
		grobBG2 <- if (gmeta$design.mode) {
			cellplot(2:(length(gmeta$rowhs)-1), 2:(length(gmeta$colws)-1), e=rectGrob(gp=gpar(fill="green", col=NA), name="bg_wo_outer"))
		} else NULL
		
		
		## additional background rect for design mode only
		grobFacetBG <- if (gmeta$design.mode) {
			cellplot(4:(length(gmeta$rowhs)-2), 3:(length(gmeta$colws)-2), e=rectGrob(gp=gpar(fill="brown", col=NA), name="bg_facets_rect"))
		} else NULL
		
		## print main title
		grobMainBG <- if (gmeta$main.title[k]!="" && gmeta$design.mode) {
			cellplot(3, 3:(length(gmeta$colws)-2), e=rectGrob(gp=gpar(fill="gold", col=NA), name="bg_main_rect"))
		} else NULL
		

		grobMain <- if (gmeta$main.title[k]!="") {
			cellplot(3,  3:(length(gmeta$colws)-2), e={
				margin <- convertWidth(unit(gmeta$main.title.size, "lines"), "npc", valueOnly = TRUE) * .25
				main_pos <- gmeta$main.title.position
				main_align <- ifelse(is.character(main_pos), ifelse(main_pos %in% c("center", "centre"), "center", ifelse(main_pos == "left", "left", "right")), "left")
				main_pos <- ifelse(is.character(main_pos), ifelse(main_pos %in% c("center", "centre"), .5, ifelse(main_pos == "left", margin, 1-margin)), main_pos)
				textGrob(gmeta$main.title[k], x = main_pos, just = main_align, gp=gpar(cex=gmeta$main.title.size, col=gmeta$main.title.color, fontface=gmeta$main.title.fontface, fontfamily=gmeta$main.title.fontfamily))
			}, name="main_title")
		} else NULL
			
		
		## draw maps
		istart <- (k-1) * pp + 1
		iend <- min(istart + pp-1, nx)
		ni <- iend-istart+1
		treeMults <- mapply(function(i, rw, cl) {
			#cellplot(rw, cl, e=grid.rect(gp=gpar(fill="blue")))
			cellplot(rw, cl, e=do.call(fun, args=list(i, gps[[i]], gal, shps, dasp, sasp, inner.margins.new, legend_pos, nx>1)), name = paste("multiple", i, sep="_"))
		}, istart:iend, 
		rep(gmeta$rowrange, each=mfcol, length.out=ni), 
		rep(gmeta$colrange, times=mfrow, length.out=ni), SIMPLIFY=FALSE)
		
		## draw outside grid labels
		treeGridLabels <- if (external_grid_labels && gmeta$grid.show && gmeta$grid.labels.show) {
			mapply(function(i, rw, cl) {
				if (multi_shapes) {
					proj <- bbxproj[[i]]$proj
					bbx <- bbxproj[[i]]$bbx
				} else {
					proj <- bbxproj$proj
					bbx <- bbxproj$bbx
				}
				gt <- gps[[i]]$tm_layout
				if (gt$grid.show) {
					gt <- process_grid(gt, bbx, proj, sasp)
				}
				gTree(children=gList(
					#cellplot((rw+1),cl, e=rectGrob(gp=gpar(fill="purple")), name="gridLabelsX"),
					#cellplot(rw,(cl-1), e=rectGrob(gp=gpar(fill="grey")), name="gridLabelsY")), name=paste("gridLabels", i, sep="_"))
					cellplot((rw+1),cl, clip = FALSE, e=plot_grid_labels_x(gt, scale=gt$scale), name="gridLabelsX"),
					cellplot(rw,(cl-1), clip = FALSE, e=plot_grid_labels_y(gt, scale=gt$scale), name="gridLabelsY")), name=paste("gridLabels", i, sep="_"))
			}, istart:iend, 
			rep(gmeta$rowrange, each=mfcol, length.out=ni), 
			rep(gmeta$colrange, times=mfrow, length.out=ni), SIMPLIFY=FALSE)
		} else NULL
		
		## draw panels		
		if (panel.mode=="both") {
			rowPanels <- lapply((1:mfrow), function(i) {
				cellplot(gmeta$rowrange[i], gmeta$rowpanelcol, e=gList(rectGrob(gp=gpar(fill=gmeta$panel.label.bg.color, lwd=gmeta$frame.lwd)),
									   textGrob(panel.names[[1]][i], rot=gmeta$panel.label.rot[1], gp=gpar(col=gmeta$panel.label.color, cex=gmeta$panel.label.size, fontface=gmeta$panel.label.fontface, fontfamily=gmeta$panel.label.fontfamily))))
			})
			
			colPanels <- lapply((1:mfcol), function(i) {
				cellplot(gmeta$colpanelrow, gmeta$colrange[i], e=gList(rectGrob(gp=gpar(fill=gmeta$panel.label.bg.color, lwd=gmeta$frame.lwd)),
									   textGrob(panel.names[[2]][i], rot=gmeta$panel.label.rot[2], gp=gpar(col=gmeta$panel.label.color, cex=gmeta$panel.label.size, fontface=gmeta$panel.label.fontface, fontfamily=gmeta$panel.label.fontfamily))))
			})
		}  else if (panel.mode=="one") {
			colPanels <- mapply(function(i, rw, cl) {
				cellplot(rw, cl, e=gList(rectGrob(gp=gpar(fill=gmeta$panel.label.bg.color, lwd=gmeta$frame.lwd)),
										 textGrob(panel.names[i], rot=gmeta$panel.label.rot[2], gp=gpar(col=gmeta$panel.label.color, cex=gmeta$panel.label.size, fontface=gmeta$panel.label.fontface, fontfamily=gmeta$panel.label.fontfamily))))
			}, istart:iend, 
			rep(gmeta$rowrange-1, each=mfcol, length.out=ni), 
			rep(gmeta$colrange, times=mfrow, length.out=ni), SIMPLIFY=FALSE)
			rowPanels <- NULL
		} else {
			rowPanels <- NULL
			colPanels <- NULL
		}

		## draw outside legend
		if (!is.null(gp_leg)) {
			legPanel <- gList(cellplot(gmeta$legy, gmeta$legx, e=do.call(fun, args=list(1, gp_leg[[k]], gal, shps, dasp, sasp, inner.margins.new, legend_pos, nx>1)), name = "outside_legend"))
		} else {
			legPanel <- NULL
		}
		
		## draw attributes legend
		if (!is.null(gp_attr)) {
			attrPanel <- gList(cellplot(gmeta$attry, gmeta$attrx, e=do.call(fun, args=list(1, gp_attr[[k]], gal, shps, dasp, sasp, inner.margins.new, legend_pos, nx>1)), name = "outside_attr"))
		} else {
			attrPanel <- NULL
		}

		if (gmeta$xlab.show) {
			y <- unit(gmeta$xlab.nlines/2 + .2, "lines")
			xlabPanel <- gList(cellplot(gmeta$xlaby, gmeta$xlabx, e=textGrob(gmeta$xlab.text, rot=gmeta$xlab.rotation, y = y, gp=gpar(cex=gmeta$xlab.size, fontface=gmeta$fontface, fontfamily=gmeta$fontfamily)), name = "xlab"))
		} else {
			xlabPanel <- NULL
		}
		
		if (gmeta$ylab.show) {
			x <- unit(gmeta$ylab.nlines/2 + .2, "lines")
			ylabPanel <- gList(cellplot(gmeta$ylaby, gmeta$ylabx, e=textGrob(gmeta$ylab.text, rot=gmeta$ylab.rotation, x = x, gp=gpar(cex=gmeta$ylab.size, fontface=gmeta$fontface, fontfamily=gmeta$fontfamily)), name = "ylab"))
		} else {
			ylabPanel <- NULL
		}
		
		
		tree <- gTree(children=do.call("gList", c(list(grobBG, grobBG2, grobFacetBG, grobMainBG, grobMain), treeGridLabels, treeMults, rowPanels, colPanels, legPanel, attrPanel, xlabPanel, ylabPanel)), vp=vpGrid)
		grid.draw(tree)
	})
	upViewport(2)
	invisible()
}
