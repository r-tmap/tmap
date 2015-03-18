legend_plot <- function(gt, x, legend_pos) {
	title.only <- all(sapply(x, is.null))
	lineHeight <- convertHeight(unit(1, "lines"), "npc", valueOnly=TRUE)
	
	if (is.null(gt$legend.position)) {
		if (gt$free.coords) {
			gt$legend.position <- c("left", "top")
		} else {
			gt$legend.position <- c(ifelse(legend_pos<3, "left", "right"), ifelse(legend_pos %in% c(1,4), "bottom", "top"))
		}
	}
	if (is.null(gt$title.position)) {
		if (title.only || gt$free.coords) {
			gt$title.position <- c("left", "top")
		} else {
			gt$title.position <- c(ifelse(legend_pos<3, "left", "right"), ifelse(legend_pos %in% c(1,4), "bottom", "top"))
		}
	}

	
	#legend.position
	conf <- gt$legend.config 
	ishist <- (substr(conf, nchar(conf)-3, nchar(conf))=="hist")
	conf2 <- ifelse(ishist, substr(conf, 1, nchar(conf)-5), conf)
	
	if (!length(conf) || gt$legend.only) title.only <- TRUE

	if (!title.only) {
		for (i in 1:length(gt$legend.titles)) {
			lt <- gt$legend.titles[i]
			if (lt!="") {
				name <- names(lt)
				id <- which(conf2==name)[1]
				id2 <- paste("title", name, sep=".")
				if (!is.na(id)) {
					conf <- if (id==1) c(id2, conf) else c(conf[1:(id-1)], id2, conf[id:length(conf)])
					conf2 <- if (id==1) c(id2, conf2) else c(conf2[1:(id-1)], id2, conf2[id:length(conf2)])
					lst <- list(list(legend.type="title", title=lt))
					names(lst) <- id2
					x <- c(x, lst)
				}
			}
		}
		
		
		x <- x[conf]
		
		legend.title.npc <- convertHeight(unit(1, "lines"), "npc", valueOnly=TRUE) * gt$legend.title.cex * 2
		
		x.not.null <- !sapply(x, is.null)
		
		k <- sum(x.not.null)
		
		x <- x[x.not.null]
		
		types <- ifelse(sapply(x, function(y)if (is.null(y$legend.is.portrait)) FALSE else y$legend.is.portrait), "portrait", "landscape")
		types[names(x)=="fill_hist"] <- "hist"
		types[substr(names(x), 1, 5)=="title"] <- "title"
		
		
# 		heights <- gt$legend.heights[types]
# 		names(heights) <- names(x)
# 		heights[is.na(heights)] <- legend.title.npc
# 		heights <- heights[conf][x.not.null]
		
		# shrink heights (remove white space)
		margin <- 0.25 * gt$legend.text.cex
		heights <- mapply(function(p, hname) {
			if (p && hname %in% c("fill", "bubble.col", "line.col", "line.lwd")) {
				length(x[[hname]]$legend.labels) * lineHeight * gt$legend.text.cex + 2*margin*lineHeight
			} else if (p && hname == "bubble.size") {
				sum(pmax(convertHeight(unit(x[[hname]]$legend.sizes, "inch"), "npc", valueOnly=TRUE) * 2 * 1.25, lineHeight * gt$legend.text.cex)) + 2*margin*lineHeight
			} else if (!p && hname == "bubble.size") {
				max(convertHeight(unit(x[[hname]]$legend.sizes, "inch"), "npc", valueOnly=TRUE) * 2, 1.5*lineHeight*gt$legend.text.cex) + 2*margin*lineHeight*gt$legend.text.cex + 1.25*lineHeight*gt$legend.text.cex
			} else if (!p && hname %in% c("fill", "bubble.col", "line.col", "line.lwd")) {
				2*margin*lineHeight*gt$legend.text.cex + 2.75 * lineHeight*gt$legend.text.cex
			} else if (substr(hname, 1, 5) == "title") {
				legend.title.npc
			} else if (hname == "fill_hist") {
				gt$legend.hist.height
			}
		}, types=="portrait", names(x))
		
	} else {
		heights <- 0
	}

	# normalize heights
	nlines <- length(strsplit(gt$title, "\n")[[1]])

 	titleWidth <- convertWidth(stringWidth(gt$title), "npc", valueOnly=TRUE) * gt$title.cex
 	titleHeight <- lineHeight * (nlines+1) * gt$title.cex

	legendWidth <- gt$legend.width

	# normalize legendHeight
	if (is.character(gt$title.position) && gt$title.position[1]==gt$legend.position[1]) {
		legendHeight <- min(sum(heights), 1-titleHeight, gt$legend.height)
	} else {
		legendHeight <- min(sum(heights), 1, gt$legend.height)
	}

# translate automatic position settings
	positions <- with(gt, {
		if (is.character(title.position) && all(title.position==legend.position)){
			## title is stacked on legend
			title.position <- c(switch(title.position[1], 
									   left=0.02, 
									   center=(1-titleWidth)/2, 
									   centre=(1-titleWidth)/2, 
									   right=.98-titleWidth),
								switch(title.position[2], 
									   top=1-titleHeight*.75, 
									   center=(1-titleHeight)/2, 
									   centre=(1-titleHeight)/2, 
									   bottom=0.02+titleHeight*.25+legendHeight))
			legend.position <- c(switch(legend.position[1], 
										left=0.02, 
										center=(1-legendWidth)/2, 
										centre=(1-legendWidth)/2, 
										right=.98-legendWidth),
								 switch(legend.position[2], 
								 	   top=1-legendHeight-titleHeight, 
								 	   center=(1-legendHeight)/2, 
								 	   centre=(1-legendHeight)/2, 
								 	   bottom=0.02))		
		} else {
			if (is.character(title.position)) {
				title.position <- c(switch(title.position[1], 
										   left=0.02, 
										   center=(1-titleWidth)/2, 
										   centre=(1-titleWidth)/2, 
										   right=.98-titleWidth),
									switch(title.position[2], 
										   top=1-titleHeight*.75, 
										   center=(1-titleHeight)/2, 
										   centre=(1-titleHeight)/2, 
										   bottom=titleHeight*.25))	
			}
			
			if (is.character(legend.position)) {
				legend.position <- c(switch(legend.position[1], 
											left=0.02, 
											center=(1-legendWidth)/2, 
											centre=(1-legendWidth)/2, 
											right=0.98-legendWidth),
									 switch(legend.position[2], 
									 	   top=.98-legendHeight, 
									 	   center=(1-legendHeight)/2, 
									 	   centre=(1-legendHeight)/2, 
									 	   bottom=0.02))
			}
		}
		list(title.position, legend.position)
	})
	
	title.position <- positions[[1]]
	legend.position <- positions[[2]]

	grobTitle <- plot_text(co.npc=matrix(title.position, ncol=2), 
			  g=list(text=gt$title, text.cex=gt$title.cex, 
			  	   text.cex.lowerbound=gt$title.cex, 
			  	   text.fontcolor="black",
			  	   text.shadow=FALSE,
			  	   text.bg.color=gt$title.bg.color, 
			  	   text.bg.alpha=255, 
			  	   text.scale=1,
			  	   text.print.tiny=FALSE, 
			  	   text.fontface="plain", 
			  	   text.fontfamily="sans",
			  	   text_sel=TRUE,
			  	   text.xmod=0,
			  	   text.ymod=0), 
			  just=c("left", "bottom"))

	if (title.only) {
		return(grobTitle)
	}

	vpLegend <- viewport(y=legend.position[2], x=legend.position[1], 
						 height=legendHeight, width=legendWidth, 
						 just=c("left", "bottom"), name="legend")
	
	pushViewport(vpLegend)
	grobLegBG <- if (!is.na(gt$legend.bg.color)) {
		rectGrob(gp=gpar(col=NA, fill=gt$legend.bg.color))
	} else {
		NULL
	}
	heights <- heights / legendHeight
	vpLeg <- viewport(layout=grid.layout(k, 1, heights=heights, widths=1), name="legend_grid")
	pushViewport(vpLeg)
	grobList <- lapply(x, FUN="legend_subplot", gt)

	treeLegend <- gTree(children=gList(grobTitle, gTree(children=gList(grobLegBG, gTree(children=do.call("gList", grobList), vp=vpLeg)), vp=vpLegend)))
	upViewport(2)
	treeLegend
}


legend_subplot <- function(x, gt) {
	id <- substitute(x)[[3]]
	cellplot2(id, 1, e={
		lineHeight <- convertHeight(unit(1, "lines"), unitTo="npc", valueOnly=TRUE)
		legend.type <- x$legend.type
		if (legend.type=="fill_hist") {
			legend_hist(x, gt$legend.hist.cex, lineHeight, scale=gt$scale)
		} else if (legend.type=="title") {
			legend_title(x, gt$legend.title.cex, lineHeight)
		} else if (x$legend.is.portrait) {
			legend_portr(x, gt$legend.text.cex, lineHeight)
		} else {
			legend_landsc(x, gt$legend.text.cex, lineHeight)
		}	
	})
}

legend_title <- function(x, legend.title.cex, lineHeight) {
	#grid.rect()
	cex <- min(legend.title.cex, 1/lineHeight)
	textGrob(x$title, x=0, y=5/12 , just=c("left", "center"), gp=gpar(cex=cex))
}


legend_portr <- function(x, legend.text.cex, lineHeight) {
	with(x, {
		
		my <- lineHeight * legend.text.cex / 4
		mx <- convertWidth(convertHeight(unit(my, "npc"), "inch"), "npc", TRUE)
		s <- 1.25 ## for bubbles only
		r <- 1-2*my
		

		if (legend.type=="bubble.size") {
			nitems <- length(legend.labels)
			hs <- convertHeight(unit(legend.sizes, "inch"), "npc", valueOnly=TRUE) * 2
			lhs <- pmax(hs*s, legend.text.cex * lineHeight)
			if (sum(lhs)>r+1e-6) {
				clipID <- which(cumsum(lhs) > r)[1]
				hs <- hs[1:(clipID-1)]
				lhs <- lhs[1:(clipID-1)]
				legend.labels <- legend.labels[1:(clipID-1)]
			}
		} else {
			nitems <- length(legend.labels)
			lhs <- hs <- rep(r / nitems, nitems)
		}
		
		if (legend.type=="bubble.col") {
			bmax <- convertHeight(unit(bubble.max.size, "inch"), "npc", valueOnly=TRUE) * 2
			hs <- pmin(hs/s, bmax)
		}

		ys <- 1 - my - cumsum(lhs) + lhs/2
		cex <- pmin(lhs / lineHeight, legend.text.cex)
		
		ws <- convertWidth(convertHeight(unit(hs, "npc"), "inch"), "npc", TRUE)
		wsmax <- max(ws)
		hsi <- convertHeight(unit(hs, "npc"), "inch", valueOnly=TRUE)
			
		
		grobLegendItem <- if (legend.type=="fill") {
			fill <- get_alpha_col(legend.palette, alpha)
			col <- get_alpha_col(border.col, border.alpha)
			rectGrob(x=mx+ws/2, 
					  y=ys, 
					  width= ws, 
					  height= hs,
					  gp=gpar(fill=fill, col=col, lwd=lwd))
		} else if (legend.type %in% c("bubble.size", "bubble.col")) {
			bordercol <- get_alpha_col(bubble.border.col, bubble.border.alpha)
			cols <- get_alpha_col(legend.palette, bubble.alpha)
			circleGrob(x=mx+wsmax/2, 
					y=ys, r=unit(hsi/2, "inch"),
					gp=gpar(fill=cols,
							col=bordercol,
							lwd=bubble.border.lwd))
		} else if (legend.type %in% c("line.col", "line.lwd")) {
			lwds <- if (legend.type == "line.col") line.legend.lwd else legend.lwds
			cols <- get_alpha_col(legend.palette, line.legend.alpha)
			polylineGrob(x=mx+ c(0,1)*rep(ws, each=2),
  				  y=rep(ys, each=2), 
  				  id=rep(1:nitems, each=2),
  				  gp=gpar(col=cols, 
  				  		lwd=lwds,
  				  		lty=line.legend.lty,
  				  		lineend="butt"))
		}
		grobLegendText <- textGrob(legend.labels, x=mx*2+wsmax,
								   y=ys, just=c("left", "center"), gp=gpar(cex=cex))
		gList(grobLegendItem, grobLegendText)
	})
}

## design from top to bottom: .25 margin, 1.5 items, 1.25 text, .25 margin
legend_landsc <- function(x, legend.text.cex, lineHeight) {
	with(x, {
		#grid.rect()
		
		if (lineHeight*legend.text.cex * 3.25 > 1) {
			legend.text.cex <- 1/(lineHeight * 3.25)
		}
		
		my <- lineHeight * legend.text.cex / 4
		mx <- convertWidth(convertHeight(unit(my, "npc"), "inch"), "npc", TRUE)
		rx <- 1-2*mx
		ry <- 1-2*my
		
		nitems <- length(legend.labels)
		# delete too high 
		if (legend.type=="bubble.size") {
			hs <- convertHeight(unit(legend.sizes, "inch"), "npc", valueOnly=TRUE) * 2
			nofit <- which(hs>(ry-1.25*lineHeight*legend.text.cex))
			
			if (length(nofit)) {
				clipID <- nofit[1]
				nitems <- clipID - 1
				legend.labels <- legend.labels[1:nitems]
				if (length(legend.palette)>1) legend.palette <- legend.palette[1:nitems]
				legend.sizes <- legend.sizes[1:nitems]
				hs <- hs[1:nitems]
			}
		} else {
			hs <- rep(1.5*lineHeight*legend.text.cex, nitems)
		}
		
		
		labelsws <- convertWidth(stringWidth(legend.labels), "npc", TRUE) * legend.text.cex * 1.15
		maxlabelsws <- max(labelsws)
		
		ws <- rep(maxlabelsws, nitems)
		if (sum(ws)>rx) {
			ratio <- (sum(ws)/rx)
			ws <- ws / ratio
			legend.text.cex <- legend.text.cex / ratio
		}
		
		if (legend.type=="bubble.size") {
			bubblews <- convertWidth(unit(legend.sizes, "inch"), "npc", valueOnly=TRUE) * 2
			clipID2 <- which(bubblews>ws)[1]
			if (!is.na(clipID2)) {
				nitems <- clipID2 - 1
				legend.labels <- legend.labels[1:nitems]
				if (length(legend.palette)>1) legend.palette <- legend.palette[1:nitems]
				legend.sizes <- legend.sizes[1:nitems]
				hs <- hs[1:nitems]
				ws <- ws[1:nitems]
			}
		}
		
		xs <- mx + cumsum(ws) - ws/2
					
		if (legend.type=="bubble.col") {
			bmax <- convertHeight(unit(bubble.max.size, "inch"), "npc", valueOnly=TRUE) * 2
			hs <- pmin(hs, bmax)
		}
		
		hsmax <- max(hs)
		hsi <- convertHeight(unit(hs, "npc"), "inch", valueOnly=TRUE)
		
		
		grobLegendItem <- if (legend.type=="fill") {
			fill <- get_alpha_col(legend.palette, alpha)
			col <- get_alpha_col(border.col, border.alpha)
			rectGrob(x=xs, 
					  y=1-my-hs/2, 
					  width= ws, 
					  height= hs,
					  gp=gpar(fill=fill, col=col, lwd=lwd))
		} else if (legend.type %in% c("bubble.size", "bubble.col")) {
			bordercol <- get_alpha_col(bubble.border.col, bubble.border.alpha)
			cols <- get_alpha_col(legend.palette, bubble.alpha)
			circleGrob(x=xs, 
						y=1-my-hsmax/2, r=unit(hsi/2, "inch"),
						gp=gpar(fill=cols,
								col=bordercol,
								lwd=bubble.border.lwd))
		} else if (legend.type %in% c("line.col", "line.lwd")) {
			lwds <- if (legend.type == "line.col") line.legend.lwd else legend.lwds
			cols <- get_alpha_col(legend.palette, line.legend.alpha)
			polylineGrob(x=rep(xs, each=2), 
						  y=1-my-c(0,1)*rep(hs, each=2),
						  id=rep(1:nitems, each=2),
						  gp=gpar(col=cols, 
						  		lwd=lwds,
						  		lty=line.legend.lty,
						  		lineend="butt"))
		}
		grobLegendText <- textGrob(legend.labels, x=xs,
				  y=my+lineHeight*legend.text.cex, just=c("center", "top"), gp=gpar(cex=legend.text.cex))
		gList(grobLegendItem, grobLegendText)
	})
}

