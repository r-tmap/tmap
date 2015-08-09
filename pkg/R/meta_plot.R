meta_plot <- function(gt, x, legend_pos, bb, metaX, metaY) {
	# are there any legend elements? if not, title.only=TRUE
	#title.only <- all(sapply(x, is.null))
	has.legend <- !is.null(x)
	
	# legend positioning
	if (is.null(gt$legend.position)) {
		if (gt$free.coords || gt$legend.only) {
			gt$legend.position <- c("left", "top")
		} else {
			gt$legend.position <- c(ifelse(legend_pos<3, "left", "right"), ifelse(legend_pos %in% c(1,4), "bottom", "top"))
		}
	}

	# title positioning
	# titleg: is title attached to legend?
	if (is.null(gt$title.position)) {
		gt$title.position <- if (has.legend) gt$legend.position else c("left", "top")
		titleg <- has.legend
	} else {
		titleg <- (is.character(gt$title.position) && all(gt$title.position==gt$legend.position) && has.legend)	
	}
	snap <- titleg && gt$title.snap.to.legend && gt$title!=""

	# constant needed for margins etc
	lineHeight <- convertHeight(unit(1, "lines"), "npc", valueOnly=TRUE)
	lineWidth <- convertWidth(unit(1, "lines"), "npc", valueOnly=TRUE)
	my <- lineHeight / 2
	mx <- convertWidth(convertHeight(unit(my, "npc"), "inch"), "npc", TRUE)
	
	# title properties
	nlines <- length(strsplit(gt$title, "\n")[[1]])
	
	title.size <- min((1-2*mx) / convertWidth(stringWidth(gt$title), "npc", valueOnly=TRUE), gt$title.size)
	
	titleWidth <- convertWidth(stringWidth(gt$title), "npc", valueOnly=TRUE) * title.size
	titleHeight <- lineHeight * (nlines) * title.size
	
	if (has.legend) {

		zs <- sapply(x, function(y) y$legend.z)
		x <- x[order(zs)]
		
		x <- lapply(x, function(y) {
			name <- y$legend.type
			#id_title <- paste("title", name, sep=".")
			#id_spacer <- paste("spacer", name, sep=".")
			list_spacer <- list(legend.type="spacer", legend.is.portrait=FALSE)
			list_title <- if(y$legend.title=="") NULL else list(legend.type="title", title=y$legend.title, legend.is.portrait=FALSE)
			list(list_spacer, list_title, y)
		})
		x <- do.call("c", x)[-1]
		
		
		legend.title.npc <- convertHeight(unit(1, "lines"), "npc", valueOnly=TRUE) * gt$legend.title.size

		
		# add element for main title
		if (snap && gt$title!="") {
			x <- c(TITLE=list(list(legend.type="TITLE", title=gt$title, legend.is.portrait=FALSE)), x)
		}

		# remove empty legend elements
		x.not.null <- !sapply(x, is.null)
		k <- sum(x.not.null)
		x <- x[x.not.null]

		# shrink heights (remove white space)
		margin <- 0.25 * gt$legend.text.size
		
		heights <- sapply(x, function(p){
			type <- p$legend.type
			port <- p$legend.is.portrait
			if (type=="TITLE") {
				titleHeight
			} else if (port && type %in% c("fill", "bubble.col", "line.col", "line.lwd", "raster")) {
				length(p$legend.labels) * lineHeight * gt$legend.text.size + 2*margin*lineHeight
			} else if (port && type == "bubble.size") {
				sum(pmax(convertHeight(unit(p$legend.sizes, "inch"), "npc", valueOnly=TRUE) * 2 * 1.25, lineHeight * gt$legend.text.size)) + 2*margin*lineHeight
			} else if (!port && type == "bubble.size") {
				max(convertHeight(unit(p$legend.sizes, "inch"), "npc", valueOnly=TRUE) * 2, 1.5*lineHeight*gt$legend.text.size) + 2*margin*lineHeight*gt$legend.text.size + 1.25*lineHeight*gt$legend.text.size
			} else if (!port && type %in% c("fill", "bubble.col", "line.col", "line.lwd", "raster")) {
				2*margin*lineHeight*gt$legend.text.size + 2.75 * lineHeight*gt$legend.text.size
			} else if (type == "spacer") {
				legend.title.npc * .25
			} else if (type == "title") {
				legend.title.npc * 1.5
			} else if (type == "hist") {
				gt$legend.hist.height
			}
		})
		
		legendWidth <- gt$legend.width
		histWidth <- min(gt$legend.hist.width / legendWidth, 1)
		
		# normalize legendHeight
		if (is.character(gt$title.position) && gt$title.position[1]==gt$legend.position[1] && !snap) {
			legendHeight <- min(sum(heights), 1-metaY-titleHeight, gt$legend.height)
		} else {
			legendHeight <- min(sum(heights), 1-metaY, gt$legend.height)
		}
		if (is.character(gt$legend.position)) {
			legend.position <- c(switch(gt$legend.position[1], 
										left=mx+metaX, 
										center=(1-legendWidth)/2, 
										centre=(1-legendWidth)/2, 
										right=1-mx-legendWidth),
								 switch(gt$legend.position[2], 
								 	   top= 1-my-legendHeight - ifelse(titleg && !snap && gt$title!="", titleHeight, 0), 
								 	   center=(1-legendHeight)/2, 
								 	   centre=(1-legendHeight)/2, 
								 	   bottom=my+metaY))		
		} else legend.position <- gt$legend.position
	}
	
	if (is.character(gt$title.position) || snap) {
		title.position <- if (snap) NULL else {
			c(switch(gt$title.position[1], 
					 left=mx+metaX,
					 center=(1-titleWidth)/2,
					 centre=(1-titleWidth)/2,
					 right=1-mx-titleWidth),
			  switch(gt$title.position[2],
			  	     top=1-titleHeight*.75,
			  	     center=.5,
			  	     centre=.5,
			  	     bottom=my+metaY+titleHeight*.5 + ifelse(titleg && !snap && gt$title!="", legendHeight, 0)))	
		}
	} else title.position <- gt$title.position

	
	grobTitle <- if (snap || gt$title=="") {
		NULL
	} else {
		gTree(children=gList(if (gt$design.mode) {
			rectGrob(x = title.position[1]-.5*mx, y = title.position[2], width=titleWidth+mx, just=c("left", "center"), height=titleHeight, gp=gpar(col="black", fill="#888888BB"))
		} else if (!is.na(gt$title.bg.color)) {
			rectGrob(x = title.position[1]-.5*mx, y = title.position[2], width=titleWidth+mx, just=c("left", "center"), height=titleHeight, gp=gpar(col=NA, fill=gt$title.bg.col))
		} else {
			NULL
		}, textGrob(label=gt$title, x = title.position[1], y = title.position[2], just=c("left", "center"), gp=gpar(cex=title.size, fontface=gt$fontface, fontfamily=gt$fontfamily))))
		
	}

	
	if (gt$credits.show || gt$scale.show || gt$compass.show) {
		elems <- do.call("rbind", list(
			if (gt$credits.show) data.frame(type="credits",
				 height=lineHeight * (length(strsplit(gt$credits.text, "\n")[[1]])+1) * 
				 	min((1-2*convertWidth(convertHeight(unit(lineHeight / 2, "npc"), "inch"), "npc", TRUE)) / 
				 			convertWidth(stringWidth(gt$credits.text), "npc", valueOnly=TRUE),
				 		gt$credits.size),
				 width=1,
				 position1=gt$credits.position[1],
				 position2=gt$credits.position[2], stringsAsFactors = FALSE) else NULL,
			if (gt$scale.show) data.frame(type="scale_bar",
				height=3*lineHeight * gt$scale.size,
				width=1,
				position1=gt$scale.position[1],
				position2=gt$scale.position[2], stringsAsFactors = FALSE) else NULL,
			if (gt$compass.show) data.frame(type="compass",
				height=(gt$compass.size * gt$compass.fontsize +2)*lineHeight,
				width=(gt$compass.size * gt$compass.fontsize +2)*lineWidth,
				position1=gt$compass.position[1],
				position2=gt$compass.position[2], stringsAsFactors = FALSE) else NULL))

		elemHeight <- sum(elems$height)

		if (is.character(gt$elem.position)) {
			elemleg <- all(gt$elem.position==gt$legend.position) && has.legend
			elemtitle <- all(gt$elem.position==gt$title.position) && gt$title!="" && !snap
			if (elemleg) {
				elem.position <- c(switch(gt$elem.position[1], 
										  left=metaX+2*mx+legendWidth,
										  center=.5 + mx + legendWidth/2,
										  centre=.5 + mx + legendWidth/2,
										  right=1-mx-legendWidth - mx),
								   switch(gt$elem.position[2],
								   	   top= 1-my-elemHeight, 
								   	   center=.5, 
								   	   centre=.5, 
								   	   bottom=my+metaY))	
				elem.max.width <- 1 - 3*mx - legendWidth
			} else if (elemtitle) {
				elem.position <- c(switch(gt$elem.position[1], 
										  left=mx+metaX,
										  center=.5,
										  centre=.5,
										  right=1-mx),
								   switch(gt$elem.position[2],
								   	   top= 1-my-elemHeight - titleHeight, 
								   	   center=.5, 
								   	   centre=.5, 
								   	   bottom=my+metaY+titleHeight))	
				elem.max.width <- if (has.legend && gt$elem.position[2]==gt$legend.position[2]) 1 - 3*mx - legendWidth else 1 - 2 *mx
			} else {
				elem.position <- c(switch(gt$elem.position[1], 
										  left=mx+metaX,
										  center=.5,
										  centre=.5,
										  right=1-mx),
								   switch(gt$elem.position[2],
								   	   top= 1-my-elemHeight, 
								   	   center=.5, 
								   	   centre=.5, 
								   	   bottom=my+metaY))
				elem.max.width <- if (has.legend && gt$elem.position[2]==gt$legend.position[2]) 1 - 3*mx - legendWidth else 1 - 2*mx
			}
			elem.just <- switch(gt$elem.position[1],
								  right="right",
								  "left")
		} else {
			elem.position <- gt$elem.position
			elem.just <- "left"
			elem.max.width <- 1 - 2*mx
		}
		elems$y <- elem.position[2] + c(0, cumsum(elems$height))[1:nrow(elems)]
		elems$width2 <- pmin(elems$width, elem.max.width)
		
		elems$x <- elem.position[1] - if (elem.just=="left") 0 else elems$width2
		
		elemGrobs <- lapply(1:nrow(elems), function(i) {
			e <- elems[i,]
			vpi <- viewport(x=e$x, 
							y=e$y,
							height=e$height, width=e$width2,
							just=c("left", "bottom"),
							name=as.character(e$type))
			pushViewport(vpi)
			if (e$type=="credits") {
				grb <- plot_cred(gt, just=elem.just)
			} else if (e$type=="scale_bar") {
				grb <- plot_scale(gt, just=elem.just, xrange=(bb[1,2] - bb[1,1])*e$width2, crop_factor=gt$scale.width/e$width2)
			} else {
				grb <- plot_compass(gt, just=elem.just)
			}
			grt <- gTree(children=gList(grb), vp=vpi)
			upViewport()
			grt
		})
		
		treeElem <- gTree(children=do.call("gList", elemGrobs))
	} else {
		treeElem <- NULL
	}
	
	
	
	treeLegend <- if (has.legend) {
	
		vpLegend <- viewport(y=legend.position[2], x=legend.position[1], 
							 height=legendHeight, width=legendWidth, 
							 just=c("left", "bottom"), name="legend")
		
		pushViewport(vpLegend)
		
		legend.frame <- !is.logical(gt$legend.frame) || gt$legend.frame
		legend.bg.color <- gt$legend.bg.color
		
		legend.frame.color <- if (legend.frame) {
			if (is.logical(gt$legend.frame) || gt$design.mode) "black" else gt$legend.frame
		} else NA
		
		legend.frame.fill <- if (gt$design.mode) "#888888BB" else legend.bg.color
		
		grobLegBG <- rectGrob(gp=gpar(col=legend.frame.color, fill=legend.frame.fill))
		
		# normalise heights
		heights <- heights / legendHeight
		vpLeg <- viewport(layout=grid.layout(k, 2, heights=heights, widths=c(histWidth, 1-histWidth)), name="legend_grid")
	
		if (gt$legend.inside.box) {
			vpLegFrame <- viewport(width=.95, height=.95, name="legend_frame")
			vpLeg <- vpStack(vpLegFrame, vpLeg)
		} 
		
		pushViewport(vpLeg)
		grobList <- mapply("legend_subplot", x, id=1:k, MoreArgs = list(gt=gt), SIMPLIFY = FALSE)
	
		upViewport(2)
		gTree(children=gList(grobLegBG, gTree(children=do.call("gList", grobList), vp=vpLeg)), vp=vpLegend)
	} else {
		NULL
	}
	
	treeMeta <- gTree(children=gList(grobTitle, treeElem, treeLegend))
	
	treeMeta
}

elem_subplot <- function(x, id, gt, just) {
	type <- x$type
	cols <- if (type=="credits") c(1,2) else ifelse(just=="left", 1, 2)
	cellplot(id, cols, e={
		lineHeight <- convertHeight(unit(1, "lines"), unitTo="npc", valueOnly=TRUE)
		rectGrob(gp=gpar(col="green", fill=NA))
	})
}


legend_subplot <- function(x, id, gt) {
	legend.type <- x$legend.type
	cols <- if (legend.type=="hist") 1 else c(1,2)
	cellplot(id, cols, e={
	    lineHeight <- convertHeight(unit(1, "lines"), unitTo="npc", valueOnly=TRUE)
		legGrob <- if (legend.type=="hist") {
			legend_hist(x, gt$legend.hist.size, lineHeight, scale=gt$scale, m=.25, legend.hist.bg.color = gt$legend.hist.bg.color)
		} else if (legend.type=="TITLE") {
			legend_title(x, gt, is.main.title=TRUE, lineHeight, m=.1)
		} else if (legend.type=="title") {
			legend_title(x, gt, is.main.title=FALSE, lineHeight, m=.1)
		} else if (legend.type=="spacer") {
			NULL
		} else if (x$legend.is.portrait) {
			legend_portr(x, gt, lineHeight, m=.25)
		} else {
			legend_landsc(x, gt, lineHeight, m=.25)
		}
		if (gt$design.mode) {
			gTree(children=gList(rectGrob(gp=gpar(fill="#CCCCCCCC")), legGrob))	
		} else legGrob
	})
}

legend_title <- function(x, gt, is.main.title, lineHeight, m) {
	size <- ifelse(is.main.title, gt$title.size, gt$legend.title.size)
	title <- x$title
	my <- lineHeight * size * m
	mx <- convertWidth(convertHeight(unit(my, "npc"), "inch"), "npc", TRUE)
	
	w <- convertWidth(stringWidth(paste(title, " ")), unitTo = "npc", valueOnly = TRUE)# * size * 1.05
	newsize <- min(size, 5/(lineHeight*6), (1-2*mx)/w)
	
	
	textGrob(title, x=mx, y=5/12 , just=c("left", "center"), gp=gpar(cex=newsize, fontface=gt$fontface, fontfamily=gt$fontfamily))
}


legend_portr <- function(x, gt, lineHeight, m) {
	legend.text.size <- gt$legend.text.size
	with(x, {
		
		my <- lineHeight * legend.text.size * m
		mx <- convertWidth(convertHeight(unit(my, "npc"), "inch"), "npc", TRUE)
		s <- 1.25 ## for bubbles only
		r <- 1-2*my
		

		if (legend.type=="bubble.size") {
			nitems <- length(legend.labels)
			hs <- convertHeight(unit(legend.sizes, "inch"), "npc", valueOnly=TRUE) * 2
			lhs <- pmax(hs*s, legend.text.size * lineHeight)
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
		size <- pmin(lhs / lineHeight, legend.text.size)
		
		ws <- convertWidth(convertHeight(unit(hs, "npc"), "inch"), "npc", TRUE)
		wsmax <- max(ws)
		hsi <- convertHeight(unit(hs, "npc"), "inch", valueOnly=TRUE)
		
		wstext <- convertWidth(stringWidth(legend.labels), unitTo = "npc", valueOnly = TRUE)
		newsize <- pmin(size, (1-wsmax-4*mx) / wstext)
		
		grobLegendItem <- if (legend.type %in% c("fill", "raster")) {
			fill <- legend.palette
			col <- ifelse(legend.type =="fill", border.col, NA)
			if (legend.type=="raster") lwd <- NA
			rectGrob(x=mx+ws/2, 
					  y=ys, 
					  width= ws, 
					  height= hs,
					  gp=gpar(fill=fill, col=col, lwd=lwd))
		} else if (legend.type %in% c("bubble.size", "bubble.col")) {
			cols <- legend.palette
			circleGrob(x=mx+wsmax/2, 
					y=ys, r=unit(hsi/2, "inch"),
					gp=gpar(fill=cols,
							col=bubble.border.col,
							lwd=bubble.border.lwd))
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
		grobLegendText <- textGrob(legend.labels, x=mx*2+wsmax,
								   y=ys, just=c("left", "center"), gp=gpar(cex=newsize, fontface=gt$fontface, fontfamily=gt$fontfamily))
		gList(grobLegendItem, grobLegendText)
	})
}

## design from top to bottom: .25 margin, 1.5 items, 1.25 text, .25 margin
legend_landsc <- function(x, gt, lineHeight, m) {
	legend.text.size <- gt$legend.text.size
	with(x, {
		#grid.rect()
		
		if (lineHeight*legend.text.size * 3.25 > 1) {
			legend.text.size <- 1/(lineHeight * 3.25)
		}
		
		my <- lineHeight * legend.text.size * m
		mx <- convertWidth(convertHeight(unit(my, "npc"), "inch"), "npc", TRUE)
		rx <- 1-2*mx
		ry <- 1-2*my
		
		nitems <- length(legend.labels)
		# delete too high 
		if (legend.type=="bubble.size") {
			hs <- convertHeight(unit(legend.sizes, "inch"), "npc", valueOnly=TRUE) * 2
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
		} else {
			hs <- rep(1.5*lineHeight*legend.text.size, nitems)
		}
		
		
		labelsws <- convertWidth(stringWidth(paste(legend.labels, " ")), "npc", TRUE) * legend.text.size# * 1.3 #1.2
		maxlabelsws <- max(labelsws)
		
		ws <- rep(maxlabelsws, nitems)
		if (sum(ws)>rx) {
			ratio <- (sum(ws)/rx)
			ws <- ws / ratio
			legend.text.size <- legend.text.size / ratio
		}

		wsmax <- rx/nitems
		
		if (legend.type=="bubble.size") {
			bubblews <- convertWidth(unit(legend.sizes, "inch"), "npc", valueOnly=TRUE) * 2
			ws <- pmax(ws, bubblews*1.1)
			
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
					
		if (legend.type=="bubble.col") {
			bmax <- convertHeight(unit(bubble.max.size, "inch"), "npc", valueOnly=TRUE) * 2
			hs <- pmin(hs, bmax)
		}
		
		hsmax <- max(hs)
		hsi <- convertHeight(unit(hs, "npc"), "inch", valueOnly=TRUE)
		
		
		grobLegendItem <- if (legend.type %in% c("fill", "raster")) {
			fill <- legend.palette
			rectGrob(x=xs, 
					  y=1-my-hs/2, 
					  width= ws, 
					  height= hs,
					  gp=gpar(fill=fill, col=border.col, lwd=lwd))
		} else if (legend.type %in% c("bubble.size", "bubble.col")) {
			cols <- legend.palette
			circleGrob(x=xs, 
						y=1-my-hsmax/2, r=unit(hsi/2, "inch"),
						gp=gpar(fill=cols,
								col=bubble.border.col,
								lwd=bubble.border.lwd))
		} else if (legend.type %in% c("line.col", "line.lwd")) {
			lwds <- if (legend.type == "line.col") line.legend.lwd else legend.lwds
			cols <- legend.palette
			polylineGrob(x=rep(xs, each=2), 
						  y=1-my-c(0,1)*rep(hs, each=2),
						  id=rep(1:nitems, each=2),
						  gp=gpar(col=cols, 
						  		lwd=lwds,
						  		lty=line.legend.lty,
						  		lineend="butt"))
		}
		grobLegendText <- textGrob(legend.labels, x=xs,
				  y=my+lineHeight*legend.text.size, just=c("center", "top"), gp=gpar(cex=legend.text.size, fontface=gt$fontface, fontfamily=gt$fontfamily))
		gList(grobLegendItem, grobLegendText)
	})
}


plot_scale <- function(gt, just, xrange, crop_factor) {
	light <- process_color(gt$scale.color.light, alpha=1, sepia.intensity=gt$sepia.intensity, saturation=gt$saturation)
	dark <- process_color(gt$scale.color.dark, alpha=1, sepia.intensity=gt$sepia.intensity, saturation=gt$saturation)
	
	xrange2 <- xrange/gt$unit.size
	
	if (is.null(gt$scale.breaks)) {
		ticks2 <- pretty(c(0, xrange2*crop_factor), 4)
	} else {
		ticks2 <- gt$scale.breaks
	}
	labels <- c(ticks2, gt$unit)
	
	n <- length(ticks2)
	ticks <- ticks2*gt$unit.size
	ticks3 <- ticks / xrange
	
	widths <- ticks3[2] - ticks3[1]
	x <- ticks3[1:(n-1)]
	
	size <- min(gt$scale.size, widths/max(convertWidth(stringWidth(paste(ticks2, " ")), "npc", TRUE)))
	
	lineHeight <- convertHeight(unit(1, "lines"), "npc", valueOnly=TRUE) * size
	my <- lineHeight / 2
	mx <- convertWidth(convertHeight(unit(my, "npc"), "inch"), "npc", TRUE)
	
	unitWidth <- convertWidth(stringWidth(paste(gt$unit, " ")), "npc", TRUE) * size
	width <- widths * n + unitWidth
	
	xtext <- c(ticks3, ticks3[n] + widths*.5 + unitWidth*.5) #+ position[1]

	if (just=="right") {
		x <- 1-width+x
		xtext <- 1-width+xtext
	}
	
	grobBG <- if (gt$design.mode) rectGrob(gp=gpar(fill="orange")) else NULL
	
	gTree(children=gList(
		grobBG,
		rectGrob(x=x, y=1.5*lineHeight, width = widths, height=lineHeight*.5, just=c("left", "bottom"), gp=gpar(col=dark, fill=c(light, dark))),
		textGrob(label=labels, x = xtext, y = lineHeight, just=c("center", "center"), gp=gpar(cex=size, fontface=gt$fontface, fontfamily=gt$fontfamily))))
	
	
}


plot_cred <- function(gt, just) {
	lineHeight <- convertHeight(unit(1, "lines"), "npc", valueOnly=TRUE)

	my <- lineHeight / 2
	mx <- convertWidth(convertHeight(unit(my, "npc"), "inch"), "npc", TRUE)
	
	# number of lines
	nlines <- length(strsplit(gt$credits.text, "\n")[[1]])
	
	size <- min((1-2*mx) / convertWidth(stringWidth(gt$credits.text), "npc", valueOnly=TRUE), gt$credits.size)
	
	width <- (convertWidth(stringWidth(gt$credits.text), "npc", valueOnly=TRUE)+4*mx) * size
	height <- lineHeight * (nlines) * size

	x <- if (just=="left") 0 else 1-width

	grobBG <- if (gt$design.mode) rectGrob(gp=gpar(fill="orange")) else NULL
	
	gTree(children=gList(grobBG,
						 if (!is.na(gt$credits.bg.color)) {
		bg.col <- process_color(gt$credits.bg.color, alpha=gt$credits.bg.alpha, sepia.intensity=gt$sepia.intensity, saturation=gt$saturation)
		rectGrob(x=x, width=width, just="left", gp=gpar(col=NA, fill=bg.col))
	} else {
		NULL
	}, textGrob(label=gt$credits.text, x = x+mx*size, y =.5, just=c("left", "center"), gp=gpar(cex=size, fontface=gt$fontface, fontfamily=gt$fontfamily))))
}


plot_compass <- function(gt, just) {
	u <- 1/(gt$compass.size+2)
	#vpComp <- viewport(x=u, y=u, height=1-2*u, width=1-2*u, just=c("left", "bottom"))

	
	light <- process_color(gt$compass.color.light, alpha=1, sepia.intensity=gt$sepia.intensity, saturation=gt$saturation)
	dark <- process_color(gt$compass.color.dark, alpha=1, sepia.intensity=gt$sepia.intensity, saturation=gt$saturation)
	
	if (gt$compass.type=="4star") {
		s <- c(.5, .5, .57, .5, .5, .43, 0, .5, .43, 1, .5, .57)
		x <- rep.int(s, 2)
		y <- s[c(10:12, 10:12, 1:3, 1:3, 7:9, 7:9, 4:6, 4:6)]
		id <- rep(1:8, each=3)
		fill <- c(dark, light, dark, light, light, dark, light, dark)
	} else if (gt$compass.type=="arrow") {
		x <- c(.5, .7, .5, .5, .3, .5)
		y <- c(1, 0, .2, 1, 0, .2)
		id <- rep(1:2, each=3)
		fill <- c(dark, light)
	}
	
	# rescale
	x <- (x-.5)*(gt$compass.size/(gt$compass.size+2)) + .5
	y <- (y-.5)*(gt$compass.size/(gt$compass.size+2)) + .5
	
	
	if (gt$compass.north!=0) {
		d <- atan2(y-.5, x-.5)
		r <- sqrt((x-.5)^2 + (y-.5)^2)
		drotate <- gt$compass.north/180*pi
		
		x <- r * sin(d+drotate) + .5
		y <- r * cos(d+drotate) + .5
	} else drotate <- 0
	
	
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
		textGrob(labels, x=lx, y=ly, just=c("center", "center"), rot=-drotate/pi*180, gp=gpar(cex=gt$compass.fontsize, fontface=gt$fontface, fontfamily=gt$fontfamily))
	}
	
	gTree(children=gList(grobBG, polygonGrob(x=x, y=y, id=id, gp=gpar(fill=fill)), grobLabels))
}
	