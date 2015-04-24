legend_plot <- function(gt, x, legend_pos) {
	# are there any legend elements? if not, title.only=TRUE
	title.only <- all(sapply(x, is.null))

	# check legend configuration
	conf <- gt$legend.config 
	ishist <- (substr(conf, nchar(conf)-3, nchar(conf))=="hist")
	conf2 <- ifelse(ishist, substr(conf, 1, nchar(conf)-5), conf)
	if (!length(conf)) title.only <- TRUE # || gt$legend.only
	
	# legend positioning
	if (is.null(gt$legend.position)) {
		if (gt$free.coords) {
			gt$legend.position <- c("left", "top")
		} else {
			gt$legend.position <- c(ifelse(legend_pos<3, "left", "right"), ifelse(legend_pos %in% c(1,4), "bottom", "top"))
		}
	}

	# title positioning
	# titleg: is title attached to legend?
	if (is.null(gt$title.position)) {
		gt$title.position <- if (title.only) c("left", "top") else gt$legend.position
		titleg <- !title.only
	} else {
		titleg <- (is.character(gt$title.position) && all(gt$title.position==gt$legend.position) && !title.only)	
	}

	# constant needed for margins etc
	lineHeight <- convertHeight(unit(1, "lines"), "npc", valueOnly=TRUE)
	my <- lineHeight / 2
	mx <- convertWidth(convertHeight(unit(my, "npc"), "inch"), "npc", TRUE)
	
	# title properties
	nlines <- length(strsplit(gt$title, "\n")[[1]])
	titleWidth <- convertWidth(stringWidth(gt$title), "npc", valueOnly=TRUE) * gt$title.size
	titleHeight <- lineHeight * (nlines+.5) * gt$title.size
	
	if (!title.only) {
		# put legend titles and spacers between legend elements
		for (i in 1:length(gt$legend.titles)) {
			lt <- gt$legend.titles[i]
			if (lt!="") {
				name <- names(lt)
				id <- which(conf2==name)[1]
				id2 <- paste("title", name, sep=".")
				id3 <- paste("spacer", name, sep=".")
				if (!is.na(id)) {
					conf <- if (id==1) c(id2, conf) else c(conf[1:(id-1)], id3, id2, conf[id:length(conf)])
					conf2 <- if (id==1) c(id2, conf2) else c(conf2[1:(id-1)], id3, id2, conf2[id:length(conf2)])
					lst <- list(if (id==1) NULL else list(legend.type="spacer"), list(legend.type="title", title=lt))
					names(lst) <- c(id3, id2)
					x <- c(x, lst)
				}
			}
		}
		x <- x[conf]
		
		legend.title.npc <- convertHeight(unit(1, "lines"), "npc", valueOnly=TRUE) * gt$legend.title.size

		
		# add element for main title
		if (titleg) {
			x <- c(TITLE=list(list(legend.type="TITLE", title=gt$title)), x)
		}

		# remove empty legend elements
		x.not.null <- !sapply(x, is.null)
		k <- sum(x.not.null)
		x <- x[x.not.null]

		# determine legend types (portrait, landscape, hist, or title)
		types <- ifelse(sapply(x, function(y)if (is.null(y$legend.is.portrait)) FALSE else y$legend.is.portrait), "portrait", "landscape")
		types[names(x)=="fill_hist"] <- "hist"
		types[substr(names(x), 1, 5)=="title"] <- "title"
		types[names(x)=="TITLE"] <- "title"
		
		
		# shrink heights (remove white space)
		margin <- 0.25 * gt$legend.text.size
		heights <- mapply(function(p, hname) {
			if (hname=="TITLE") {
				titleHeight
			} else if (p && hname %in% c("fill", "bubble.col", "line.col", "line.lwd", "raster")) {
				length(x[[hname]]$legend.labels) * lineHeight * gt$legend.text.size + 2*margin*lineHeight
			} else if (p && hname == "bubble.size") {
				sum(pmax(convertHeight(unit(x[[hname]]$legend.sizes, "inch"), "npc", valueOnly=TRUE) * 2 * 1.25, lineHeight * gt$legend.text.size)) + 2*margin*lineHeight
			} else if (!p && hname == "bubble.size") {
				max(convertHeight(unit(x[[hname]]$legend.sizes, "inch"), "npc", valueOnly=TRUE) * 2, 1.5*lineHeight*gt$legend.text.size) + 2*margin*lineHeight*gt$legend.text.size + 1.25*lineHeight*gt$legend.text.size
			} else if (!p && hname %in% c("fill", "bubble.col", "line.col", "line.lwd", "raster")) {
				2*margin*lineHeight*gt$legend.text.size + 2.75 * lineHeight*gt$legend.text.size
			} else if (substr(hname, 1, 6) == "spacer") {
				legend.title.npc * .25
			} else if (substr(hname, 1, 5) == "title") {
				legend.title.npc * 1.5
			} else if (hname == "fill_hist") {
				gt$legend.hist.height
			}
		}, types=="portrait", names(x))
		
		legendWidth <- gt$legend.width
		
		# normalize legendHeight
		if (is.character(gt$title.position) && gt$title.position[1]==gt$legend.position[1]) {
			legendHeight <- min(sum(heights), 1-titleHeight, gt$legend.height)
		} else {
			legendHeight <- min(sum(heights), 1, gt$legend.height)
		}
		if (is.character(gt$legend.position)) {
			legend.position <- c(switch(gt$legend.position[1], 
										left=mx, 
										center=(1-legendWidth)/2, 
										centre=(1-legendWidth)/2, 
										right=1-mx-legendWidth),
								 switch(gt$legend.position[2], 
								 	   top=1-my-legendHeight, #-titleHeight, 
								 	   center=(1-legendHeight)/2, 
								 	   centre=(1-legendHeight)/2, 
								 	   bottom=my))		
		} else legend.position <- gt$legend.position
	} else {
		heights <- 0
	}
	
	if (is.character(gt$title.position) || titleg) {
		title.position <- if (titleg) NULL else {
			c(switch(gt$title.position[1], 
					 left=mx,
					 center=(1-titleWidth)/2,
					 centre=(1-titleWidth)/2,
					 right=1-mx-titleWidth),
			  switch(gt$title.position[2],
			  	     top=1-titleHeight*.75,
			  	     center=.5,
			  	     centre=.5,
			  	     bottom=titleHeight*.25))	
		}
	} else title.position <- gt$title.position

	
	grobTitle <- if (titleg) {
		NULL
	} else {
		gTree(children=gList(if (!is.na(gt$title.bg.color)) {
			title.bg.col <- get_alpha_col(gt$title.bg.color, gt$title.bg.alpha)
			rectGrob(x = title.position[1]-.5*mx, y = title.position[2], width=titleWidth+mx, just=c("left", "center"), height=titleHeight, gp=gpar(col=NA, fill=title.bg.col))
		} else {
			NULL
		}, textGrob(label=gt$title, x = title.position[1], y = title.position[2], just=c("left", "bottom"), gp=gpar(cex=gt$title.size))))
		
	}

	if (title.only) return(grobTitle)

	vpLegend <- viewport(y=legend.position[2], x=legend.position[1], 
						 height=legendHeight, width=legendWidth, 
						 just=c("left", "bottom"), name="legend")
	
	pushViewport(vpLegend)
	
	legend.frame <- !is.logical(gt$legend.frame) || gt$legend.frame
	legend.bg.color <- if (is.na(gt$legend.bg.color)) NA else get_alpha_col(gt$legend.bg.color, gt$legend.bg.alpha)
	
	legend.frame.color <- if (legend.frame) {
		if (is.logical(gt$legend.frame)) "black" else gt$legend.frame
	} else NA
	
	grobLegBG <- rectGrob(gp=gpar(col=legend.frame.color, fill=legend.bg.color))
	
	# normalise heights
	heights <- heights / legendHeight
	vpLeg <- viewport(layout=grid.layout(k, 1, heights=heights, widths=1), name="legend_grid")

	if (gt$legend.inside.box) {
		vpLegFrame <- viewport(width=.95, height=.95, name="legend_frame")
		vpLeg <- vpStack(vpLegFrame, vpLeg)
	} 
	
	pushViewport(vpLeg)
	grobList <- mapply("legend_subplot", x, id=1:k, MoreArgs = list(gt=gt), SIMPLIFY = FALSE)

	upViewport(2)

	treeLegend <- gTree(children=gList(grobTitle, gTree(children=gList(grobLegBG, gTree(children=do.call("gList", grobList), vp=vpLeg)), vp=vpLegend)))
	
	treeLegend
}


legend_subplot <- function(x, id, gt) {
	cellplot2(id, 1, e={
		lineHeight <- convertHeight(unit(1, "lines"), unitTo="npc", valueOnly=TRUE)
		legend.type <- x$legend.type
		if (legend.type=="fill_hist") {
			legend_hist(x, gt$legend.hist.size, lineHeight, scale=gt$scale, m=.25)
		} else if (legend.type=="TITLE") {
			legend_title(x, gt$title.size, lineHeight, m=.15)
		} else if (legend.type=="title") {
			legend_title(x, gt$legend.title.size, lineHeight, m=.3)
		} else if (legend.type=="spacer") {
			NULL
		} else if (x$legend.is.portrait) {
			legend_portr(x, gt$legend.text.size, lineHeight, m=.25)
		} else {
			legend_landsc(x, gt$legend.text.size, lineHeight, m=.25)
		}	
	})
}

legend_title <- function(x, size, lineHeight, m) {
	my <- lineHeight * size * m
	mx <- convertWidth(convertHeight(unit(my, "npc"), "inch"), "npc", TRUE)
	title <- x$title
	w <- convertWidth(stringWidth(title), unitTo = "npc", valueOnly = TRUE) * size
	newsize <- min(size, 1/lineHeight, size*(1-2*mx)/w)
	textGrob(title, x=mx, y=5/12 , just=c("left", "center"), gp=gpar(cex=newsize))
}


legend_portr <- function(x, legend.text.size, lineHeight, m) {
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
			
		
		grobLegendItem <- if (legend.type %in% c("fill", "raster")) {
			fill <- get_alpha_col(legend.palette, alpha)
			col <- ifelse(legend.type =="fill", get_alpha_col(border.col, border.alpha), NA)
			if (legend.type=="raster") lwd <- NA
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
								   y=ys, just=c("left", "center"), gp=gpar(cex=size))
		gList(grobLegendItem, grobLegendText)
	})
}

## design from top to bottom: .25 margin, 1.5 items, 1.25 text, .25 margin
legend_landsc <- function(x, legend.text.size, lineHeight, m) {
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
		
		
		labelsws <- convertWidth(stringWidth(legend.labels), "npc", TRUE) * legend.text.size * 1.3 #1.2
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
				  y=my+lineHeight*legend.text.size, just=c("center", "top"), gp=gpar(cex=legend.text.size))
		gList(grobLegendItem, grobLegendText)
	})
}

