legend_plot <- function(gt, x, legend_pos) {
	# are there any legend elements? if not, title.only=TRUE
	title.only <- all(sapply(x, is.null))

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
		gt$title.position <- if (title.only) c("left", "top") else gt$legend.position
		titleg <- !title.only
	} else {
		titleg <- (is.character(gt$title.position) && all(gt$title.position==gt$legend.position) && !title.only)	
	}
	
	##
	snap <- titleg && gt$title.snap.to.legend && gt$title!=""

	# constant needed for margins etc
	lineHeight <- convertHeight(unit(1, "lines"), "npc", valueOnly=TRUE)
	my <- lineHeight / 2
	mx <- convertWidth(convertHeight(unit(my, "npc"), "inch"), "npc", TRUE)
	
	# title properties
	nlines <- length(strsplit(gt$title, "\n")[[1]])
	titleWidth <- convertWidth(stringWidth(gt$title), "npc", valueOnly=TRUE) * gt$title.size
	titleHeight <- lineHeight * (nlines+.5) * gt$title.size
	
	if (!title.only) {

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
								 	   top= 1-my-legendHeight - ifelse(titleg && !snap && gt$title!="", titleHeight, 0), 
								 	   center=(1-legendHeight)/2, 
								 	   centre=(1-legendHeight)/2, 
								 	   bottom=my))		
		} else legend.position <- gt$legend.position
	} else {
		heights <- 0
	}
	
	if (is.character(gt$title.position) || snap) {
		title.position <- if (snap) NULL else {
			c(switch(gt$title.position[1], 
					 left=mx,
					 center=(1-titleWidth)/2,
					 centre=(1-titleWidth)/2,
					 right=1-mx-titleWidth),
			  switch(gt$title.position[2],
			  	     top=1-titleHeight*.75,
			  	     center=.5,
			  	     centre=.5,
			  	     bottom=titleHeight*.25 + ifelse(titleg && !snap && gt$title!="", legendHeight, 0)))	
		}
	} else title.position <- gt$title.position

	
	grobTitle <- if (snap || gt$title=="") {
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
	vpLeg <- viewport(layout=grid.layout(k, 2, heights=heights, widths=c(histWidth, 1-histWidth)), name="legend_grid")

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
	legend.type <- x$legend.type
	cols <- if (legend.type=="hist") 1 else c(1,2)
	cellplot(id, cols, e={
	    lineHeight <- convertHeight(unit(1, "lines"), unitTo="npc", valueOnly=TRUE)
		legGrob <- if (legend.type=="hist") {
			legend_hist(x, gt$legend.hist.size, lineHeight, scale=gt$scale, m=.25)
		} else if (legend.type=="TITLE") {
			legend_title(x, gt$title.size, lineHeight, m=.1)
		} else if (legend.type=="title") {
			legend_title(x, gt$legend.title.size, lineHeight, m=.1)
		} else if (legend.type=="spacer") {
			NULL
		} else if (x$legend.is.portrait) {
			legend_portr(x, gt$legend.text.size, lineHeight, m=.25)
		} else {
			legend_landsc(x, gt$legend.text.size, lineHeight, m=.25)
		}
		if (gt$design.mode) {
			gTree(children=gList(rectGrob(gp=gpar(fill="#CCCCCCCC")), legGrob))	
		} else legGrob
	})
}

legend_title <- function(x, size, lineHeight, m) {
	title <- x$title
	my <- lineHeight * size * m
	mx <- convertWidth(convertHeight(unit(my, "npc"), "inch"), "npc", TRUE)
	
	w <- convertWidth(stringWidth(paste(title, " ")), unitTo = "npc", valueOnly = TRUE)# * size * 1.05
	newsize <- min(size, 5/(lineHeight*6), (1-2*mx)/w)
	
	
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
		
		wstext <- convertWidth(stringWidth(legend.labels), unitTo = "npc", valueOnly = TRUE)
		newsize <- pmin(size, (1-wsmax-4*mx) / wstext)
		
		grobLegendItem <- if (legend.type %in% c("fill", "raster")) {
			fill <- legend.palette
			col <- ifelse(legend.type =="fill", get_alpha_col(border.col, border.alpha), NA)
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
								   y=ys, just=c("left", "center"), gp=gpar(cex=newsize))
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
			col <- get_alpha_col(border.col, border.alpha)
			rectGrob(x=xs, 
					  y=1-my-hs/2, 
					  width= ws, 
					  height= hs,
					  gp=gpar(fill=fill, col=col, lwd=lwd))
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
				  y=my+lineHeight*legend.text.size, just=c("center", "top"), gp=gpar(cex=legend.text.size))
		gList(grobLegendItem, grobLegendText)
	})
}

