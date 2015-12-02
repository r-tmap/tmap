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
		titleg <- (is.character(gt$title.position) && all(tolower(gt$title.position)==tolower(gt$legend.position)) && has.legend)	
	}
	snap <- titleg && gt$title.snap.to.legend && gt$title!=""

	# constant needed for margins etc
	lineHeight <- convertHeight(unit(1, "lines"), "npc", valueOnly=TRUE)
	lineWidth <- convertWidth(unit(1, "lines"), "npc", valueOnly=TRUE)
	my <- lineHeight / 2
	mx <- convertWidth(convertHeight(unit(my, "npc"), "inch"), "npc", TRUE)
	
	# title properties
	nlines <- length(strsplit(gt$title, "\n")[[1]])
	
	title.width <- convertWidth(stringWidth(gt$title), "npc", valueOnly=TRUE) * 1.02
	
	title.size <- min((1-2*mx) / title.width, gt$title.size)
	
	titleWidth <- title.width * title.size
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
			} else if (port && type %in% c("fill", "bubble.col", "line.col", "line.lwd", "raster", "text.col")) {
				length(p$legend.labels) * lineHeight * gt$legend.text.size + 2*margin*lineHeight
			} else if (port && type == "bubble.size") {
				sum(pmax(convertHeight(unit(p$legend.sizes, "inch"), "npc", valueOnly=TRUE) * 2 * 1.25, lineHeight * gt$legend.text.size)) + 2*margin*lineHeight
			} else if (!port && type == "bubble.size") {
				max(convertHeight(unit(p$legend.sizes, "inch"), "npc", valueOnly=TRUE) * 2, 1.5*lineHeight*gt$legend.text.size) + 2*margin*lineHeight*gt$legend.text.size + 1.25*lineHeight*gt$legend.text.size
			} else if (port && type == "text.size") {
				sum(pmax(convertHeight(unit(p$legend.sizes, "inch"), "npc", valueOnly=TRUE) * 2 * 1.25, lineHeight * gt$legend.text.size)) + 2*margin*lineHeight
			} else if (!port && type == "text.size") {
				max(convertHeight(unit(p$legend.sizes, "inch"), "npc", valueOnly=TRUE) * 2, 1.5*lineHeight*gt$legend.text.size) + 2*margin*lineHeight*gt$legend.text.size + 1.25*lineHeight*gt$legend.text.size
			} else if (!port && type %in% c("fill", "bubble.col", "line.col", "line.lwd", "raster")) {
				2*margin*lineHeight*gt$legend.text.size + 2.75 * lineHeight*gt$legend.text.size
			} else if (type == "spacer") {
				legend.title.npc * .25
			} else if (type == "title") {
				nlines <- length(strsplit(p$title, "\n")[[1]])
				legend.title.npc * nlines
			} else if (type == "hist") {
				gt$legend.hist.height
			}
		})
		
		legendWidth <- gt$legend.width
		histWidth <- min(gt$legend.hist.width / legendWidth, 1)
		
		# normalize legendHeight
		if (is.character(gt$title.position) && tolower(gt$title.position[1])==tolower(gt$legend.position[1]) && !snap) {
			legendHeight <- min(sum(heights), 1-metaY-titleHeight, gt$legend.height)
		} else {
			legendHeight <- min(sum(heights), 1-metaY, gt$legend.height)
		}
		if (is.character(gt$legend.position)) {
			legend.position <- c(switch(gt$legend.position[1], 
										left=mx+metaX, 
										center=(1-legendWidth)/2, 
										centre=(1-legendWidth)/2, 
										right=1-mx-legendWidth,
										LEFT=0,
										RIGHT=1-legendWidth,
										as.numeric(gt$legend.position[1])),
								 switch(gt$legend.position[2], 
								 	   top= 1 - my - ifelse(gt$title.position[2]=="top", my,0) - legendHeight - ifelse(titleg && !snap && gt$title!="", titleHeight, 0), 
								 	   center=(1-legendHeight)/2, 
								 	   centre=(1-legendHeight)/2, 
								 	   bottom=my+metaY,
								 	   TOP=1 - ifelse(gt$title.position[2]=="top", my,0) - legendHeight - ifelse(titleg && !snap && gt$title!="", titleHeight, 0),
								 	   BOTTOM=0,
								 	   as.numeric(gt$legend.position[2])))		
		} else legend.position <- gt$legend.position
		if (any(is.na(legend.position))) stop("Wrong position argument for legend")
		
	}
	
	if (is.character(gt$title.position) || snap) {
		title.position <- if (snap) NULL else {
			c(switch(gt$title.position[1], 
					 left=mx+metaX,
					 center=(1-titleWidth)/2,
					 centre=(1-titleWidth)/2,
					 right=1-mx-titleWidth,
					 LEFT=0,
					 RIGHT=1-titleWidth,
					 as.numeric(gt$title.position[1])),
			  switch(gt$title.position[2],
			  	     top=1-titleHeight*.5-my,
			  	     center=.5,
			  	     centre=.5,
			  	     bottom=my+metaY+titleHeight*.5 + ifelse(titleg && !snap && gt$title!="", legendHeight + ifelse(gt$legend.position[2]=="bottom", my,0), 0),
			  	     TOP=1-titleHeight*.5,
			  	     BOTTOM=titleHeight*.5 + ifelse(titleg && !snap && gt$title!="", legendHeight + ifelse(gt$legend.position[2]=="bottom", my,0), 0),
			  	     as.numeric(gt$title.position[2])))	
		}
	} else title.position <- gt$title.position
	if (any(is.na(title.position))) stop("Wrong position argument for title")
	
	grobTitle <- if (snap || gt$title=="") {
		NULL
	} else {
		gTree(children=gList(if (gt$design.mode) {
			rectGrob(x = title.position[1]-.5*mx, y = title.position[2], width=titleWidth+mx, just=c("left", "center"), height=titleHeight, gp=gpar(col=gt$title.color, fill="#888888BB"))
		} else if (!is.na(gt$title.bg.color)) {
			rectGrob(x = title.position[1]-.5*mx, y = title.position[2], width=titleWidth+mx, just=c("left", "center"), height=titleHeight, gp=gpar(col=NA, fill=gt$title.bg.col))
		} else {
			NULL
		}, textGrob(label=gt$title, x = title.position[1], y = title.position[2], just=c("left", "center"), gp=gpar(col=gt$title.color, cex=title.size, fontface=gt$fontface, fontfamily=gt$fontfamily))))
		
	}

	
	if (gt$credits.show || gt$scale.show || gt$compass.show) {
		elems <- do.call("rbind", list(
			if (gt$credits.show) data.frame(type="credits",
				 height=unname(mapply(function(txt, sz) {
				 	lineHeight * (length(strsplit(txt, "\n")[[1]])+1) * 
				 	min((1-2*convertWidth(convertHeight(unit(lineHeight / 2, "npc"), "inch"), "npc", TRUE)) / 
				 			convertWidth(stringWidth(txt), "npc", valueOnly=TRUE), sz)
				 	}, gt$credits.text, gt$credits.size)),
				 width=1,
				 position1=sapply(gt$credits.position, "[", 1, USE.NAMES=FALSE),
				 position2=sapply(gt$credits.position, "[", 2, USE.NAMES=FALSE), 
				 sortid=gt$credits.id,
				 stringsAsFactors = FALSE) else NULL,
			if (gt$scale.show) data.frame(type="scale_bar",
				height=3*lineHeight * gt$scale.size,
				width=1,
				position1=gt$scale.position[1],
				position2=gt$scale.position[2], 
				sortid=gt$scale.id,
				stringsAsFactors = FALSE) else NULL,
			if (gt$compass.show) data.frame(type="compass",
				height=(gt$compass.nlines * gt$compass.fontsize)*lineHeight,
				width=(gt$compass.nlines * gt$compass.fontsize)*lineWidth,
				position1=gt$compass.position[1],
				position2=gt$compass.position[2], 
				sortid=gt$compass.id,
				stringsAsFactors = FALSE) else NULL))

		
		elems$cred.id <- NA
		elems$cred.id[elems$type=="credits"] <- order(order(elems$sortid[elems$type=="credits"]))
		
		elems$position1[is.na(elems$position1)] <- gt$attr.position[1]
		elems$position2[is.na(elems$position2)] <- gt$attr.position[2]
		
		elems$isChar1 <- (elems$position1 %in% c("left", "center", "centre", "right", "LEFT", "RIGHT"))
		elems$isChar2 <- (elems$position2 %in% c("top", "center", "centre", "bottom", "TOP", "BOTTOM"))
		elems$id <- paste(elems$position1, elems$position2, (!elems$isChar1 | !elems$isChar1) * (1:nrow(elems))) # create id for elements that are snapped to each other
		
		elemsOrder <- order(elems$sortid, decreasing=TRUE)
		elemsList <- split(elems[elemsOrder,], f = elems$id[elemsOrder])
		
		elemGrobs <- lapply(elemsList, function(el) {
			elemHeight <- sum(el$height)
			elpos <- c(el$position1[1], el$position2[1])
			elemleg <- all(tolower(elpos)==tolower(gt$legend.position)) && has.legend
			elemtitle <- all(tolower(elpos)==tolower(gt$title.position)) && gt$title!="" && !snap
			if (elemleg) {
				elem.position <- c(switch(elpos[1], 
										  left=metaX+mx+legendWidth+ifelse(gt$legend.position[1]=="left", mx, 0),
										  center=.5 + mx + legendWidth/2,
										  centre=.5 + mx + legendWidth/2,
										  right=1-mx-legendWidth - ifelse(gt$legend.position[1]=="right", mx, 0),
										  LEFT=legendWidth+ifelse(gt$legend.position[1]=="left", mx, 0),
										  RIGHT=1-legendWidth-ifelse(gt$legend.position[1]=="right", mx, 0),
								   		  as.numeric(elpos[1])),
								   switch(elpos[2],
								   	   top= 1-my-elemHeight, 
								   	   center=.5, 
								   	   centre=.5, 
								   	   bottom=my+metaY,
								   	   TOP=1-elemHeight,
								   	   BOTTOM=0,
								   	   as.numeric(elpos[2])))	
				if (any(is.na(elem.position))) stop("Wrong position argument for attributes")
				elem.max.width <- 1 - mx - legendWidth - metaX - ifelse(gt$legend.position[1] %in% c("left", "right"), mx, 0) - ifelse(elpos[1] %in% c("left", "right"), mx, 0)
			} else if (elemtitle) {
				elem.position <- c(switch(elpos[1], 
										  left=mx+metaX,
										  center=.5,
										  centre=.5,
										  right=1-mx,
										  LEFT=0,
										  RIGHT=1,
										  as.numeric(elpos[1])),
								   switch(elpos[2],
								   	   top= 1-ifelse(gt$title.position[2]=="top", my, 0) - elemHeight - titleHeight, 
								   	   center=.5, 
								   	   centre=.5, 
								   	   bottom=ifelse(gt$title.position[2]=="bottom", my, 0)+metaY+titleHeight,
								   	   TOP=1-ifelse(gt$title.position[2]=="top", my, 0) - elemHeight - titleHeight,
								   	   BOTTOM=ifelse(gt$title.position[2]=="bottom", my, 0)+titleHeight,
								   	   as.numeric(elpos[2])))	
				if (any(is.na(elem.position))) stop("Wrong position argument for attributes")
				elem.max.width <- 1 - (if (has.legend && tolower(elpos[2])==tolower(gt$legend.position[2])) 2*mx + legendWidth else mx) - ifelse(elpos[1] %in% c("left", "right"), mx, 0) - metaX
			} else {
				elem.position <- c(switch(elpos[1], 
										  left=mx+metaX,
										  center=.5,
										  centre=.5,
										  right=1-mx,
										  LEFT=0,
										  RIGHT=1,
										  as.numeric(elpos[1])),
								   switch(elpos[2],
								   	   top= 1-my-elemHeight, 
								   	   center=.5, 
								   	   centre=.5, 
								   	   bottom=my+metaY,
								   	   TOP=1-elemHeight,
								   	   BOTTOM=0,
								   	   as.numeric(elpos[2])))
				if (any(is.na(elem.position))) stop("Wrong position argument for attributes")
				elem.max.width <- (if (has.legend && tolower(elpos[2])==tolower(gt$legend.position[2])) 1 - 3*mx - legendWidth else 1 - 2*mx) - metaX
			}
			elem.just <- switch(elpos[1],
								right="right",
								RIGHT="right",
								"left")
			el$y <- elem.position[2] + c(0, cumsum(el$height))[1:nrow(el)]
			el$width2 <- pmin(el$width, elem.max.width)
			
			el$x <- elem.position[1] - if (elem.just=="left") 0 else el$width2
			
			lapply(1:nrow(el), function(i) {
				e <- el[i,]
				vpi <- viewport(x=e$x, 
								y=e$y,
								height=e$height, width=e$width2,
								just=c("left", "bottom"),
								name=as.character(e$type))
				pushViewport(vpi)
				if (e$type=="credits") {
					grb <- plot_cred(gt, just=elem.just, id=e$cred.id)
				} else if (e$type=="scale_bar") {
					grb <- plot_scale(gt, just=elem.just, xrange=(bb[1,2] - bb[1,1])*e$width2, crop_factor=gt$scale.width/e$width2)
				} else {
					grb <- plot_compass(gt, just=elem.just)
				}
				grt <- gTree(children=gList(grb), vp=vpi)
				upViewport()
				grt
			})
		})
		elemGrobs <- do.call("c", elemGrobs)
		
		
		treeElem <- gTree(children=do.call("gList", elemGrobs))
	} else {
		treeElem <- NULL
	}
	
	
	
	treeLegend <- if (has.legend) {
	
		vpLegend <- viewport(y=legend.position[2], x=legend.position[1], 
							 height=legendHeight, width=legendWidth, 
							 just=c("left", "bottom"), name="legend")
		
		pushViewport(vpLegend)
		
		legend.frame <- !is.na(gt$legend.frame)
		legend.bg.color <- gt$legend.bg.color
		
		legend.frame.color <- if (legend.frame) {
			if (is.logical(gt$legend.frame) || gt$design.mode) "black" else gt$legend.frame
		} else NA
		
		legend.frame.fill <- if (gt$design.mode) "#888888BB" else legend.bg.color
		
		
		# normalise heights
		heights <- heights / legendHeight
		vpLeg <- viewport(layout=grid.layout(k, 2, heights=heights, widths=c(histWidth, 1-histWidth)), name="legend_grid")
	
		
		if (gt$legend.inside.box) {
			vpLegFrame <- viewport(width=1-mx, height=1-my, name="legend_frame")
			vpLeg <- vpStack(vpLegFrame, vpLeg)
		} 
		
		pushViewport(vpLeg)
		grobListRes <- mapply("legend_subplot", x, id=1:k, MoreArgs = list(gt=gt, histWidth=histWidth), SIMPLIFY = FALSE)
	
		grobList <- lapply(grobListRes, "[[", 1)
		legWidth <- max(sapply(grobListRes, "[[", 2))

		if (gt$legend.inside.box) legWidth <- legWidth / (1-mx)
		
		grobLegBG <- rectGrob(x=0, width=legWidth, just=c("left", "center"), gp=gpar(lwd=gt$scale, col=gt$legend.frame, fill=legend.frame.fill))
		
		upViewport(2)
		gTree(children=gList(grobLegBG, gTree(children=do.call("gList", grobList), vp=vpLeg)), vp=vpLegend, name="legend")
	} else {
		NULL
	}
	
	treeMeta <- gTree(children=gList(grobTitle, treeElem, treeLegend), name="meta")
	
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

legend_title <- function(x, gt, is.main.title, lineHeight, m) {
	size <- ifelse(is.main.title, gt$title.size, gt$legend.title.size)
	title <- x$title
	nlines <- length(strsplit(x$title, "\n")[[1]])
	my <- lineHeight * size * m
	mx <- convertWidth(convertHeight(unit(my, "npc"), "inch"), "npc", TRUE)
	
	w <- convertWidth(stringWidth(paste(title, " ")), unitTo = "npc", valueOnly = TRUE)# * size * 1.05
	newsize <- min(size, 5/(lineHeight*nlines*6), (1-2*mx)/w)
	
	
	list(textGrob(title, x=mx, y=6/12 , just=c("left", "center"), gp=gpar(col=gt$legend.text.color, cex=newsize, fontface=gt$fontface, fontfamily=gt$fontfamily)), legWidth=2*mx+w*newsize)
}


legend_portr <- function(x, gt, lineHeight, m) {
	legend.text.size <- gt$legend.text.size
	with(x, {
		is.cont <- (nchar(legend.palette[1])>20)
		
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
		} else if (legend.type=="text.size") {
			nitems <- length(legend.labels)
			hs <- convertHeight(unit(legend.sizes, "lines"), "npc", valueOnly=TRUE) * 2
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
		
		if (legend.type=="bubble.col" && !is.cont) {
			bmax <- convertHeight(unit(bubble.max.size, "inch"), "npc", valueOnly=TRUE) * 2
			hs <- pmin(hs/s, bmax)
		} else if (legend.type=="text.col" && !is.cont) {
			bmax <- convertHeight(unit(text.max.size, "lines"), "npc", valueOnly=TRUE) * 2
			hs <- pmin(hs/s, bmax)
		}

		ys <- 1 - my - cumsum(lhs) + lhs/2
		size <- pmin(lhs / lineHeight, legend.text.size)
		
		ws <- convertWidth(convertHeight(unit(hs, "npc"), "inch"), "npc", TRUE)
		wsmax <- max(ws)
		hsi <- convertHeight(unit(hs, "npc"), "inch", valueOnly=TRUE)
		
		wstext <- convertWidth(stringWidth(paste(legend.labels, " ")), unitTo = "npc", valueOnly = TRUE)
		newsize <- pmin(size, (1-wsmax-4*mx) / wstext)
		

		grobLegendItem <- if (is.cont) {
			fill <- legend.palette
			xs <- mx+ws/2

			# process fill colors
			fill_list <- strsplit(fill, split = "-", fixed=TRUE)
			fill_list <- lapply(fill_list, function(i) {
				i[i=="NA"] <- NA
				i
			})
			fill_len <- sapply(fill_list, length)
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
		} else if (legend.type %in% c("bubble.size", "bubble.col")) {
			cols <- legend.palette
			circleGrob(x=mx+wsmax/2, 
					y=ys, r=unit(hsi/2, "inch"),
					gp=gpar(fill=cols,
							col=bubble.border.col,
							lwd=bubble.border.lwd))
		} else if (legend.type %in% c("text.size", "text.col")) {
			cols <- legend.palette
			textGrob(legend.labels,
					 x=mx+wsmax/2, 
					 y=ys,
					  gp=gpar(col=cols))
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
								   y=ys, just=c("left", "center"), gp=gpar(col=gt$legend.text.color, cex=newsize, fontface=gt$fontface, fontfamily=gt$fontfamily))
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
		} else if (legend.type=="text.size") {
			hs <- convertHeight(unit(legend.sizes, "lines"), "npc", valueOnly=TRUE)
		} else {
			hs <- rep(1.5*lineHeight*legend.text.size, nitems)
		}
		
		
		labelsws <- convertWidth(stringWidth(paste(legend.labels, " ")), "npc", TRUE)
		maxlabelsws <- max(labelsws) * legend.text.size
		
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
		} else if (legend.type=="text.size") {
			textws <- convertWidth(unit(legend.sizes, "lines"), "npc", valueOnly=TRUE)
			ws <- pmax(ws, textws*1.1)
			
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
		} else if (legend.type=="text.col") {
			bmax <- convertHeight(unit(text.max.size, "lines"), "npc", valueOnly=TRUE)
			hs <- pmin(hs, bmax)
		}
		
		hsmax <- max(hs)
		hsi <- convertHeight(unit(hs, "npc"), "inch", valueOnly=TRUE)
		
		
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
			fill_len <- sapply(fill_list, length)
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
			rectGrob(x=xs, 
					  y=1-my-hs/2, 
					  width= ws, 
					  height= hs,
					  gp=gpar(fill=fill, col=border.col, lwd=lwd))
		} else if (legend.type %in% c("bubble.size", "bubble.col")) {
			cols <- legend.palette
			bubbleR <- unit(hsi/2, "inch")
			xtraWidth <- convertWidth(max(bubbleR), "npc", valueOnly=TRUE)
			circleGrob(x=xs, y=1-my-hsmax/2, r=bubbleR,
						gp=gpar(fill=cols,
								col=bubble.border.col,
								lwd=bubble.border.lwd))
		} else if (legend.type %in% c("text.size", "text.col")) {
			cols <- legend.palette
			xtraWidth <- convertWidth(unit(hsi, "inch"), "npc", valueOnly=TRUE)
			textGrob(legend.labels,
					 x=xs, y=1-my-hsmax/2,
					   gp=gpar(col=cols))
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
		grobLegendText <- textGrob(legend.labels, x=xs,
				  y=my+lineHeight*legend.text.size, just=c("center", "top"), gp=gpar(col=gt$legend.text.color, cex=legend.text.size, fontface=gt$fontface, fontfamily=gt$fontfamily))
		
		legWidth <- mx*2+xs[length(xs)]+max(xtraWidth, labelsws[nitems]*legend.text.size/2)

		list(gList(grobLegendItem, grobLegendText), legWidth=legWidth)
	})
}


plot_scale <- function(gt, just, xrange, crop_factor) {
	light <- do.call("process_color", c(list(gt$scale.color.light, alpha=1), gt$pc))
	dark <- do.call("process_color", c(list(gt$scale.color.dark, alpha=1), gt$pc))

	xrange2 <- xrange/gt$unit.size
	
	if (is.null(gt$scale.breaks)) {
		ticks2 <- pretty(c(0, xrange2*crop_factor), 4)
	} else {
		ticks2 <- gt$scale.breaks
	}
	ticksWidths <- convertWidth(stringWidth(paste(ticks2, " ")), "npc", TRUE)

	labels <- c(ticks2, gt$unit)
	
	n <- length(ticks2)
	ticks <- ticks2*gt$unit.size
	ticks3 <- ticks / xrange
	
	widths <- ticks3[2] - ticks3[1]
	size <- min(gt$scale.size, widths/max(ticksWidths))
	x <- ticks3[1:(n-1)] + .5*ticksWidths[1]*size
	
# 	cat(size, "s\n")
# 	cat(gt$scale.size, "gts\n")
	
	lineHeight <- convertHeight(unit(1, "lines"), "npc", valueOnly=TRUE) * size
	#my <- lineHeight / 2
	#mx <- convertWidth(convertHeight(unit(my, "npc"), "inch"), "npc", TRUE)
	
	unitWidth <- convertWidth(stringWidth(paste(gt$unit, " ")), "npc", TRUE) * size
	width <- widths * (n-1) + .5*ticksWidths[1]*size + .5*ticksWidths[n]*size+ unitWidth   #widths * n 
	
	xtext <- x[1] + c(ticks3, ticks3[n] + .5*ticksWidths[n]*size + .5*unitWidth)# + widths*.5 + unitWidth*.5) #+ position[1]

	if (just=="right") {
		x <- 1-width+x
		xtext <- 1-width+xtext
	}
	
	grobBG <- if (gt$design.mode) rectGrob(gp=gpar(fill="orange")) else NULL
	
	gTree(children=gList(
		grobBG,
		rectGrob(x=x, y=1.5*lineHeight, width = widths, height=lineHeight*.5, just=c("left", "bottom"), gp=gpar(col=dark, fill=c(light, dark))),
		textGrob(label=labels, x = xtext, y = lineHeight, just=c("center", "center"), gp=gpar(col=gt$attr.color, cex=size, fontface=gt$fontface, fontfamily=gt$fontfamily))), name="scale_bar")
	
	
}


plot_cred <- function(gt, just, id) {
	lineHeight <- convertHeight(unit(1, "lines"), "npc", valueOnly=TRUE)

	my <- lineHeight / 2
	mx <- convertWidth(convertHeight(unit(my, "npc"), "inch"), "npc", TRUE)
	
	# number of lines
	nlines <- length(strsplit(gt$credits.text[id], "\n")[[1]])
	
	size <- min((1-2*mx) / convertWidth(stringWidth(gt$credits.text[id]), "npc", valueOnly=TRUE), gt$credits.size[id])
	
	width <- (convertWidth(stringWidth(gt$credits.text[id]), "npc", valueOnly=TRUE)+0*mx) * size
	height <- lineHeight * (nlines) * size

	x <- if (just=="left") mx*size else 1-width-mx*size

	if (gt$credits.align[id]=="center") {
		x <- x + width/2
	} else if (gt$credits.align[id]=="right") {
		x <- x + width
	}
	
	grobBG <- if (gt$design.mode) rectGrob(gp=gpar(fill="orange")) else NULL
	
	col <- do.call("process_color", c(list(gt$credits.col[id], alpha=gt$credits.alpha[id]), gt$pc))
	
	gTree(children=gList(grobBG,
						 if (!is.na(gt$credits.bg.color[id])) {
		bg.col <- do.call("process_color", c(list(gt$credits.bg.color[id], alpha=gt$credits.bg.alpha[id]), gt$pc))
		rectGrob(x=x, width=width, just="left", gp=gpar(col=NA, fill=bg.col))
	} else {
		NULL
	}, textGrob(label=gt$credits.text[id], x = x, y =.5, just=c(gt$credits.align[id], "center"), gp=gpar(cex=size, col=col, fontface=gt$credits.fontface[id], fontfamily=gt$credits.fontfamily[id]))), name="credits")
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
		LWD <- round(convertWidth(unit(.01, "npc"), "points", valueOnly=TRUE))
		
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
		LWD <- convertWidth(unit(.01, "npc"), "points", valueOnly=TRUE)
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
		polygonGrob(x=x[[1]], y=y[[1]], id=id, gp=gpar(fill=fill))
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
	