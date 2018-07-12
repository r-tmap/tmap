meta_plot <- function(gt, x, legend_pos, bb, metaX, metaY, frameX, frameY, use_facets) {
	# are there any legend elements? if not, title.only=TRUE
	#title.only <- all(sapply(x, is.null))
	has.legend <- !is.null(x)
	
	# legend positioning
	if (is.null(gt$legend.position)) {
		if ((use_facets && gt$free.coords) || gt$legend.only) {
			gt$legend.position <- c("left", "top")
		} else {
			gt$legend.position <- c(ifelse(legend_pos<3, "left", "right"), ifelse(legend_pos %in% c(1,4), "bottom", "top"))
		}
	}

	# legend justification
	gt$legend.just <- c(
		if (is.null(gt$legend.just) || !is_num_string(gt$legend.position[1])) {
			0
		} else {
			as.numeric(ifelse(is_num_string(gt$legend.just[1]), gt$legend.just[1], 
					   ifelse(gt$legend.just[1]=="right", 1, ifelse(gt$legend.just[1]=="left", 0, .5))))
		},
		if (is.null(gt$legend.just) || !is_num_string(gt$legend.position[2])) {
			0
		} else {
			as.numeric(ifelse(is_num_string(gt$legend.just[2]), gt$legend.just[2], 
					  ifelse(gt$legend.just[2]=="top", 1, ifelse(gt$legend.just[2]=="bottom", 0, .5))))
		}
	)

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
	nlines <- number_text_lines(gt$title)
	
	title.width <- text_width_npc(gt$title, space=FALSE) * 1.02
 	
	title.size <- min((1-2*mx) / title.width, gt$title.size)
	
	titleWidth <- title.width * title.size
	titleHeight <- lineHeight * (nlines*1.2) * title.size
	
	stackV <- gt$legend.stack=="vertical"
	########################################################################################
	## legend
	########################################################################################
	if (has.legend) {
		nx <- length(x)
		
		zs <- vapply(x, function(y) y$legend.z, numeric(1))
		x <- x[order(zs)]
		
		if (stackV) {
			x <- lapply(x, function(y) {
				name <- y$legend.type
				list_spacer <- list(legend.type="spacer", legend.is.portrait=FALSE)
				list_title <- if(!nonempty_text(y$legend.title)) NULL else list(legend.type="title", title=y$legend.title, legend.is.portrait=FALSE)
				list(list_spacer, list_title, y)
			})
			x <- do.call("c", x)[-1]
		} else {
			x <- lapply(x, function(y) {
				name <- y$legend.type
				list_title <- if(!nonempty_text(y$legend.title)) NULL else list(legend.type="title", title=y$legend.title, legend.is.portrait=FALSE)
				list(list_title, y)
			})
			x <- do.call("c", x)
		}
		
		
		legend.title.npc <- convertHeight(unit(1, "lines"), "npc", valueOnly=TRUE) * gt$legend.title.size

		
		# add element for main title
		if (snap && gt$title!="") {
			if (stackV) {
				x <- c(TITLE=list(list(legend.type="TITLE", title=gt$title, legend.is.portrait=FALSE)), x)
			} else {
				xtitle <- list(legend.type="TITLE", title=gt$title, legend.is.portrait=FALSE)
			}
		} else {
			xtitle <- NULL
		}

		# remove empty legend elements
		x.not.null <- !vapply(x, is.null, logical(1))
		if (stackV) {
			k <- sum(x.not.null)
			x <- x[x.not.null]
		} else k <- length(x)

		# shrink heights (remove white space)
		margin <- 0.25 * gt$legend.text.size
		
		s2 <- 4/3
		
		heights <- vapply(x, function(p){
			if (is.null(p)) return(0)
			type <- p$legend.type
			port <- p$legend.is.portrait
			if (type=="TITLE") {
				titleHeight
			} else if (port && type %in% c("fill", "line.col", "line.lwd", "raster", "text.col")) {
				length(p$legend.labels) * lineHeight * gt$legend.text.size + 2*margin*lineHeight
			} else if (port && type %in% c("symbol.col", "symbol.shape")) {
				legend.sizes <- rep(if (length(p$legend.sizes)==1) rep(0, length.out=length(p$legend.labels)) else p$legend.sizes) 
				sum(pmax(convertHeight(unit(legend.sizes, "inch"), "npc", valueOnly=TRUE) /s2 * 1.35, lineHeight * gt$legend.text.size)) + 2*margin*lineHeight
			} else if (port && type == "symbol.size") {
				sum(pmax(convertHeight(unit(p$legend.sizes, "inch"), "npc", valueOnly=TRUE) /s2 * 1.35, lineHeight * gt$legend.text.size)) + 2*margin*lineHeight
			} else if (!port && type == "symbol.size") {
				max(convertHeight(unit(p$legend.sizes, "inch"), "npc", valueOnly=TRUE)/s2, 1.5*lineHeight*gt$legend.text.size) + 2*margin*lineHeight*gt$legend.text.size + 1.25*lineHeight*gt$legend.text.size
			} else if (port && type == "text.size") {
				sum(pmax(convertHeight(unit(p$legend.sizes, "lines"), "npc", valueOnly=TRUE) * 1.25, lineHeight * gt$legend.text.size)) + 2*margin*lineHeight
			} else if (!port && type %in% c("text.size", "text.col")) {
				max(convertHeight(unit(p$legend.sizes, "lines"), "npc", valueOnly=TRUE), 1.5*lineHeight*gt$legend.text.size) + 2*margin*lineHeight*gt$legend.text.size + 1.25*lineHeight*gt$legend.text.size
			} else if (!port && type %in% c("fill", "symbol.col", "symbol.shape", "line.col", "line.lwd", "raster")) {
				2*margin*lineHeight*gt$legend.text.size + 2.75 * lineHeight*gt$legend.text.size
			} else if (type == "spacer") {
				legend.title.npc * .25
			} else if (type == "title") {
				nlines <- number_text_lines(p$title)
				legend.title.npc * nlines
			} else if (type == "hist") {
				gt$legend.hist.height
			}
		}, numeric(1))
		
		legendWidth <- gt$legend.width
		
		autoWidth <- (legendWidth > 0)
		if (!autoWidth) legendWidth <- -legendWidth
		
		histWidth <- min(gt$legend.hist.width / legendWidth, 1)
		
		legendHeight <- gt$legend.height
		
		autoHeight <- (legendHeight > 0)
		if (!autoHeight) legendHeight <- -legendHeight
		
		
		# normalize legendHeight
		if (stackV) {
			if (autoHeight) {
				if (is.character(gt$title.position) && tolower(gt$title.position[1])==tolower(gt$legend.position[1]) && !snap) {
					legendHeight <- min(sum(heights), 1-metaY-titleHeight, legendHeight)
				} else {
					legendHeight <- min(sum(heights), 1-metaY, legendHeight)
				}
			}
			
			# normalise heights
			heights <- heights / legendHeight

		} else {
			legItemTitleHeight <- max(heights[seq(1,k, by=2)])
			legItemHeight <- max(heights[seq(2,k, by=2)])
			legTitleHeight <- ifelse(!is.null(xtitle), titleHeight, 0)
			legTotHeight <- legItemTitleHeight + legItemHeight + legTitleHeight
			
			if (autoHeight) {
				if (is.character(gt$title.position) && tolower(gt$title.position[1])==tolower(gt$legend.position[1]) && !snap) {
					legendHeight <- min(legTotHeight, 1-metaY-titleHeight, gt$legend.height)
				} else {
					legendHeight <- min(legTotHeight, 1-metaY, gt$legend.height)
				}
			}
			nh <- legendHeight/legTotHeight
			
			if (autoWidth) {
				legendWidth <- min(legendWidth, 1 - 2 * mx)
			}

			
			# calculate height ratios
			rel_heights <- heights / (nh*c(legItemTitleHeight, legItemHeight))
			
			# normalise heights
			heights[seq(1,k, by=2)] <- pmin(heights[seq(1,k, by=2)], nh*legItemTitleHeight)
			heights[seq(2,k, by=2)] <- pmin(heights[seq(2,k, by=2)], nh*legItemHeight)
			heightLegTitle <- nh*legTitleHeight
		}
		

		
		
		
		
		if (is.character(gt$legend.position)) {
			legend.position <- c(switch(gt$legend.position[1], 
										left=frameX+mx+metaX, 
										center=(1-legendWidth)/2, 
										centre=(1-legendWidth)/2, 
										right=1-mx-legendWidth-frameX,
										LEFT=frameX,
										RIGHT=1-legendWidth-frameX,
										as.numeric(gt$legend.position[1])),
								 switch(gt$legend.position[2], 
								 	   top= 1 - legendHeight - ifelse(titleg && !snap && gt$title!="", titleHeight+ifelse(gt$title.position[2]=="top", 1.5*my,.5*my), my) - frameY, 
								 	   center=(1-legendHeight)/2, 
								 	   centre=(1-legendHeight)/2, 
								 	   bottom=my+metaY + frameY,
								 	   TOP= 1 - legendHeight - ifelse(titleg && !snap && gt$title!="", titleHeight+ifelse(gt$title.position[2]=="top", my,0), 0) - frameY, 
								 	   BOTTOM=frameY,
								 	   as.numeric(gt$legend.position[2])))	
			legWidthCorr <- if(gt$legend.position[1] == c("right")) mx else 0 
		} else {
			legend.position <- gt$legend.position
			legWidthCorr <- 0
		}
		if (any(is.na(legend.position))) stop("Wrong position argument for legend", call. = FALSE)
		#stackV && 
		legSnapToRight <- ifelse(!is_num_string(gt$legend.position[1]),
								 ifelse(gt$legend.position[1]%in%c("center", "centre"),
								 	   .5, gt$legend.position[1]%in%c("right", "RIGHT")),
								 ifelse(is_num_string(gt$legend.just[1]),
								 	   as.numeric(gt$legend.just[1]),
								 	   gt$legend.just[1]=="right"))
		
		
	}
	

	#########################################################################################
	## title
	#########################################################################################
	
	if (is.character(gt$title.position) || snap) {
		title.position <- if (snap) NULL else {
			c(switch(gt$title.position[1], 
					 left=frameX+mx+metaX,
					 center=(1-titleWidth)/2,
					 centre=(1-titleWidth)/2,
					 right=1-mx-frameX-titleWidth,
					 LEFT=frameX+.5*mx,
					 RIGHT=1-titleWidth-frameX-.5*mx,
					 as.numeric(gt$title.position[1])),
			  switch(gt$title.position[2],
			  	     top=1-titleHeight*.5-my-frameY,
			  	     center=.5,
			  	     centre=.5,
			  	     bottom=frameY+metaY+titleHeight*.5 + ifelse(titleg && !snap && gt$title!="", legendHeight + ifelse(gt$legend.position[2]=="bottom", 1.5*my,.5*my), my),
			  	     TOP=1-titleHeight*.5-frameY,
			  	     BOTTOM=frameY+titleHeight*.5 + ifelse(titleg && !snap && gt$title!="", legendHeight + ifelse(gt$legend.position[2]=="bottom", my,0), 0),
			  	     as.numeric(gt$title.position[2])))	
		}
	} else title.position <- gt$title.position
	if (!snap && any(is.na(title.position))) stop("Wrong position argument for title", call. = FALSE)
	
	grobTitle <- if (snap || !nonempty_text(gt$title)) {
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

	#########################################################################################
	## attributes
	#########################################################################################
	if (gt$credits.show || gt$logo.show || gt$scale.show || gt$compass.show) {
		elems <- do.call("rbind", list(
			if (gt$credits.show) data.frame(type="credits",
				 height=unname(emapply(function(txt, sz) {
				 	lineHeight * (number_text_lines(txt)*1.2+.25) * 
				 	min((1-2*convertWidth(convertHeight(unit(lineHeight / 2, "npc"), "inch"), "npc", TRUE)) / 
				 			text_width_npc(txt, space = FALSE), sz)
				 	}, gt$credits.text, gt$credits.size)),
				 width=1,
				 position1=sapply(gt$credits.position, "[", 1, USE.NAMES=FALSE),
				 position2=sapply(gt$credits.position, "[", 2, USE.NAMES=FALSE), 
				 just1=sapply(gt$credits.just, "[", 1, USE.NAMES=FALSE),
				 just2=sapply(gt$credits.just, "[", 2, USE.NAMES=FALSE),
				 sortid=gt$credits.id,
				 stringsAsFactors = FALSE) else NULL,
			if (gt$logo.show) data.frame(type="logo",
				height=mapply(function(lh, m) {
					(max(unlist(lh)) + m*2) * lineHeight
				}, gt$logo.height, gt$logo.margin, SIMPLIFY = TRUE, USE.NAMES = FALSE),#  sapply(gt$logo.height, function(lh) (lh+1) * lineHeight),
				width=mapply(function(lw, m) {
					(sum(unlist(lw) + m) + m) * lineWidth
				}, gt$logo.width, gt$logo.margin, SIMPLIFY = TRUE, USE.NAMES = FALSE),#  sapply(gt$logo.height, function(lh) (lh+1) * lineHeight),
				#width=sapply(gt$logo.width, function(lw) (lw+1) * lineWidth),
				position1=sapply(gt$logo.position, "[", 1, USE.NAMES=FALSE),
				position2=sapply(gt$logo.position, "[", 2, USE.NAMES=FALSE),
				just1=sapply(gt$logo.just, "[", 1, USE.NAMES=FALSE),
				just2=sapply(gt$logo.just, "[", 2, USE.NAMES=FALSE),
				sortid=gt$logo.id,
				stringsAsFactors = FALSE) else NULL,
			if (gt$scale.show) data.frame(type="scale_bar",
				height=3*lineHeight * gt$scale.size,
				width=1,
				position1=gt$scale.position[1],
				position2=gt$scale.position[2], 
				just1=gt$scale.just[1],
				just2=gt$scale.just[2], 
				sortid=gt$scale.id,
				stringsAsFactors = FALSE) else NULL,
			if (gt$compass.show) data.frame(type="compass",
				height=(gt$compass.nlines * gt$compass.fontsize)*lineHeight,
				width=(gt$compass.nlines * gt$compass.fontsize)*lineWidth,
				position1=gt$compass.position[1],
				position2=gt$compass.position[2], 
				just1=gt$compass.just[1],
				just2=gt$compass.just[2], 
				sortid=gt$compass.id,
				stringsAsFactors = FALSE) else NULL))

		
		elems$cred.id <- NA
		elems$cred.id[elems$type=="credits"] <- order(order(elems$sortid[elems$type=="credits"]))
		
		elems$logo.id <- NA
		elems$logo.id[elems$type=="logo"] <- order(order(elems$sortid[elems$type=="logo"]))
		
		
		elems$position1[is.na(elems$position1)] <- gt$attr.position[1]
		elems$position2[is.na(elems$position2)] <- gt$attr.position[2]
		
		elems$just1[is.na(elems$just1)] <- gt$attr.just[1]
		elems$just2[is.na(elems$just2)] <- gt$attr.just[2]
		
		elems$isChar1 <- (elems$position1 %in% c("left", "center", "centre", "right", "LEFT", "RIGHT"))
		elems$isChar2 <- (elems$position2 %in% c("top", "center", "centre", "bottom", "TOP", "BOTTOM"))
		#elems$id <- paste(elems$position1, elems$position2, (!elems$isChar1 | !elems$isChar1) * (1:nrow(elems))) # create id for elements that are snapped to each other
		elems$id <- paste(elems$position1, elems$position2)
		
		
		elemsOrder <- order(elems$sortid, decreasing=TRUE)
		elemsList <- split(elems[elemsOrder,], f = elems$id[elemsOrder])
		
		elemGrobs <- lapply(elemsList, function(el) {
			elemHeight <- sum(el$height)
			elpos <- c(el$position1[1], el$position2[1])
			eljust <- c(el$just1[1], el$just2[1])
			elemleg <- all(tolower(elpos)==tolower(gt$legend.position)) && has.legend
			elemtitle <- all(tolower(elpos)==tolower(gt$title.position)) && gt$title!="" && !snap
			if (elemleg) {
				elemSnapToRight <- legSnapToRight
				xcor1 <- frameX+mx+legendWidth+mx
				XCOR1 <- frameX+legendWidth+mx
				elem.position <- c(switch(elpos[1], 
										  left=xcor1+metaX,
										  center=.5-legendWidth/2-mx ,
										  centre=.5-legendWidth/2-mx,
										  right=1-xcor1,
										  LEFT=XCOR1,
										  RIGHT=1-XCOR1,
								   		  as.numeric(elpos[1])),
								   switch(elpos[2],
								   	   top= 1-my-frameY-elemHeight, 
								   	   center=.5, 
								   	   centre=.5, 
								   	   bottom=my+metaY+frameY,
								   	   TOP=1-frameY-elemHeight,
								   	   BOTTOM=frameY,
								   	   as.numeric(elpos[2])))	
				if (any(is.na(elem.position))) stop("Wrong position argument for attributes", call. = FALSE)
				elem.max.width <- if (gt$legend.position[1] %in% c("center", "centre")) {
					.5 - mx - legendWidth/2 - metaX - 2*frameX
				} else {
					1 - mx - legendWidth - ifelse(gt$legend.position[1] %in% c("left"), metaX, 0) - ifelse(gt$legend.position[1] %in% c("left", "right"), mx, 0) - ifelse(elpos[1] %in% c("left", "right"), mx, 0) - 2*frameX
				}
			} else if (elemtitle) {
				elemSnapToRight <- FALSE
				xcor1 <- mx+frameX
				XCOR1 <- frameX
				elem.position <- c(switch(elpos[1], 
										  left=xcor1+metaX,#mx+metaX+frameX,
										  center=xcor1,#.5,
										  centre=xcor1,#.5,
										  right=1-xcor1,#1-mx-frameX,
										  LEFT=XCOR1,#frameX,
										  RIGHT=1-XCOR1,#1-frameX,
										  as.numeric(elpos[1])),
								   switch(elpos[2],
								   	   top= 1-frameY-ifelse(gt$title.position[2]=="top", my, 0) - elemHeight - titleHeight, 
								   	   center=.5, 
								   	   centre=.5, 
								   	   bottom=frameY + ifelse(gt$title.position[2]=="bottom", my, 0)+metaY+titleHeight,
								   	   TOP=1-frameY-ifelse(gt$title.position[2]=="top", my, 0) - elemHeight - titleHeight,
								   	   BOTTOM=frameY+ifelse(gt$title.position[2]=="bottom", my, 0)+titleHeight,
								   	   as.numeric(elpos[2])))	
				if (any(is.na(elem.position))) stop("Wrong position argument for attributes", call. = FALSE)
				elem.max.width <- 1 - (if (has.legend && tolower(elpos[2])==tolower(gt$legend.position[2])) 2*mx + legendWidth else mx) - ifelse(elpos[1] %in% c("left", "right"), mx, 0) - metaX - 2 * frameX
			} else {
				elemSnapToRight <- FALSE
				xcor1 <- mx+frameX
				XCOR1 <- frameX
				elem.position <- c(switch(as.character(elpos[1]), 
										  left=xcor1+metaX,#mx+metaX+frameX,
										  center=.5,
										  centre=.5,
										  right=1-xcor1,#1-mx-frameX,
										  LEFT=XCOR1,#frameX,
										  RIGHT=1-XCOR1,#1-frameX,
										  as.numeric(elpos[1])),
								   switch(as.character(elpos[2]),
								   	   top= 1-my-elemHeight-frameY, 
								   	   center=.5, 
								   	   centre=.5, 
								   	   bottom=my+metaY+frameY,
								   	   TOP=1-elemHeight-frameY,
								   	   BOTTOM=frameY,
								   	   as.numeric(elpos[2])))
								   	   
				if (any(is.na(elem.position))) stop("Wrong position argument for attributes", call. = FALSE)
				elem.max.width <- (if (has.legend && tolower(elpos[2])==tolower(gt$legend.position[2])) 1 - 3*mx - legendWidth else 1 - 2*mx) - metaX - 2 * frameX
			}
			elem.just <- c(switch(as.character(elpos[1]),
								center="center",
								centre="center",
								right="right",
								RIGHT="right",
								left="left",
								LEFT="left",
								eljust[1]),
							ifelse(elpos[2] %in% c("top", "TOP", "bottom", "BOTTOM"), "bottom",
							ifelse(elpos[2] %in% c("center", "centre"), "center", eljust[2])))
			
			if (elemleg && elem.just[1]=="center") elem.just[1] <- "right"
			
			elem.just <- c(ifelse(is_num_string(elem.just[1]),
								  as.numeric(elem.just[1]),
								  ifelse(elem.just[1]%in%c("center", "centre"),
								  	   .5, as.integer(elem.just[1]=="right"))),
						   ifelse(is_num_string(elem.just[2]),
						   	   as.numeric(elem.just[2]),
						   	   ifelse(elem.just[2]%in%c("center", "centre"),
						   	   	   .5, as.integer(elem.just[2]=="top"))))

			el$y <- elem.position[2] + c(0, cumsum(el$height))[1:nrow(el)]
			
			

			el$y <- el$y - elem.just[2] * sum(el$height)
			
			el$width2 <- pmin(el$width, elem.max.width)
			
			el$x <- elem.position[1] #+ if (elem.just=="left") 0 else if (elem.just=="center") el$width2/2 else el$width2
			
			structure(lapply(1:nrow(el), function(i) {
				e <- el[i,]
				vpi <- viewport(x=e$x, 
								y=e$y,
								height=e$height, width=e$width2,
								just=c(elem.just[1], 0),
								name=as.character(e$type))
				pushViewport(vpi)
				
				if (e$type=="credits") {
					grb <- plot_cred(gt, just=elem.just[1], id=e$cred.id)
				} else if (e$type=="logo") {
					grb <- plot_logo(gt, just=elem.just[1], id=e$logo.id)
				} else if (e$type=="scale_bar") {
					grb <- plot_scale(gt, just=elem.just[1], xrange=(bb[3] - bb[1])*e$width2, crop_factor=gt$scale.width/e$width2)
				} else {
					grb <- plot_compass(gt, just=elem.just[1])
				}
				#grt <- gTree(children=gList(rectGrob()), vp=vpi)
				grt <- gTree(children=gList(grb), vp=vpi)
				upViewport()
				grt
			}), snap=rep(elemSnapToRight, nrow(el)))
		})
		elemSnapToRight <- unlist(lapply(elemGrobs, attr, "snap"))
		elemGrobs <- do.call("c", elemGrobs)
		
		
		treeElem <- gTree(children=do.call("gList", elemGrobs))
	} else {
	  elemSnapToRight <- FALSE
		treeElem <- NULL
	}
	
	
	
	treeLegend <- if (has.legend) {
	
		vpLegend <- viewport(y=legend.position[2], x=legend.position[1], 
							 height=legendHeight, width=legendWidth, 
							 just=gt$legend.just, name="legend")
		
		pushViewport(vpLegend)
		
		legend.frame <- !is.na(gt$legend.frame)
		legend.bg.color <- gt$legend.bg.color
		
		legend.frame.color <- if (legend.frame) {
			if (is.logical(gt$legend.frame) || gt$design.mode) "black" else gt$legend.frame
		} else NA
		
		legend.frame.fill <- if (gt$design.mode) "#888888BB" else legend.bg.color
		
		
		legend.frame.lwd <- gt$scale * gt$legend.frame.lwd
		
		if (stackV) {
			vpLeg <- viewport(layout=grid.layout(k, 2, heights=heights, widths=c(histWidth, 1-histWidth)), name="legend_grid")
		} else {
			#legTitleHeight
			if (!is.null(xtitle)) {
				vpLeg <- viewport(layout=grid.layout(3, nx, heights=nh*c(legTitleHeight, legItemTitleHeight, legItemHeight), widths=legendWidth/nx), name="legend_grid")
			} else {
				vpLeg <- viewport(layout=grid.layout(2, nx, heights=nh*c(legItemTitleHeight, legItemHeight), widths=legendWidth/nx), name="legend_grid")
			}
			
		}
		

		if (gt$legend.inside.box) {
			vpLegFrame <- viewport(width=1-mx, height=1-my, name="legend_frame")
			vpLeg <- vpStack(vpLegFrame, vpLeg)
		} 
		
		pushViewport(vpLeg)
		
		if (stackV) {
			grobListRes <- mapply("legend_subplot", x, id=1:k, MoreArgs = list(gt=gt, histWidth=histWidth), SIMPLIFY = FALSE)
		} else {
			#x_null <- sapply(x, is.null)
			#grobListRes <- mapply("legend_subplot2", x[!x_null], id=(1:k)[!x_null], rel_height=rel_heights[!x_null], MoreArgs = list(gt=gt, histWidth=histWidth), SIMPLIFY = FALSE)
			
			grobListRes <- mapply("legend_subplot2", x, id=(1:k), rel_height=rel_heights, MoreArgs = list(gt=gt, histWidth=histWidth, titleRow=as.numeric(!is.null(xtitle))), SIMPLIFY = FALSE)
			if (!is.null(xtitle)) {
				grobListRes <- c(grobListRes, list(legend_subplot2(xtitle, id=-nx, rel_height=1, gt=gt, histWidth=histWidth, titleRow=0)))
			}
			
		}
	
		grobList <- lapply(grobListRes, "[[", 1)

		if (stackV) {
			if (autoWidth) {
				legWidth <- max(sapply(grobListRes, "[[", 2))
				if (gt$legend.inside.box) legWidth <- legWidth / (1-mx)
			} else {
				legWidth <- 1#legendWidth
				#if (gt$legend.inside.box) legWidth <- legWidth / (1-mx)
				#legWidthInch <- convertWidth(unit(legWidth+mx, "npc"), "inch", valueOnly=TRUE)
			}
			legWidthInch <- convertWidth(unit(legWidth, "npc"), "inch", valueOnly=TRUE)
			
			grobLegBG <- rectGrob(x=0, width=legWidth, just=c("left", "center"), gp=gpar(lwd=legend.frame.lwd, col=legend.frame.color, fill=legend.frame.fill))
			
		} else {
			lW <- sapply(grobListRes, "[[", 2)
			legItemTitleWidths <- sapply(grobListRes, "[[", 2)[seq(1,k, by=2)]
			legItemsWidths <- sapply(grobListRes, "[[", 2)[seq(2,k, by=2)]
			legWidths <- pmax(legItemTitleWidths, legItemsWidths) + mx

			
			if (!autoWidth) legWidths <- legWidths / sum(legWidths) * nx
			
			#if (gt$legend.inside.box) legWidths <- legWidths / (1-mx)

			legWidths2 <- legendWidth/nx * legWidths
			
			legWidthsInch <- convertWidth(unit(legWidths/nx, "npc"), "inch", valueOnly=TRUE)
			legWidthInch <- sum(legWidthsInch)
			

			# re-adjust legend
			vpLegend <- viewport(y=legend.position[2], x=legend.position[1],
								 height=legendHeight, width=sum(legendWidth/nx*legWidths),
								 just=gt$legend.just, name="legend")
			if (!is.null(xtitle)) {
				vpLeg <- viewport(layout=grid.layout(3, nx, heights=nh*c(legTitleHeight, legItemTitleHeight, legItemHeight), widths=legWidths), name="legend_grid")
			} else {
				vpLeg <- viewport(layout=grid.layout(2, nx, heights=nh*c(legItemTitleHeight, legItemHeight), widths=legWidths), name="legend_grid")
			}

			if (gt$legend.inside.box) {
				vpLegFrame <- viewport(width=1-mx, height=1-my, name="legend_frame")
				vpLeg <- vpStack(vpLegFrame, vpLeg)
			} 
			

			# vpLeg$layout$widths <- unit(legWidths, "npc") #unit(legendWidth/nx*legWidths, "npc")
			for (i in seq(1, k, by=2)) {
				idi <- i/2 + .5
				if (!is.null(grobList[[i]])) grobList[[i]] <- grob_mod(grobList[[i]], x.b=1/legWidths[idi])
				if (!is.null(grobList[[i+1]])) grobList[[i+1]] <- grob_mod(grobList[[i+1]], x.b=1/legWidths[idi])
			}
				
			grobLegBG <- rectGrob(x=0, width=1, just=c("left", "center"), gp=gpar(lwd=gt$scale, col=gt$legend.frame, fill=legend.frame.fill))
		}

		upViewport(2 + gt$legend.inside.box)
		if (legSnapToRight && autoWidth) { #  
			legWidthNpc <- convertWidth(unit(legWidthInch, "inch"), "npc", valueOnly = TRUE)
			shiftX <- (legendWidth-legWidthNpc) * legSnapToRight
			vpLegend$x <- unit(legend.position[1] + shiftX, "npc")
			# vpLegend$width <- unit(legWidthNpc, "npc") #not working, since legend items are drawn with npc instead of inch
			
			iselemSnapToRight <- as.logical(elemSnapToRight)
			if (any(iselemSnapToRight)) {
			  for (i in which(iselemSnapToRight)) {
			    treeElem$children[[i]]$vp$x <- treeElem$children[[i]]$vp$x + unit(shiftX, "npc")
			  }
			}
		}
		
		gTree(children=gList(grobLegBG, gTree(children=do.call("gList", grobList), vp=vpLeg)), vp=vpLegend, name="legend")
	} else {
		NULL
	}
	
	treeMeta <- gTree(children=gList(grobTitle, treeElem, treeLegend), name="meta")
	
	treeMeta
}

