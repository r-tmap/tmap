legend_plot <- function(gt, x) {
	title.only <- all(sapply(x, is.null))
	lineHeight <- convertHeight(unit(1, "lines"), "npc", valueOnly=TRUE)
	
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
		
		names(x)
		types <- ifelse(sapply(x, function(y)if (is.null(y$legend.is.portrait)) FALSE else y$legend.is.portrait), "portrait", "landscape")
		types[names(x)=="fill_hist"] <- "hist"
		types[substr(names(x), 1, 5)=="title"] <- "title"
		
		
		heights <- gt$legend.heights[types]
		names(heights) <- names(x)
		heights[is.na(heights)] <- legend.title.npc
		heights <- heights[conf][x.not.null]
		
		# shrink heights (remove white space)
		margin <- 0.25
		heights <- mapply(function(h, p, hname) {
			if (p && hname %in% c("fill", "bubble.col", "line.col", "line.lwd")) {
				min(length(x[[hname]]$legend.labels) * lineHeight * gt$legend.text.cex + 2*margin*lineHeight, h)
			} else if (p && hname == "bubble.size") {
				min(sum(convertHeight(unit(x[[hname]]$legend.sizes, "inch"), "npc", valueOnly=TRUE)) * 2 * 1.25, h)
			} else h
		}, heights, types=="portrait", names(heights))
		
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
		legendHeight <- min(sum(heights), 1-titleHeight, gt$legend.max.height)
	} else {
		legendHeight <- min(sum(heights), 1, gt$legend.max.height)
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

	plot_text(co.npc=matrix(title.position, ncol=2), 
			  g=list(text=gt$title, text.cex=gt$title.cex, 
			  	   text.cex.lowerbound=gt$title.cex, 
			  	   text.fontcolor="black",
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
		return(NULL)
	}

	vpLegend <- viewport(y=legend.position[2], x=legend.position[1], 
						 height=legendHeight, width=legendWidth, 
						 just=c("left", "bottom"))
	
	pushViewport(vpLegend)
	if (!is.na(gt$legend.bg.color)) grid.rect(gp=gpar(col=NA, fill=gt$legend.bg.color))
	heights <- heights / legendHeight
	pushViewport(viewport(layout=grid.layout(k, 1, heights=heights, widths=1)))
	lapply(x, FUN="legend_subplot", gt)

	upViewport(2)
}


legend_subplot <- function(x, gt) {
	id <- substitute(x)[[3]]
	cellplot(id, 1, e={
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
	grid.rect()
	cex <- min(legend.title.cex, 1/lineHeight)
	grid.text(x$title, x=0, y=5/12 , just=c("left", "center"), gp=gpar(cex=cex))
}


legend_portr <- function(x, legend.text.cex, lineHeight) {
	with(x, {
		my <- lineHeight / 4
		mx <- convertWidth(convertHeight(unit(my, "npc"), "inch"), "npc", TRUE)
		s <- 1.25 ## for bubbles only
		r <- 1-2*my
		
		# determine number of items
		if (legend.type=="bubble.size") {
			hs <- convertHeight(unit(legend.sizes, "inch"), "npc", valueOnly=TRUE) * 2
			lhs <- hs * s
			if (sum(lhs)>r) {
				clipID <- which(cumsum(lhs) > r)[1]
				hs <- hs[1:(clipID-1)]
				lhs <- lhs[1:(clipID-1)]
				legend.labels <- legend.labels[1:(clipID-1)]
			}
			nitems <- length(hs)
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
			
		
		if (legend.type=="fill") {
			grid.rect(x=mx+ws/2, 
					  y=ys, 
					  width= ws, 
					  height= hs,
					  gp=gpar(fill=legend.palette, col=border.col, lwd=lwd))
		} else if (legend.type %in% c("bubble.size", "bubble.col")) {
			grid.circle(x=mx+ws/2, 
					y=ys, r=unit(hsi/2, "inch"),
					gp=gpar(fill=legend.palette,
							col=bubble.border.col,
							lwd=bubble.border.lwd))
		} else if (legend.type %in% c("line.col", "line.lwd")) {
			lwds <- if (legend.type == "line.col") line.legend.lwd else legend.lwds
			grid.polyline(x=mx+ c(0,1)*rep(ws, each=2),
  				  y=rep(ys, each=2), 
  				  id=rep(1:nitems, each=2),
  				  gp=gpar(col=legend.palette, 
  				  		lwd=lwds,
  				  		lty=line.legend.lty,
  				  		lineend="butt"))
		}
		grid.text(legend.labels, x=mx*2+wsmax,
				  y=ys, just=c("left", "center"), gp=gpar(cex=cex))
		
	})
}


legend_landsc <- function(x, legend.text.cex, lineHeight) {
	with(x, {
		grid.rect()
		my <- lineHeight / 4
		mx <- convertWidth(convertHeight(unit(my, "npc"), "inch"), "npc", TRUE)
		s <- 1.25 ## for bubbles only
		r <- 1-2*mx
		
		# determine number of items
		if (legend.type=="bubble.size") {
			ws <- convertWidth(unit(legend.sizes, "inch"), "npc", valueOnly=TRUE) * 2
			lws <- ws * s
			if (sum(lws)>r) {
				clipID <- which(cumsum(lws) > r)[1]
				hw <- hw[1:(clipID-1)]
				lws <- lws[1:(clipID-1)]
				legend.labels <- legend.labels[1:(clipID-1)]
			}
			nitems <- length(ws)
		} else {
			nitems <- length(legend.labels)
			lws <- ws <- rep(r / nitems, nitems)
		}
		
		if (legend.type=="bubble.col") {
			bmax <- convertWidth(unit(bubble.max.size, "inch"), "npc", valueOnly=TRUE) * 2
			ws <- pmin(ws/s, bmax)
		}
		
		xs <- mx + cumsum(lws) - lws/2
		cex <- pmin(lws / (lineHeight, legend.text.cex)
		
		hs <- convertHeight(convertWidth(unit(ws, "npc"), "inch"), "npc", TRUE)
		hsmax <- max(hs)
		wsi <- convertWidth(unit(ws, "npc"), "inch", valueOnly=TRUE)
		
		
		if (legend.type=="fill") {
			grid.rect(x=xs, 
					  y=1-my-hs/2, 
					  width= ws, 
					  height= hs,
					  gp=gpar(fill=legend.palette, col=border.col, lwd=lwd))
		} else if (legend.type %in% c("bubble.size", "bubble.col")) {
			grid.circle(x=xs, 
						y=1-my-hs/2, r=unit(wsi/2, "inch"),
						gp=gpar(fill=legend.palette,
								col=bubble.border.col,
								lwd=bubble.border.lwd))
		} else if (legend.type %in% c("line.col", "line.lwd")) {
			lwds <- if (legend.type == "line.col") line.legend.lwd else legend.lwds
			grid.polyline(x=rep(xs, each=2), 
						  y=1-my-c(0,1)*rep(hs, each=2),
						  id=rep(1:nitems, each=2),
						  gp=gpar(col=legend.palette, 
						  		lwd=lwds,
						  		lty=line.legend.lty,
						  		lineend="butt"))
		}
		grid.text(legend.labels, x=xs,
				  y=my+hsmax, just=c("center", "bottom"), gp=gpar(cex=cex))
		
	})
}



# legend_landsc <- function(x, legend.text.cex, lineHeight) {
# 	with(x, {
# 		grid.rect()
# 		maxWidth <- max(convertWidth(stringWidth(legend.labels), "npc", valueOnly=TRUE))
# 		nitems <- length(legend.labels)
# 		linesHeight <- lineHeight * nitems
# 		xcoor <- seq(0,1,length.out=2*nitems+1)[seq(2, by=2, length.out=nitems)]
# 		if (legend.type == "bubble.size") {
# 			bubbleH <- convertHeight(unit(legend.sizes,"inch"), "npc", valueOnly=TRUE)
# 			bubbleHmax <- max(bubbleH) * 1.5
# 			
# 			cex <- min(legend.text.cex, 
# 					   (1-bubbleHmax)/lineHeight/1.5,
# 					   (1/(nitems+1))/maxWidth) 
# 			
# 			lineH <- lineHeight * cex
# 			
# 			divY <- .5 + (lineH - bubbleHmax)/2
# 			bubbleY <- divY + bubbleHmax / 1.5
# 			lineY <- divY - lineH*.75
# 			grid.circle(x=xcoor,
# 						y=bubbleY,
# 						r=bubbleH,
# 						gp=gpar(col=bubble.border.col, lwd=bubble.border.lwd, fill=legend.palette))
# 		} else {
# 			cex <- min(legend.text.cex, 
# 					   .5/lineHeight,
# 					   (1/(nitems+1))/maxWidth) 
# 			linesY <- 1-(lineHeight / 2)
# 			lineY <- (lineHeight * cex)/1.5
# 			restY <- 1 - lineY*1.5
# 			xs <- rep(xcoor, each=2)
# 			ys <- rep(c(1  - restY * .2, 1 - restY * .8), nitems)
# 			grid.polyline(x=xs,
# 						  y=ys,
# 						  id=rep(1:nitems, each=2),
# 						  gp=gpar(lwd=legend.lwds,
# 						  		col=legend.palette,
# 						  		lineend="butt"))
# 		}
# 		grid.text(legend.labels,
# 				  x=xcoor,
# 				  y=lineY,
# 				  gp=gpar(cex=cex))
# 	})
# }





