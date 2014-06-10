legend_plot <- function(gt, x) {
	title.only <- all(sapply(x, is.null))
	lineHeight <- convertHeight(unit(1, "lines"), "npc", valueOnly=TRUE)
	
	conf <- gt$legend.config 
	ishist <- (substr(conf, nchar(conf)-3, nchar(conf))=="hist")
	conf2 <- ifelse(ishist, substr(conf, 1, nchar(conf)-5), conf)
	
	if (gt$legend.profile=="text") {
		conf <- conf[!ishist]
		conf2 <- conf2[!ishist]
	}
	if (gt$legend.profile=="hist") {
		conf <- conf[ishist]
		conf2 <- conf2[ishist]
	}
	
	if (!length(conf) || gt$legend.profile=="hide") title.only <- TRUE

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
		
		heights <- gt$legend.heights[names(x)]
		names(heights) <- names(x)
		heights[is.na(heights)] <- legend.title.npc
		
		heights <- heights[conf][x.not.null]
		
		# shrink heights (remove white space)
		margin <- 0.05
		heights <- sapply(heights, function(h) {
			hname <- eval.parent(quote(names(X)))[substitute(h)[[3]]]
			if (hname %in% c("choro", "bubble.col", "line.col")) {
				min(length(x[[hname]]$legend.labels) * lineHeight * gt$legend.text.cex / ((1-2*margin) * .85), h)
			} else h
		})
		
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
		if (legend.type %in% c("fill", "bubble.col", "line.col")) {
			legend_qual(x, gt$legend.text.cex, lineHeight)
		} else if (legend.type %in% c("bubble.size", "line.lwd")) {
			legend_quan(x, gt$legend.text.cex, lineHeight)
		} else if (legend.type=="fill_hist") {
			legend_hist(x, gt$legend.hist.cex, lineHeight, scale=gt$scale)
		} else if (legend.type=="title") {
			legend_title(x, gt$legend.title.cex, lineHeight)
		}
	})
}

legend_title <- function(x, legend.title.cex, lineHeight) {
	cex <- min(legend.title.cex, 1/lineHeight)
	grid.text(x$title, x=0, y=5/12 , just=c("left", "center"), gp=gpar(cex=cex))
}


legend_qual <- function(x, legend.text.cex, lineHeight) {
	with(x, {
		nitems <- length(legend.labels)
		margin <- .05
		linesHeight <- lineHeight * nitems
		cex <- min((1-2*margin) / linesHeight * .85, 
				   legend.text.cex)
		
		linesHeight <- linesHeight * cex / .85
		
		yslines <- seq(1-margin, (1-margin)-linesHeight,
					   length.out=(nitems)*2 + 1)
		yslines <- yslines[seq(2,length(yslines)-1,by=2)]
		
		itemSize <- convertWidth(unit(1,"lines"), "inch", valueOnly=TRUE) * 0.5 * cex
		if (legend.type=="bubble.col") {
			bubbleSizes <- min(bubble.max.size, itemSize)
			grid.circle(x=unit(rep(itemSize*1.5, nitems), "inch"), 
						y=yslines, r=unit(rep(bubbleSizes, nitems), "inch"),
						gp=gpar(fill=legend.palette,
								col=bubble.border.col,
								lwd=bubble.border.lwd))
		} else if (legend.type=="line.col") {
			grid.polyline(x=unit(rep(itemSize*c(1,2), nitems), "inch"),
						  y=rep(yslines, each=2), 
						  id=rep(1:nitems, each=2),
						  gp=gpar(col=legend.palette, 
						  		lwd=line.legend.lwd,
						  		lty=line.legend.lty,
						  		lineend="butt"))
		} else {
			grid.rect(x=unit(rep(itemSize*1.5, nitems), "inch"), 
					  y=yslines, 
					  width=unit(rep(itemSize*2, nitems), "inch"), 
					  height=unit(rep(itemSize*2/.85, nitems), "inch"),
					  gp=gpar(fill=legend.palette, col=border.col, lwd=lwd))
		} 
		grid.text(legend.labels, x=unit(rep(itemSize*3, nitems), "inch"), 
				  y=yslines, just=c("left", "center"), gp=gpar(cex=cex))
	})
}






legend_quan <- function(x, legend.text.cex, lineHeight) {
	with(x, {
		maxWidth <- max(convertWidth(stringWidth(legend.labels), "npc", valueOnly=TRUE))
		nitems <- length(legend.labels)
		linesHeight <- lineHeight * nitems
		xcoor <- seq(0,1,length.out=2*nitems+1)[seq(2, by=2, length.out=nitems)]
		if (legend.type == "bubble.size") {
			bubbleH <- convertHeight(unit(legend.sizes,"inch"), "npc", valueOnly=TRUE)
			bubbleHmax <- max(bubbleH) * 1.5
			
			cex <- min(legend.text.cex, 
					   (1-bubbleHmax)/lineHeight/1.5,
					   (1/(nitems+1))/maxWidth) 
			
			
			lineH <- lineHeight * cex
			
			divY <- .5 + (lineH - bubbleHmax)/2
			bubbleY <- divY + bubbleHmax / 1.5
			lineY <- divY - lineH*.75
			grid.circle(x=xcoor,
						y=bubbleY,
						r=bubbleH,
						gp=gpar(col=bubble.border.col, lwd=bubble.border.lwd, fill=legend.palette))
		} else {
			cex <- min(legend.text.cex, 
					   .5/lineHeight,
					   (1/(nitems+1))/maxWidth) 
			linesY <- 1-(lineHeight / 2)
			lineY <- (lineHeight * cex)/1.5
			restY <- 1 - lineY*1.5
			xs <- rep(xcoor, each=2)
			ys <- rep(c(1  - restY * .2, 1 - restY * .8), nitems)
			grid.polyline(x=xs,
						  y=ys,
						  id=rep(1:nitems, each=2),
						  gp=gpar(lwd=legend.lwds,
						  		col=legend.palette,
						  		lineend="butt"))
		}
		grid.text(legend.labels,
				  x=xcoor,
				  y=lineY,
				  gp=gpar(cex=cex))
	})
}





legend_hist <- function(x, legend.hist.cex, lineHeight, scale) {
	with(x, {
		if (is.factor(values)) {
			numbers <- table(values)
			xticks <- seq(0, 1, length.out=length(numbers)*2+1)[seq(2,length(numbers)*2,by=2)]
			ptx <- levels(values)
			colors <- legend.palette
		} else {
			values <- na.omit(values)
			breaks2 <- pretty(values, n=30)
			
			toolow <- (breaks2 < min(breaks))
			toohigh <- (breaks2 > max(breaks))
			
			startID <- max(sum(toolow), 1)
			endID <- length(breaks2) - max(sum(toohigh), 1) + 1
			
			breaks2 <- breaks2[startID:endID]
			bins.mean <- (breaks2[-1] + breaks2[1:(length(breaks2)-1)])/2
			
			cvalues <- cut(values, breaks=breaks2, include.lowest=TRUE, right=FALSE)
			
			numbers <- as.vector(table(cvalues))
			breaks[1] <- -Inf
			breaks[length(breaks)] <- Inf
			
			colors <- legend.palette[sapply(bins.mean, function(x) which(x<breaks)[1]-1)]
			
			ptx <- pretty(breaks2, n=5)
			ptx <- ptx[ptx>breaks2[1] & ptx<tail(breaks2,1)]
			rng <- range(breaks2)
			xticks <- (ptx - rng[1]) / (rng[2] - rng[1])
		}
		maxnumber <- max(numbers)
		pty <- pretty(c(0, numbers), n=5)
		
		hs <- numbers / maxnumber
		pty <- pty[pty<=maxnumber]
		hpty <- pty / maxnumber
		
		x <- seq(0, 1, length.out=length(numbers)+1)[1:length(numbers)]
		
		ws <- 1/length(numbers)
		
		
		# lower 1/2 line
		hs <- hs * (1- (lineHeight/2))
		hpty <- hpty * (1- (lineHeight/2))
		
		formattedY <- format(pty, trim=TRUE)
		
		width.npc <- max(convertWidth(stringWidth(ptx), unitTo="npc", valueOnly=TRUE)) * 1.5 * length(ptx)
		height.npc <- convertHeight(unit(length(formattedY)+2, "lines"), "npc", valueOnly=TRUE) * 1.5
		
		margin <- 0.05
		npc.total <- 1-2*margin
		
		cex <- min(legend.hist.cex,
				   npc.total/width.npc,
				   npc.total/height.npc)
		
		width.npc <- width.npc * cex
		height.npc <- height.npc * cex
		
		
		width.yaxis <- max(convertWidth(stringWidth(formattedY), unitTo="npc", valueOnly=TRUE)) * cex * 1.5
		height.xaxis <- lineHeight * cex
		
		axisMargin <- convertWidth(unit(0.02, "npc"), "inch", valueOnly=TRUE)
		axisTicks <- convertWidth(unit(0.01, "npc"), "inch", valueOnly=TRUE)
		
		pushViewport(
			viewport(layout=grid.layout(5, 5, 
										heights=unit(c(margin, 1, axisMargin+axisTicks, height.xaxis, margin), c("npc", "null", "inch", "npc", "npc")),
										widths=unit(c(margin, 1, axisMargin+axisTicks, width.yaxis, margin), c("npc", "null", "inch", "npc", "npc")))))
		
		cellplot(2,2, e={
			#grid.rect(gp=gpar(fill="lightblue2"))
			grid.rect(x=x, y=0, width=ws, height=hs, gp=gpar(col=NA,fill=colors), just=c("left", "bottom"))
		})
		
		# plot y axis
		cellplot(2,3,e={
			axisMargin.npc <- convertWidth(unit(axisMargin, "inch"), "npc", valueOnly=TRUE)
			axisTicks.npc <- convertWidth(unit(axisTicks, "inch"), "npc", valueOnly=TRUE)
			
			
			grid.polyline(x=c(axisMargin.npc, axisMargin.npc, 
							  rep(c(axisMargin.npc,axisMargin.npc+axisTicks.npc), length(pty))),
						  y=c(0, 1, rep(hpty, each=2)),
						  id=rep(1:(length(pty)+1),each=2), gp=gpar(lwd=scale))
		})
		
		cellplot(2,4,e={
			maxWidth <- max(convertWidth(stringWidth(formattedY), unitTo="npc", valueOnly=TRUE)) * cex * 1.5
			grid.text(formattedY, x=maxWidth, y=hpty, 
					  just=c("right","center"), gp=gpar(cex=cex))
		})
		
		# plot x axis tick marks
		cellplot(3,2,e={
			axisMargin.npc <- convertHeight(unit(axisMargin, "inch"), "npc", valueOnly=TRUE)
			axisTicks.npc <- convertHeight(unit(axisTicks, "inch"), "npc", valueOnly=TRUE)
			
			n <- length(xticks)
			
			line_height <- convertHeight(unit(1, "lines"), "npc", valueOnly=TRUE) * cex
			grid.lines(x=c(0,1), y=c(1-axisMargin.npc, 1-axisMargin.npc), gp=gpar(lwd=scale))
			grid.polyline(x=rep(xticks, each=2), y=rep(c(1-axisMargin.npc, 1-axisMargin.npc-axisTicks.npc), n), 
						  id=rep(1:n, each=2), gp=gpar(lwd=scale)) 
		})
		
		cellplot(4,2,e={
			grid.text(ptx, x=xticks, y=.5, gp=gpar(cex=cex))
		})
		
		upViewport()
	})
}
