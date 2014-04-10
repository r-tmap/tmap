legend_plot <- function(gt, x) {
	
	
	x <- x[gt$legend.order]
	
	x.not.null <- !sapply(x, is.null)
	
	k <- sum(x.not.null)
	
	x <- x[x.not.null]
	
	heights <- c(choro=gt$legend.choro.height,
				 hist=gt$legend.choro.hist.height,
				 bubble.size=gt$legend.bubble.size.height,
				 bubble.col=gt$legend.bubble.col.height)
	heights <- heights[gt$legend.order][x.not.null]
	
	
# 	
# 	nitems <- ifelse(legend.show.text, length(legend.labels), 0)
# 	
# 	
# 	lineHeight <- convertHeight(unit(1, "lines"), unitTo="npc", valueOnly=TRUE)
# 	linesHeight <- lineHeight * nitems
# 
# 	if (!draw) legend.plot.height <- 0
# 
# 	cex.ub <- legend.text.height / linesHeight * .85
# 	cex <- min(cex.ub, legend.text.cex)
# 	legend.text.height <- (cex*linesHeight) / .85
# 	
 	titleWidth <- convertWidth(stringWidth(gt$title), "npc", valueOnly=TRUE) * gt$title.cex
 	titleHeight <- convertHeight(unit(2, "lines"), "npc", valueOnly=TRUE) * gt$title.cex

	legendHeight <- sum(heights) #legend.text.height+legend.plot.height
	legendWidth <- gt$legend.width

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
									   top=1-titleHeight/2, 
									   center=(1-titleHeight)/2, 
									   centre=(1-titleHeight)/2, 
									   bottom=0.02+titleHeight/2+legendHeight))
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
										   top=1-titleHeight/2, 
										   center=(1-titleHeight)/2, 
										   centre=(1-titleHeight)/2, 
										   bottom=titleHeight/2))	
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

	grid.text(gt$title, x=title.position[1], y=title.position[2], 
			  just=c("left", "center"), gp=gpar(cex=gt$title.cex))
	
	
	vpLegend <- viewport(y=legend.position[2], x=legend.position[1], 
						 height=legendHeight, width=legendWidth, 
						 just=c("left", "bottom"))
	
	pushViewport(vpLegend)
	if (!is.na(gt$legend.bg.color)) grid.rect(gp=gpar(col=NA, fill=gt$legend.bg.color))



# if (legend.plot.type %in% c("hist", "bar")) {
# 	nas <- is.na(values)
# 	missings <- any(nas)
# 	
# 	values <- values[!nas]
# 	n <- length(values)
# }

	
# 	## fixed layout parameters
# 	lineInch <- convertHeight(unit(1, "lines"), "inch", valueOnly=TRUE) * legend.plot.cex
# 	wordInch <- convertWidth(stringWidth("12345"), "inch", valueOnly=TRUE) * legend.plot.cex
# 
# 	yaxisWidth <- wordInch #0.6 # width of the y axis cell in inch
# 	axesMargin <- lineInch/4 # margin between plot and the axes in inch
# 	xaxisHeight <- ifelse(legend.plot.type=="hist", 2, ifelse(legend.plot.type=="none", 0, 0.5)) # height of x axis in "lines"
# 	#spacer <- 0.5 #space between plot and list in "lines"
# 
# 	xaxi.height <- lineHeight * xaxisHeight *  legend.plot.cex
# 	plot.height <- legendHeight - xaxi.height - legend.text.height
# 	
	
	# normalize heights
	heights <- heights / legendHeight

# 	pushViewport(
# 		viewport(layout=grid.layout(3, 2, 
# 									heights=unit(c(plot.height, xaxi.height, legend.text.height) / legendHeight, c("npc", "npc", "npc")),
# 									widths=unit(c(1, yaxisWidth), c("null", "inch")))))

	pushViewport(viewport(layout=grid.layout(k, 1, heights=heights, widths=1)))

	
	lapply(x, FUN="legend_subplot", gt)

	upViewport(2)
}


legend_subplot <- function(y, gt) {
	id <- substitute(y)[[3]]
	name <- paste("legend_plot_", eval.parent(quote(names(X)))[id], sep="")
	cellplot(id, 1, e=do.call(name, args=list(y, gt)))
}

legend_plot_hist <- function(x, gt) {
	with(x, {
		if (is.factor(choro.values)) {
			numbers <- table(choro.values)
			xticks <- seq(0, 1, length.out=length(numbers)*2+1)[seq(2,length(numbers)*2,by=2)]
			ptx <- levels(choro.values)
			colors <- choro.legend.palette
		} else {
			choro.values <- na.omit(choro.values)
			breaks2 <- pretty(choro.values, n=30)
			bins.mean <- (breaks2[-1] + breaks2[1:(length(breaks2)-1)])/2
			
			toolow <- (bins.mean < min(choro.breaks))
			toohigh <- (bins.mean > max(choro.breaks))
			
			bins.mean <- bins.mean[!toolow & !toohigh]
			breaks2 <- breaks2[(sum(toolow)+1):(length(breaks2)-sum(toohigh))]
			
			cchoro.values <- cut(choro.values, breaks=breaks2, include.lowest=TRUE, right=FALSE)
			
			numbers <- as.vector(table(cchoro.values))
			
			colors <- choro.legend.palette[sapply(bins.mean, function(x) which(x<choro.breaks)[1]-1)]
			
			ptx <- pretty(breaks2, n=5)
			ptx <- ptx[ptx>breaks2[1] & ptx<tail(breaks2,1)]
			rng <- range(breaks2)
			xticks <- (ptx - rng[1]) / (rng[2] - rng[1])
		}
		maxnumber <- max(numbers)
		pty <- pretty(c(0, numbers), n=5)
		
		xaxis <- 0
		hs <- numbers / maxnumber
		pty <- pty[pty<=maxnumber]
		hpty <- pty / maxnumber
		
		x <- seq(0, 1, length.out=length(numbers)+1)[1:length(numbers)]
		
		ws <- 1/length(numbers)
		
		lineHeight <- convertHeight(unit(1, "lines"), unitTo="npc", valueOnly=TRUE) * gt$legend.hist.cex
		
		# lower 1/2 line
		hs <- hs * (1- (lineHeight/2))
		hpty <- hpty * (1- (lineHeight/2))
		
		 
		# parameters in lines
		yaxisWidth <- 2.5 * gt$legend.hist.cex
		axesMargin <- 1/4 * gt$legend.hist.cex
		xaxisHeight <- 2 * gt$legend.hist.cex
		
		pushViewport(
			viewport(layout=grid.layout(2, 2, 
										heights=unit(c(1, xaxisHeight), c("null", "lines")),
										widths=unit(c(1, yaxisWidth), c("null", "lines")))))
		
		
		cellplot(1,1, e={
			grid.rect(x=x, y=xaxis, width=ws, height=hs, gp=gpar(col=NA,fill=colors), just=c("left", "bottom"))
		})
		
		
		
		# plot y axis
		cellplot(1,2,e={
			margin.npc <- convertWidth(unit(axesMargin, "lines"), "npc", valueOnly=TRUE)
			grid.polyline(x=c(margin.npc, margin.npc, 
							  rep(c(margin.npc,margin.npc+0.03), length(pty))),
						  y=c(0, 1, rep(xaxis+hpty, each=2)),
						  id=rep(1:(length(pty)+1),each=2))
			formatted <- format(pty, trim=TRUE)
			maxWidth <- max(convertWidth(stringWidth(formatted), unitTo="npc", valueOnly=TRUE))
			maxHeight <- max(convertHeight(stringHeight(formatted), unitTo="npc", valueOnly=TRUE)) 
			
			cex <- min(gt$legend.hist.cex, 
					   (1-margin.npc*2)/maxWidth, 
					   (1/(length(pty)+1))/maxHeight)
					   
			
			grid.text(formatted, x=rep(margin.npc+0.08+margin.npc+maxWidth*cex, length(pty)), y=xaxis+hpty, 
					  just=c("right","center"), gp=gpar(cex=cex))
		})
		
		# plot x axis tick marks
		cellplot(2,1,e={
			maxWidth <- max(convertWidth(stringWidth(ptx), unitTo="npc", valueOnly=TRUE))
			n <- length(xticks)
			cex <- min(gt$legend.hist.cex, 
					   (1/(n+1))/maxWidth)
			
			line_height <- 1 - convertHeight(unit(axesMargin, "lines"), "npc", valueOnly=TRUE)
			if (xaxis==0) grid.lines(x=c(0,1), y=c(line_height, line_height))
			
			grid.polyline(x=rep(xticks, each=2), y=rep(c(line_height, line_height-0.1), n), 
						  id=rep(1:n, each=2)) 
			grid.text(ptx, x=xticks, y=line_height-0.3, gp=gpar(cex=cex))
		})
		upViewport()
	})		
	
}
legend_plot_choro <- legend_plot_bubble.col <- function(x, gt) {
	with(x, {
		nitems <- length(legend.labels)
		
		lineHeight <- convertHeight(unit(1, "lines"), unitTo="npc", valueOnly=TRUE)
		linesHeight <- lineHeight * nitems
		
		cex.ub <- 1 / linesHeight * .85
		cex <- min(cex.ub, gt$legend.text.cex)
		
		linesHeight <- linesHeight * cex / .85
		
		yslines <- seq(1, 1-linesHeight,
					   length.out=(nitems)*2 + 1)
		yslines <- yslines[seq(2,length(yslines)-1,by=2)]
		
		itemSize <- convertWidth(unit(1,"lines"), "inch", valueOnly=TRUE) * 0.5 * cex
		if (legend.is.bubbles) {
			bubbleSizes <- min(bubble.max.size, itemSize*0.75)
			grid.circle(x=unit(rep(itemSize, nitems), "inch"), 
						y=yslines, r=unit(rep(bubbleSizes, nitems), "inch"),
						gp=gpar(fill=legend.palette))
		} else {
			grid.rect(x=unit(rep(itemSize/2, nitems), "inch"), 
					  y=yslines, 
					  width=unit(rep(itemSize, nitems), "inch"), 
					  height=unit(rep(itemSize, nitems), "inch"),
					  gp=gpar(fill=legend.palette))
		}
		grid.text(legend.labels, x=unit(rep(itemSize*2.5, nitems), "inch"), 
				  y=yslines, just=c("left", "center"), gp=gpar(cex=cex))
	})
}


legend_plot_bubble.size <- function(x, gt) {
	with(x, {
		nbubbles <- length(bubble.legend.sizes)
		
		lineH <- convertHeight(unit(1,"lines"), "npc", valueOnly=TRUE) * gt$legend.text.cex
		bubbleH <- convertHeight(unit(tail(bubble.legend.sizes,1),"inch"), "npc", valueOnly=TRUE)
		
		fill <- bubble.legend.col
		
		grid.circle(x=seq(0,1,length.out=nbubbles+2)[2:(nbubbles+1)],
					y=1.5*lineH+bubbleH,
					r=unit(bubble.legend.sizes, "inch"),
					gp=gpar(col="black", fill=fill))
		
		maxWidth <- max(convertWidth(stringWidth(bubble.legend.size_labels), "npc", valueOnly=TRUE))
		cex <- min((1/(nbubbles+1))/maxWidth*.85, gt$legend.text.cex)
		grid.text(bubble.legend.size_labels,
				  x=seq(0,1,length.out=nbubbles+2)[2:(nbubbles+1)],
				  y=0.75*lineH,
				  gp=gpar(cex=cex)
		)
	})
}
