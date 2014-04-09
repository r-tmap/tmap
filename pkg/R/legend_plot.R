legend_plot <- function(gt, x) {
	
	browser()
	
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

	legend_subplot <- function(y) {
		name <- paste("legend_plot_", names(x)[substitute(y)[[3]]], sep="")
		do.call(name, args=list())
	}
	lapply(x, FUN="legend_subplot")

	legend_plot_hist <- function(...) {
		1
	}
	legend_plot_choro <- function(...) {
		2
	}
	legend_plot_bubble.size <- function(...) {
		3
	}
	legend_plot_bubble.col <- function(...) {
		4
	}


	#cellplot(1,2, e=grid.rect())
	#cellplot(2,1, e=grid.rect())

	## preprocess data
	if (legend.plot.type=="bar") {
		ncat <- nitems - missings
		
		colors <- as.character(cut(values, breaks=breaks, 
								   include.lowest=TRUE, right=FALSE,
								   labels=legend.palette[1:ncat]))
		
		o <- order(values)
		colors <- colors[o]
		
		rng <- range(values)
		pty <- pretty(values, n=5)
	
		if (all(rng >=0)) {
			hs <- values[o] / rng[2]
			xaxis <- 0
			pty <- pty[pty<=rng[2]]
			hpty <- pty / rng[2]
		} else if (all(rng <=0)) {
			hs <- -(values[o] / rng[1])
			xaxis <- 1
			pty <- pty[pty>=rng[1]]
			hpty <- -(pty / rng[1])
		} else {
			hs <- values[o] / (rng[2] - rng[1])
			xaxis <- abs(rng[1]) / (rng[2] - rng[1])
			pty <- pty[pty>=rng[1] & pty<=rng[2]]
			hpty <- pty / (rng[2] - rng[1])
		}
	
		# determine group median ids
		id <- c(which(!duplicated(colors)), n+1)
		id <- id[-(ncat+1)] + round((id[-1]-id[-(ncat+1)]) / 2)
	
		x <- seq(0, 1, length.out=n+1)[1:n]
		ws <- 1/n
	} else if (legend.plot.type=="hist") {
		if (is.factor(values)) {
            numbers <- table(values)
            xticks <- seq(0, 1, length.out=length(numbers)*2+1)[seq(2,length(numbers)*2,by=2)]
            ptx <- levels(values)
            colors <- legend.palette
		} else {
		    breaks2 <- pretty(values, n=30)
		    bins.mean <- (breaks2[-1] + breaks2[1:(length(breaks2)-1)])/2
		    
		    toolow <- (bins.mean < min(breaks))
		    toohigh <- (bins.mean > max(breaks))
		    
		    bins.mean <- bins.mean[!toolow & !toohigh]
		    breaks2 <- breaks2[(sum(toolow)+1):(length(breaks2)-sum(toohigh))]
		    
		    cvalues <- cut(values, breaks=breaks2, include.lowest=TRUE, right=FALSE)
		    
		    numbers <- as.vector(table(cvalues))
		    
		    colors <- legend.palette[sapply(bins.mean, function(x) which(x<breaks)[1]-1)]
		    
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

		
	}

	# lower 1/2 line
	if (legend.plot.type %in% c("bar", "hist")) {
		hs <- hs * (1- (lineHeight/2) / plot.height)
		hpty <- hpty * (1- (lineHeight/2) / plot.height)
	}
		
	# plot legend and assignment lines
	if (legend.show.text) cellplot(3,1,e={
		yslines <- seq(1, 0,
					   length.out=(nitems)*2 + 1)
		yslines <- yslines[seq(2,length(yslines)-1,by=2)]
		
		
		## show assignment lines
		if (legend.plot.type=="bar" && legend.show.text && xaxis==0) {
			wslines <- convertUnit(stringWidth(legend.labels[1:ncat]),unitTo="npc", valueOnly=TRUE) + 0.11
			
			showLines <- (x[id]-wslines)>0
			if (any(showLines)) {
				grid.polyline(x=rep(x[id][showLines], each=2), 
							  y=as.vector(sapply(yslines[showLines], function(x)c(1,x))),
							  id=rep(1:sum(showLines),each=2), gp=gpar(col=grey(.3)))
				grid.polyline(x=as.vector(mapply(c, wslines[showLines], x[id][showLines])), 
							  y=rep(yslines[showLines], each=2),
							  id=rep(1:sum(showLines),each=2), gp=gpar(col=grey(.3)))
			}
		}
		
		## list categories
		if (legend.show.text) {
		    itemSize <- convertWidth(unit(1,"lines"), "inch", valueOnly=TRUE) * 0.5 * cex
			if (legend.bubbles) {
				bubbleSizes <- min(max(legend.bubble.sizes), itemSize*0.75)
				grid.circle(x=unit(rep(itemSize/2, nitems), "inch"), 
                            y=yslines, r=unit(rep(bubbleSizes, nitems), "inch"),
                            gp=gpar(fill=legend.palette))
			} else {
				grid.rect(x=unit(rep(itemSize/2, nitems), "inch"), 
                          y=yslines, 
                          width=unit(rep(itemSize, nitems), "inch"), 
                          height=unit(rep(itemSize, nitems), "inch"),
                          gp=gpar(fill=legend.palette))
			}
			grid.text(legend.labels, x=unit(rep(itemSize*2, nitems), "inch"), 
					  y=yslines, just=c("left", "center"), gp=gpar(cex=cex))
		}
		
	})
	
	# plot x axis tick marks
	if (legend.plot.type %in% c("hist", "bar")) cellplot(2,1,e={
		line_height <- 1 - convertHeight(unit(axesMargin, "inch"), "npc", valueOnly=TRUE)
		if (xaxis==0) grid.lines(x=c(0,1), y=c(line_height, line_height))
		
		if (legend.plot.type=="bar" && legend.show.text && xaxis==0) {
			# plot vertical assignment lines
			grid.polyline(x=rep(x[id], each=2),
					   y=rep(c(0, line_height), length(id)),
					   id=rep(1:length(id), each=2),
					   gp=gpar(col=grey(.3)))
		} else if (legend.plot.type=="hist") {
			grid.polyline(x=rep(xticks, each=2), y=rep(c(line_height, line_height-0.1), length(xticks)), 
						  id=rep(1:length(xticks), each=2)) 
			grid.text(ptx, x=xticks, y=line_height-0.3, gp=gpar(cex=legend.plot.cex))
		}
		
	})
		
	# plot chart
	if (legend.plot.type %in% c("hist", "bar")) cellplot(1,1,e={
		grid.rect(x=x, y=xaxis, width=ws, height=hs, gp=gpar(col=NA,fill=colors), just=c("left", "bottom"))
		
		# plot x axis if there are negative values; otherwise, plot it in cell2,1
		if (xaxis!=0) grid.lines(x=c(0,1), y=c(xaxis, xaxis))
	})
	
	if (legend.plot.type == "bubble") cellplot(1,1,e={
		
		nbubbles <- length(legend.bubble.sizes)
		
		lineH <- convertHeight(unit(1,"lines"), "npc", valueOnly=TRUE)
		bubbleH <- convertHeight(unit(tail(legend.bubble.sizes,1),"inch"), "npc", valueOnly=TRUE)
		
		fill <- ifelse(nitems==0, legend.palette, legend.palette[round((1+nitems)/2)])
		
		grid.circle(x=seq(0,1,length.out=nbubbles+2)[2:(nbubbles+1)],
					y=1.5*lineH+bubbleH,
					r=unit(legend.bubble.sizes, "inch"),
					gp=gpar(col=ifelse(plot.bubble.borders, "black", NA), fill=fill))
		
		maxWidth <- max(convertWidth(stringWidth(legend.bubble.labels), "npc", valueOnly=TRUE))
		cex <-  min((1/nbubbles)/maxWidth-.1, 1)
		grid.text(legend.bubble.labels,
					x=seq(0,1,length.out=nbubbles+2)[2:(nbubbles+1)],
				  	y=0.75*lineH,
				  	gp=gpar(cex=cex)
		)
		
	})
		
	
	# plot y axis
	if (legend.plot.type %in% c("hist", "bar")) cellplot(1,2,e={
		margin.npc <- convertWidth(unit(axesMargin, "inch"), "npc", valueOnly=TRUE)
		grid.polyline(x=c(margin.npc, margin.npc, 
						  rep(c(margin.npc,margin.npc+0.03), length(pty))),
					  y=c(0, 1, rep(xaxis+hpty, each=2)),
					  id=rep(1:(length(pty)+1),each=2))
		formatted <- format(pty, trim=TRUE)
		maxWidth <- max(convertWidth(stringWidth(formatted), unitTo="npc", valueOnly=TRUE)) * legend.plot.cex
		grid.text(formatted, x=rep(margin.npc+0.08+maxWidth, length(pty)), y=xaxis+hpty, 
				  just=c("right","center"), gp=gpar(cex=legend.plot.cex))
	})
	
	
	upViewport(3)
	detach("gt")
}