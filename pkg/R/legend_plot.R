legendPlot <- function(gt, legend.palette, legend.labels, values=NULL, breaks=NULL, legend.bubbles=FALSE, legend.bubble.sizes=NULL, legend.bubble.labels=NULL, plot.bubble.borders=TRUE) {
	attach(gt)
	draw <- (type.legend.plot != "none")
	if (!draw) legend.plot.size <- c(0,0)
	
	pushViewport(viewport(gp=gpar(cex=legend.cex)))
				 
	# set legend and title dimensions
	nitems <- ifelse(show.legend.text, length(legend.labels), 0)
	linesHeight <- convertHeight(unit(nitems, "lines"), unitTo="npc", valueOnly=TRUE)
	linesWidth <- max(convertWidth(stringWidth(string=legend.labels), unitTo="npc", valueOnly=TRUE))
	
	legendWidth <- max(linesWidth, legend.plot.size[2])
	legendHeight <- legend.plot.size[1] + linesHeight
	
	titleWidth <- convertWidth(stringWidth(title), "npc", valueOnly=TRUE)
	titleHeight <- convertHeight(unit(title.cex*2, "lines"), "npc", 
								 valueOnly=TRUE)
	# translate automatic position settings
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
	
	
	
	grid.text(title, x=title.position[1], y=title.position[2], 
			  just=c("left", "center"), gp=gpar(cex=title.cex))
	
	
	vpLegend <- viewport(y=legend.position[2], x=legend.position[1], 
						 height=legendHeight, width=legendWidth, 
						 just=c("left", "bottom"))
	
	pushViewport(vpLegend)
	#grid.rect(gp=gpar(col="steelblue"))
	if (type.legend.plot %in% c("hist", "bar")) {
		nas <- is.na(values)
		missings <- any(nas)
		
		values <- values[!nas]
		n <- length(values)
	}
	
	
	## fixed layout parameters
	yaxisWidth <- 0.6 # width of the y axis cell in inch
	axesMargin <- 0.05 # margin between plot and the axes in inch
	xaxisHeight <- ifelse(type.legend.plot=="hist", 2, 0.5) # height of x axis in "lines"
	#spacer <- 0.5 #space between plot and list in "lines"
	
	pushViewport(
		viewport(layout=grid.layout(3, 2, 
									heights=unit(c(1, xaxisHeight, nitems),
												 c("null", "lines", "lines")),
									widths=unit(c(1, yaxisWidth), c("null", "inch")),)))
	
	
	## preprocess data
	if (type.legend.plot=="bar") {
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
	} else if (type.legend.plot=="hist") {
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
		
	# plot legend and assignment lines
	if (show.legend.text) cellplot(3,1,e={
		yslines <- seq(1, 0,
					   length.out=(nitems)*2 + 1)
		yslines <- yslines[seq(2,length(yslines)-1,by=2)]
		
		
		## show assignment lines
		if (type.legend.plot=="bar" && show.legend.text && xaxis==0) {
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
		if (show.legend.text) {
		    itemSize <- convertWidth(unit(1,"lines"), "inch", valueOnly=TRUE) * 0.5
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
					  y=yslines, just=c("left", "center"))
		}
		
	})
	
	# plot x axis tick marks
	if (type.legend.plot %in% c("hist", "bar")) cellplot(2,1,e={
		lineHeigth <- 1 - convertHeight(unit(axesMargin, "inch"), "npc", valueOnly=TRUE)
		if (xaxis==0) grid.lines(x=c(0,1), y=c(lineHeigth, lineHeigth))
		
		if (type.legend.plot=="bar" && show.legend.text && xaxis==0) {
			# plot vertical assignment lines
			grid.polyline(x=rep(x[id], each=2),
					   y=rep(c(0, lineHeigth), length(id)),
					   id=rep(1:length(id), each=2),
					   gp=gpar(col=grey(.3)))
		} else if (type.legend.plot=="hist") {
			grid.polyline(x=rep(xticks, each=2), y=rep(c(lineHeigth, lineHeigth-0.1), length(xticks)), 
						  id=rep(1:length(xticks), each=2)) 
			grid.text(ptx, x=xticks, y=lineHeigth-0.3, gp=gpar(cex=0.8))
		}
		
	})
		
	# plot chart
	if (type.legend.plot %in% c("hist", "bar")) cellplot(1,1,e={
		grid.rect(x=x, y=xaxis, width=ws, height=hs, gp=gpar(col=NA,fill=colors), just=c("left", "bottom"))
		
		# plot x axis if there are negative values; otherwise, plot it in cell2,1
		if (xaxis!=0) grid.lines(x=c(0,1), y=c(xaxis, xaxis))
	})
	
	if (type.legend.plot == "bubble") cellplot(1,1,e={
		
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
	if (type.legend.plot %in% c("hist", "bar")) cellplot(1,2,e={
		margin.npc <- convertWidth(unit(axesMargin, "inch"), "npc", valueOnly=TRUE)
		grid.polyline(x=c(margin.npc, margin.npc, 
						  rep(c(margin.npc,margin.npc+0.03), length(pty))),
					  y=c(0, 1, rep(xaxis+hpty, each=2)),
					  id=rep(1:(length(pty)+1),each=2))
		formatted <- format(pty, trim=TRUE)
		maxWidth <- max(convertUnit(stringWidth(formatted), unitTo="npc", valueOnly=TRUE))
		grid.text(formatted, x=rep(margin.npc+0.08+maxWidth, length(pty)), y=xaxis+hpty, 
				  just=c("right","center"), gp=gpar(cex=0.8))
	})
	
	
	upViewport(3)
	detach("gt")
}