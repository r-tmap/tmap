legend_hist <- function(x, legend.hist.size, lineHeight, scale) {
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
		
		width.npc <- max(convertWidth(stringWidth(ptx), unitTo="npc", valueOnly=TRUE)) * length(ptx)
		height.npc <- convertHeight(unit(length(formattedY)+2, "lines"), "npc", valueOnly=TRUE) 
		
		margin <- 0.05
		npc.total <- 1-2*margin
		
		size <- min(legend.hist.size,
				   npc.total/width.npc,
				   npc.total/height.npc)
		
		width.npc <- width.npc * size
		height.npc <- height.npc * size
		
		
		width.yaxis <- max(convertWidth(stringWidth(formattedY), unitTo="npc", valueOnly=TRUE)) * size * 1.5
		height.xaxis <- lineHeight * size
		
		axisMargin <- convertWidth(unit(0.02, "npc"), "inch", valueOnly=TRUE)
		axisTicks <- convertWidth(unit(0.01, "npc"), "inch", valueOnly=TRUE)
		
		vpHist <- viewport(layout=grid.layout(5, 5, 
											  heights=unit(c(margin, 1, axisMargin+axisTicks, height.xaxis, margin), c("npc", "null", "inch", "npc", "npc")),
											  widths=unit(c(margin, 1, axisMargin+axisTicks, width.yaxis, margin), c("npc", "null", "inch", "npc", "npc"))))
		
		
		pushViewport(vpHist)
		
		histElems <- gList(
			cellplot2(2,2, e={
				rectGrob(x=x, y=0, width=ws, height=hs, gp=gpar(col=NA,fill=colors), just=c("left", "bottom"))
			}),
			# plot y axis
			cellplot2(2,3,e={
				axisMargin.npc <- convertWidth(unit(axisMargin, "inch"), "npc", valueOnly=TRUE)
				axisTicks.npc <- convertWidth(unit(axisTicks, "inch"), "npc", valueOnly=TRUE)
				
				
				polylineGrob(x=c(axisMargin.npc, axisMargin.npc, 
								  rep(c(axisMargin.npc,axisMargin.npc+axisTicks.npc), length(pty))),
							  y=c(0, 1, rep(hpty, each=2)),
							  id=rep(1:(length(pty)+1),each=2), gp=gpar(lwd=scale))
			}),
			cellplot2(2:3,4,e={
				maxWidth <- max(convertWidth(stringWidth(formattedY), unitTo="npc", valueOnly=TRUE)) * size * 1.5
				h_total <- convertHeight(unit(1, "npc"), "inch", valueOnly = TRUE)
				h_extra <- axisMargin+axisTicks
				h_e <- h_extra / h_total
				hpty <- h_e + hpty * (1-h_e)
				textGrob(formattedY, x=maxWidth, y=hpty, 
						  just=c("right","center"), gp=gpar(cex=size))
			}),
			# plot x axis tick marks
			cellplot2(3,2,e={
				axisMargin.npc <- convertHeight(unit(axisMargin, "inch"), "npc", valueOnly=TRUE)
				axisTicks.npc <- convertHeight(unit(axisTicks, "inch"), "npc", valueOnly=TRUE)
				
				n <- length(xticks)
				
				line_height <- convertHeight(unit(1, "lines"), "npc", valueOnly=TRUE) * size
				gTree(children = gList(linesGrob(x=c(0,1), y=c(1-axisMargin.npc, 1-axisMargin.npc), gp=gpar(lwd=scale)),
									   polylineGrob(x=rep(xticks, each=2), y=rep(c(1-axisMargin.npc, 1-axisMargin.npc-axisTicks.npc), n), 
							  id=rep(1:n, each=2), gp=gpar(lwd=scale)))) 
			}),
			cellplot2(4,2,e={
				textGrob(ptx, x=xticks, y=.5, gp=gpar(cex=size))
			}))
		
		treeHist <- gTree(children=histElems, vp=vpHist)
		upViewport()

		treeHist
	})
}
