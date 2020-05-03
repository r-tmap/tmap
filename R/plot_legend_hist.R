plot_legend_hist <- function(x, legend.hist.size, lineHeight, scale, m, attr.color, legend.hist.bg.color) {
	with(x, {
		if (is.factor(values)) {
			numbers <- table(values)
			xticks <- seq(0, 1, length.out=length(numbers)*2+1)[seq(2,length(numbers)*2,by=2)]
			ptx <- levels(values)
			
			#if (any(nchar(ptx)>5)) ptx <- substr(ptx, 1, 3)
			
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
			
			colors <- legend.palette[vapply(bins.mean, function(x) which(x<breaks)[1]-1L, integer(1))]
			
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
		
		width.npc <- max(text_width_npc(ptx)) * (length(ptx)+1)
		height.npc <- convertHeight(unit(length(formattedY)+2, "lines"), "npc", valueOnly=TRUE) 
		
		my <- lineHeight * legend.hist.size * m
		mx <- convertWidth(convertHeight(unit(my, "npc"), "inch"), "npc", TRUE)
		npcx.total <- 1-2*mx
		npcy.total <- 1-2*my
		
		## decrease factor
		text_shrink <- (npcx.total/width.npc)
		if (legend.hist.size/text_shrink>1.5) {
			draw_x_axis <- FALSE
			text_shrink <- Inf
		} else {
			draw_x_axis <- TRUE
		}
		
		
		#margin <- 0.05
		#npc.total <- 1-2*m
		
		size <- min(legend.hist.size,
					text_shrink,
					npcy.total/height.npc)
		
		width.npc <- width.npc * size
		height.npc <- height.npc * size
		
		
		width.yaxis <- max(text_width_npc(formattedY, space = FALSE)) * size
		height.xaxis <- lineHeight * size * ifelse(draw_x_axis, 1, .25)
		
		axisTicks <- convertWidth(unit(mx, "npc"), "inch", valueOnly=TRUE)
		
		mxInch <- convertWidth(unit(mx, "npc"), "inch", valueOnly=TRUE)

		
		height.xaxisInch <- convertHeight(unit(height.xaxis, "npc"), "inch", valueOnly=TRUE)
		
		vpHist <- viewport(layout=grid.layout(5, 5, 
											  heights=unit(c(my, 1, axisTicks, height.xaxis, my), c("npc", "null", "inch", "npc", "npc")),
											  widths=unit(c(width.yaxis, mx, axisTicks, 1, 3*mx), c("npc", "npc", "inch", "null", "npc"))))
		
		
		pushViewport(vpHist)
		
		histElems <- gList(
			if (!is.na(legend.hist.bg.color)) cellplot(2,4,e={
				rectGrob(gp=gpar(fill=legend.hist.bg.color, col=attr.color))
			}) else NULL,
			cellplot(2,4, e={
				rectGrob(x=x, y=0, width=ws, height=hs, gp=gpar(col=NA,fill=colors), just=c("left", "bottom"))
			}),
			# plot y axis
			cellplot(2,3,e={
				axisTicks.npc <- convertWidth(unit(axisTicks, "inch"), "npc", valueOnly=TRUE)
				
				
				polylineGrob(x=c(axisTicks.npc, axisTicks.npc, 
								  rep(c(0,axisTicks.npc), length(pty))),
							  y=c(0, 1, rep(hpty, each=2)),
							  id=rep(1:(length(pty)+1),each=2), gp=gpar(col=attr.color, lwd=scale))
			}),
			cellplot(2:4,1,e={
				maxWidth <- max(text_width_npc(formattedY, space=FALSE)) * size
				h_total <- convertHeight(unit(1, "npc"), "inch", valueOnly = TRUE)
				h_extra <- axisTicks+height.xaxisInch
				h_e <- h_extra / h_total
				hpty <- h_e + hpty * (1-h_e)
				textGrob(formattedY, x=maxWidth, y=hpty, 
						  just=c(1,.4), gp=gpar(col=attr.color, cex=size))
			}),
			# plot x axis tick marks
			cellplot(3,4,e={
				axisTicks.npc <- convertHeight(unit(axisTicks, "inch"), "npc", valueOnly=TRUE)
				
				n <- length(xticks)

				line_height <- convertHeight(unit(1, "lines"), "npc", valueOnly=TRUE) * size
				if (draw_x_axis) {
					
					gTree(children = gList(linesGrob(x=c(0,1), y=c(1, 1), gp=gpar(lwd=scale)),
										   polylineGrob(x=rep(xticks, each=2), y=rep(c(1, 1-axisTicks.npc), n), 
								  id=rep(1:n, each=2), gp=gpar(col=attr.color, lwd=scale)))) 
				} else linesGrob(x=c(0,1), y=c(1, 1), gp=gpar(col=attr.color, lwd=scale))
			}),
			if (draw_x_axis) cellplot(4,4:5,e={
 				w_total <- convertWidth(unit(1, "npc"), "inch", valueOnly = TRUE)
 				w_extra <- 3*mxInch
 				w_e <- w_extra / w_total
 				xticks <- xticks * (1-w_e)
				textGrob(ptx, x=xticks, y=.5, gp=gpar(col=attr.color, cex=size))
			}) else NULL)
		
		treeHist <- gTree(children=histElems, vp=vpHist)
		upViewport()

		list(treeHist, 1)
	})
}
