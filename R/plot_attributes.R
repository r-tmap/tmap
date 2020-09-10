plot_scale <- function(gt, just, xrange, crop_factor) {
	light <- do.call("process_color", c(list(gt$scale.color.light, alpha=1), gt$pc))
	dark <- do.call("process_color", c(list(gt$scale.color.dark, alpha=1), gt$pc))
	
	unit <- gt$shape.units$unit
	unit.size <- 1/gt$shape.units$to
	
	if (is.na(unit.size)) {
		if (gt$show.warnings) warning("Unable to determine shape coordinate units. Please check if the \"+units\" part of the projection is present. Otherwise, specify coords.unit or unit.size")
	} else if (!gt$shape.units$projected && ((gt$shape.bbx[4]-gt$shape.bbx[2]) > 30)) {
		if (gt$show.messages) message("Scale bar set for latitude ", gsub("long@lat(.+)$", "\\1", unit), " and will be different at the top and bottom of the map.")
	}
	
	xrange2 <- xrange/unit.size
	
	if (is.null(gt$scale.breaks)) {
		ticks2 <- pretty(c(0, xrange2*crop_factor), 4)
	} else {
		ticks2 <- gt$scale.breaks
	}
	ticks2Labels <- format(ticks2, trim=TRUE)
	ticksWidths <- text_width_npc(ticks2Labels)
	
	labels <- c(ticks2Labels, unit)
	
	n <- length(ticks2)
	ticks3 <- ticks2*unit.size / xrange
	
	widths <- ticks3[2:n] - ticks3[1:(n-1)]
	size <- min(gt$scale.text.size, widths/max(ticksWidths))
	x <- ticks3[1:(n-1)] + .5*ticksWidths[1]*size
	
	lineHeight <- convertHeight(unit(1, "lines"), "npc", valueOnly=TRUE) * size
	
	unitWidth <- text_width_npc(unit) * size
	width <- sum(widths[-n]) + .5*ticksWidths[1]*size + .5*ticksWidths[n]*size+ unitWidth   #widths * n 
	
	xtext <- x[1] + c(ticks3, ticks3[n] + .5*ticksWidths[n]*size + .5*unitWidth)# + widths*.5 + unitWidth*.5) #+ position[1]
	
	x <- just-just*width+x
	xtext <- just-just*width+xtext
	
	grobBG <- if (gt$design.mode) rectGrob(gp=gpar(fill="orange")) else NULL
	
	gTree(children=gList(
		grobBG,
		if (!is.na(gt$scale.bg.color)) {
			bg.col <- do.call("process_color", c(list(gt$scale.bg.color, alpha=gt$scale.bg.alpha), gt$pc))
			rectGrob(x=x[1]-unitWidth, width=xtext[n]-xtext[1]+2.5*unitWidth, just=c("left", "center"), gp=gpar(col=NA, fill=bg.col))
		} else {
			NULL
		}, rectGrob(x=x, y=1.5*lineHeight, width = widths, height=lineHeight*.5, just=c("left", "bottom"), gp=gpar(col=dark, fill=c(light, dark), lwd=gt$scale.lwd)),
		textGrob(label=labels, x = xtext, y = lineHeight, just=c("center", "center"), gp=gpar(col=gt$scale.text.color, cex=size, fontface=gt$fontface, fontfamily=gt$fontfamily))), name="scale_bar")
	
	
}

plot_logo <- function(gt, just, id) {
	lineHeight <- convertHeight(unit(1, "lines"), "npc", valueOnly=TRUE)
	
	
	grobBG <- if (gt$design.mode) rectGrob(gp=gpar(fill="orange")) else NULL
	
	files <- gt$logo.file[[id]]
	heights <- gt$logo.height[[id]]
	widths <- gt$logo.width[[id]]
	margin <- gt$logo.margin[id]
	halign <- gt$logo.halign[id]
	
	n <- length(files)
	xs <- c(0, cumsum(widths))[-(n+1)] + (1:n)*margin +  (widths / 2)
	
	# translate to number from 0 (bottom) to 1 (top)
	halign_num <- ifelse(is_num_string(halign), as.numeric(halign), ifelse(halign=="top", 1, ifelse(halign=="bottom", 0, 0.5)))
	
	# rescale to -1 to 1
	halign_num <- (halign_num-.5) * 2
	
	h <- convertHeight(unit(1, "npc"), "lines", valueOnly=TRUE)
	
	ys <- h/2 + (max(heights) - heights)/2 * halign_num
	
	
	heights_npc <- convertHeight(unit(heights, "lines"), "npc", valueOnly=TRUE)
	widths_npc <- convertWidth(unit(widths, "lines"), "npc", valueOnly=TRUE)
	xs_npc <- convertX(unit(xs, "lines"), "npc", valueOnly=TRUE)
	ys_npc <- convertY(unit(ys, "lines"), "npc", valueOnly=TRUE)
	
	heights_in <- convertHeight(unit(heights, "lines"), "inch", valueOnly=TRUE)
	widths_in <- convertHeight(unit(widths, "lines"), "inch", valueOnly=TRUE)
	
	
	grobsLogo <- do.call(gList, c(list(grobBG=grobBG), mapply(function(f, h, w, x, y, hin, win) {
		grobLogo <- pngGrob(f, fix.borders = T, n=2, height.inch=hin, target.dpi=96)
		
		rdim <- dim(grobLogo$raster)
		grobLogo$raster <- matrix(do.call("process_color", c(list(as.vector(grobLogo$raster), alpha=1), gt$pc)), nrow = rdim[1], ncol=rdim[2])
		# correct width to logo asp
		win2 <- hin * 1/do.call("/", as.list(rdim))
		w2 <- convertWidth(unit(win2, "inch"), "npc", valueOnly=TRUE)
		grobLogo$x <- unit(x, "npc")
		grobLogo$y <- unit(y, "npc")
		grobLogo$width <- unit(w2, "npc")
		grobLogo$height <- unit(h, "npc")
		grobLogo
	}, files, heights_npc, widths_npc, xs_npc, ys_npc, heights_in, widths_in, SIMPLIFY = FALSE)))
	
	
	
	gTree(children=grobsLogo)
	
}

plot_cred <- function(gt, just, id, width_scale) {
	lineHeight <- convertHeight(unit(1, "lines"), "npc", valueOnly=TRUE)
	
	my <- lineHeight / 2
	mx <- convertWidth(convertHeight(unit(my, "npc"), "inch"), "npc", TRUE)
	
	# number of lines
	txt <- gt$credits.text[id]
	nlines <- number_text_lines(txt)
	
	size <- min((1-2*mx) / text_width_npc(txt, space=FALSE), gt$credits.size[id])
	
	width =	if (is.na(gt$credits.width[id])) (text_width_npc(txt, space=FALSE)+1*mx) * size else gt$credits.width[id] * width_scale

	x <- just - just*width
	tx <- mx*.5*size + just - just*width
	
	if (gt$credits.align[id]=="center") {
		x <- x + width/2 + mx*.5*size
		tx <- tx + width/2
	} else if (gt$credits.align[id]=="right") {
		x <- x + width + mx*1*size
		tx <- tx + width
	}
	
	
	grobBG <- if (gt$design.mode) rectGrob(gp=gpar(fill="orange")) else NULL
	
	col <- do.call("process_color", c(list(gt$credits.col[id], alpha=gt$credits.alpha[id]), gt$pc))
	
	gTree(children=gList(grobBG,
						 if (!is.na(gt$credits.bg.color[id])) {
						 	bg.col <- do.call("process_color", c(list(gt$credits.bg.color[id], alpha=gt$credits.bg.alpha[id]), gt$pc))
						 	rectGrob(x=x, width=width, just=c(gt$credits.align[id], "center"), gp=gpar(col=NA, fill=bg.col))
						 } else {
						 	NULL
						 }, textGrob(label=txt, x = tx, y =.5, just=c(gt$credits.align[id], "center"), gp=gpar(cex=size, col=col, fontface=gt$credits.fontface[id], fontfamily=gt$credits.fontfamily[id]))), name="credits")
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
		LWD <- round(convertWidth(unit(.01, "npc"), "points", valueOnly=TRUE)) * gt$compass.lwd
		
		cd <- seq(1/8, 15/8, by=.25) * pi
		cd2 <- seq(1/4, 7/4, by=.5) * pi
		cd3 <- seq(0, 1.75, by=.25) * pi
		
		x <- list(.5,
				  unlist(lapply(.5 + sin(cd) * cr[1], c, .5), use.names = FALSE),
				  .5 + c(0, cr[1]-.005, 0, -cr[1]+.005, 0, 0, 0, 0),
				  unlist(lapply(.5 + sin(cd2) * cr[1], c, .5), use.names = FALSE),
				  .5 + unlist(mapply(c, sin(cd3) * cr[4], sin(cd3) * cr[5], SIMPLIFY=FALSE), use.names = FALSE))
		
		y <- list(.5,
				  unlist(lapply(.5 + cos(cd) * cr[1], c, .5), use.names = FALSE),
				  .5 + c(0, 0, 0, 0, 0, cr[1]-.005, 0, -cr[1]+.005),
				  unlist(lapply(.5 + cos(cd2) * cr[1], c, .5), use.names = FALSE),
				  .5 + unlist(mapply(c, cos(cd3) * cr[4], cos(cd3) * cr[5], SIMPLIFY=FALSE), use.names = FALSE))
		
	} else if (gt$compass.type=="rose") {
		cr <- c(.45, .42, .2, .17, .1)
		LWD <- convertWidth(unit(.01, "npc"), "points", valueOnly=TRUE) * gt$compass.lwd
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
				  unlist(lapply(.5 + sin(cd) * cr[1], c, .5), use.names = FALSE),
				  .5 + unlist(mapply(c, sin(cd3) * cr[4], sin(cd3) * cr[5], SIMPLIFY=FALSE), use.names = FALSE),
				  c(rep.int(s, 2), rep.int(s2, 2)))
		
		y <- list(.5,
				  unlist(lapply(.5 + cos(cd) * cr[1], c, .5), use.names = FALSE),
				  .5 + unlist(mapply(c, cos(cd3) * cr[4], cos(cd3) * cr[5], SIMPLIFY=FALSE), use.names = FALSE),
				  c(s[c(10:12, 10:12, 1:3, 1:3, 7:9, 7:9, 4:6, 4:6)], s2[c(4:6, 1:3, 10:12, 7:9, 10:12, 7:9, 4:6, 1:3)]))
		
	}
	
	
	# rescale
	resc <- function(a) (a-.5)*(gt$compass.size/gt$compass.nlines) + .5
	
	x <- lapply(x, resc)
	y <- lapply(y, resc)
	if (gt$compass.type %in% c("radar", "rose")) cr <- cr * (gt$compass.size/gt$compass.nlines)
	
	if (gt$compass.north!=0) {
		drotate <- gt$compass.north/180*pi - .5*pi
		
		xy <- mapply(function(a,b){
			d <- atan2(b-.5, a-.5)
			r <- sqrt((a-.5)^2 + (b-.5)^2)
			
			list(x=r * sin(d+drotate) + .5,
				 y=r * cos(d+drotate) + .5)
		}, x, y, SIMPLIFY=FALSE)
		x <- lapply(xy, "[[", 1)
		y <- lapply(xy, "[[", 2)
	} else drotate <- -.5*pi
	
	
	# shift compass to south direction
	if (gt$compass.show.labels==1) {
		x <- lapply(x, function(a) a - (u/2) * sin(drotate + .5*pi))
		y <- lapply(y, function(b) b - (u/2) * cos(drotate + .5*pi))
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
		
		lx <- lr * sin(ld+drotate + .5*pi) + .5
		ly <- lr * cos(ld+drotate + .5*pi) + .5
		textGrob(labels, x=lx, y=ly, just=c("center", "center"), rot=0, gp=gpar(col=gt$compass.text.color, cex=gt$compass.text.size, fontface=gt$fontface, fontfamily=gt$fontfamily)) # -drotate/pi*180 - 90
	}
	
	grobComp <- if (gt$compass.type %in% c("arrow", "4star", "8star")) {
		polygonGrob(x=x[[1]], y=y[[1]], id=id, gp=gpar(fill=fill, lwd=gt$compass.lwd, col=dark))
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
	
	
	gTree(children=gList(grobBG, 
						 if (!is.na(gt$compass.bg.color)) {
						 	bg.col <- do.call("process_color", c(list(gt$compass.bg.color, alpha=gt$compass.bg.alpha), gt$pc))
						 	rectGrob(gp=gpar(col=NA, fill=bg.col))
						 } else {
						 	NULL
						 },
						 grobComp, 
						 grobLabels), 
		  name="compass")
}

