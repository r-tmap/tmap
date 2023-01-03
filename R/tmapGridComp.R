#' @method tmapGridCompPrepare tm_title
#' @export
tmapGridCompPrepare.tm_title = function(comp, o) {
	comp
}

#' @method tmapGridCompHeight tm_title
#' @export
tmapGridCompHeight.tm_title = function(comp, o) {
	tmapGridCompHeight_text(comp, o)
}

#' @method tmapGridCompWidth tm_title
#' @export
tmapGridCompWidth.tm_title = function(comp, o) {
	tmapGridCompWidth_text(comp, o)
}

#' @method tmapGridLegPlot tm_title
#' @export
tmapGridLegPlot.tm_title = function(comp, o) {
	tmapGridLegPlot_text(comp, o)
}

#' @method tmapGridCompPrepare tm_credits
#' @export
tmapGridCompPrepare.tm_credits = function(comp, o) {
	comp
}

#' @method tmapGridCompHeight tm_credits
#' @export
tmapGridCompHeight.tm_credits = function(comp, o) {
	tmapGridCompHeight_text(comp, o)
}

#' @method tmapGridCompWidth tm_credits
#' @export
tmapGridCompWidth.tm_credits = function(comp, o) {
	tmapGridCompWidth_text(comp, o)
}

#' @method tmapGridLegPlot tm_credits
#' @export
tmapGridLegPlot.tm_credits = function(comp, o) {
	tmapGridLegPlot_text(comp, o)
}

#' @method tmapGridCompPrepare tm_compass
#' @export
tmapGridCompPrepare.tm_compass = function(comp, o) {
	o$attr.color.light <- is_light(o$attr.color)
	within(comp, {
		if (is.na(text.color)) text.color <- o$attr.color
		text.color <- do.call("process_color", c(list(col=text.color), o$pc))
		
		if (is.na(color.dark)) color.dark = ifelse(o$attr.color.light, o$attr.color, o$attr.color)
		if (is.na(color.light)) color.light = ifelse(o$attr.color.light, "black", "white")
		color.dark = do.call("process_color", c(list(col=color.dark), o$pc))
		color.light = do.call("process_color", c(list(col=color.light), o$pc))
		
		text.size = text.size * o$scale
		lwd = lwd * o$scale
		
		show = TRUE
		if (is.na(type)) type = o$type
		if (is.na(size)) size = switch(type, arrow=2, radar=6, rose=6, 4)
		nlines = size + ifelse(show.labels==0, 0, ifelse(show.labels==1, 1, 2))
	})
}

#' @method tmapGridCompHeight tm_compass
#' @export
tmapGridCompHeight.tm_compass = function(comp, o) {

	
	textS = comp$text.size #* o$scale
	#textP = comp$padding[c(3,1)] * textS * o$lin
	
	
	marH = comp$margins[c(3,1)] * textS * o$lin
	hs = c(marH[1], comp$nlines * textS * o$lin, marH[2])
	
	
	sides = switch(comp$position$align.v, top = "second", bottom = "first", "both")
	hsu = set_unit_with_stretch(hs, sides = sides)
	
	Hin = sum(hs)

	comp$Hin = Hin #  sum(textP[1], textH, textP[2])
	comp$hsu = hsu
	comp
}

#' @method tmapGridCompWidth tm_compass
#' @export
tmapGridCompWidth.tm_compass = function(comp, o) {
	
	
	
	textS = comp$text.size #* o$scale
	#textP = comp$padding[c(3,1)] * textS * o$lin
	
	marW = comp$margins[c(2,4)] * textS * o$lin
	ws = c(marW[1], comp$nlines * textS * o$lin, marW[2])

	sides = switch(comp$position$align.h, left = "second", right = "first", "both")
	wsu = set_unit_with_stretch(ws, sides = sides)

	comp$Win = sum(ws)
	comp$wsu = wsu
	comp
}

#' @method tmapGridLegPlot tm_compass
#' @export
tmapGridLegPlot.tm_compass = function(comp, o) {
	u <- 1/(comp$nlines)
	#vpComp <- viewport(x=u, y=u, height=1-2*u, width=1-2*u, just=c("left", "bottom"))
	
	light <- do.call("process_color", c(list(comp$color.light, alpha=1), o$pc))
	dark <- do.call("process_color", c(list(comp$color.dark, alpha=1), o$pc))
	
	
	wsu = comp$wsu
	hsu = comp$hsu
	
	vp = grid::viewport(layout = grid::grid.layout(ncol = length(wsu),
												   nrow = length(hsu), 
												   widths = wsu,
												   heights = hsu))
	
	
	if (comp$type=="4star") {
		s <- c(.5, .5, .57, .5, .5, .43, 0, .5, .43, 1, .5, .57)
		x <- list(rep.int(s, 2))
		y <- list(s[c(10:12, 10:12, 1:3, 1:3, 7:9, 7:9, 4:6, 4:6)])
		id <- rep(1:8, each=3)
		fill <- c(dark, light, dark, light, light, dark, light, dark)
	} else if (comp$type=="8star") {
		s <- c(.5, .5, .56, .5, .5, .44, 0, .5, .38, 1, .5, .62)
		s2 <- c(.5, .62, .7, .5, .56, .7, .5, .38, .3, .5, .44, .3)
		x <- list(c(rep.int(s, 2), rep.int(s2, 2)))
		y <- list(c(s[c(10:12, 10:12, 1:3, 1:3, 7:9, 7:9, 4:6, 4:6)], s2[c(4:6, 1:3, 10:12, 7:9, 10:12, 7:9, 4:6, 1:3)]))
		id <- rep(1:16, each=3)
		fill <- c(dark, light, dark, light, light, dark, light, dark)
	} else if (comp$type=="8star") {
		s <- c(.5, .5, .56, .5, .5, .44, 0, .5, .38, 1, .5, .62)
		s2 <- c(.5, .62, .7, .5, .56, .7, .5, .38, .3, .5, .44, .3)
		x <- list(c(rep.int(s, 2), rep.int(s2, 2)))
		y <- list(c(s[c(10:12, 10:12, 1:3, 1:3, 7:9, 7:9, 4:6, 4:6)], s2[c(4:6, 1:3, 10:12, 7:9, 10:12, 7:9, 4:6, 1:3)]))
		id <- rep(1:16, each=3)
		fill <- c(dark, light, dark, light, light, dark, light, dark)
	} else if (comp$type=="arrow") {
		x <- list(c(.5, .7, .5, .5, .3, .5))
		y <- list(c(1, 0, .2, 1, 0, .2))
		id <- rep(1:2, each=3)
		fill <- c(dark, light)
	} else if (comp$type=="radar") {
		cr <- c(.45, .42, .2, .17, .1)
		LWD <- round(convertWidth(unit(.01, "npc"), "points", valueOnly=TRUE)) * comp$lwd
		
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
		
	} else if (comp$type=="rose") {
		cr <- c(.45, .42, .2, .17, .1)
		LWD <- convertWidth(unit(.01, "npc"), "points", valueOnly=TRUE) * comp$lwd
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
	resc <- function(a) (a-.5)*(comp$size/comp$nlines) + .5
	
	x <- lapply(x, resc)
	y <- lapply(y, resc)
	if (comp$type %in% c("radar", "rose")) cr <- cr * (comp$size/comp$nlines)
	
	if (comp$north!=0) {
		drotate <- comp$north/180*pi - .5*pi
		
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
	if (comp$show.labels==1) {
		x <- lapply(x, function(a) a - (u/2) * sin(drotate + .5*pi))
		y <- lapply(y, function(b) b - (u/2) * cos(drotate + .5*pi))
	}
	
	
	
	grobBG <- if (getOption("tmap.design.mode")) rectGrob(gp=gpar(fill="orange")) else NULL
	
	grobLabels <- if (comp$show.labels==0) {
		NULL
	} else {
		selection <- if (comp$show.labels==1) {
			c(TRUE, rep.int(FALSE, 7))
		} else if (comp$show.labels==2) {
			rep.int(c(TRUE, FALSE), 4)
		} else rep.int(TRUE, 8)
		
		labels <- comp$cardinal.directions[c(1, 1, 2, 3, 3, 3, 4, 1)]
		labels[c(2,4,6,8)] <- paste(labels[c(2,4,6,8)], comp$cardinal.directions[c(2, 2, 4, 4)], sep="")
		labels <- labels[selection]
		
		lr <- (1-u)/2
		ld <- (seq(0, 1.75, by=.25) * pi)[selection]
		
		lx <- lr * sin(ld+drotate + .5*pi) + .5
		ly <- lr * cos(ld+drotate + .5*pi) + .5
		textGrob(labels, x=lx, y=ly, just=c("center", "center"), rot=0, gp=gpar(col=comp$text.color, cex=comp$text.size, fontface=o$text.fontface, fontfamily=o$text.fontfamily)) # -drotate/pi*180 - 90
	}
	
	grobComp <- if (comp$type %in% c("arrow", "4star", "8star")) {
		polygonGrob(x=x[[1]], y=y[[1]], id=id, gp=gpar(fill=fill, lwd=comp$lwd, col=dark))
	} else if (comp$type=="radar") {
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
	} else if (comp$type=="rose") {
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
	
	
	# other grid cells are aligns (1 and 5) and margins (2 and 4)
	compass = gridCell(3,3, {
		gTree(children=gList(grobBG, 
						 if (!is.na(comp$bg.color)) {
						 	bg.col <- do.call("process_color", c(list(comp$bg.color, alpha=comp$bg.alpha), o$pc))
						 	rectGrob(gp=gpar(col=NA, fill=bg.col))
						 } else {
						 	NULL
						 },
						 grobComp, 
						 grobLabels), 
		  name="compass")
	})
	
	grid::grobTree(compass, vp = vp)
}






tmapGridLegPlot_text = function(comp, o) {
	textS = if (comp$text == "") 0 else comp$size * comp$scale #* o$scale
	
	padding = grid::unit(comp$padding[c(3,4,1,2)] * textS * o$lin, units = "inch")
	
	if (comp$position$align.h == "left") {
		#x = grid::unit(0, "npc") 
		x = grid::unit(comp$padding[2] * textS * o$lin, units = "inch")
		halign = 0
		hjust = 1
		just = "left"
	} else if (comp$position$align.h == "right") {
		#x = grid::unit(1, "npc") 
		x = grid::unit(1, "npc") - grid::unit(comp$padding[4] * textS * o$lin, units = "inch")
		halign = 1
		hjust = 0
		just = "right"
	} else {
		x = grid::unit(0.5, "npc")
		halign = 0.5
		hjust = 0.5
		just = "center"
	}
	
	# grtext = gridtext::richtext_grob(comp$text, 
	# 								  x = x,
	# 								  box_gp = gpar(col = frame.col, fill = bg.color, alpha = bg.alpha, lwd = frame.lwd),
	# 								  r = grid::unit(frame.r, "pt"),
	# 								  halign = halign,
	# 								  hjust = hjust, 
	# 								  gp = grid::gpar(cex = textS))
	grtext = grid::textGrob(comp$text, 
							 x = x,
							 just = just,
							 gp = grid::gpar(col = comp$color, cex = textS))
	
	if (getOption("tmap.design.mode")) {
		grDesign = grid::rectGrob(gp=gpar(fill=NA,col="red", lwd=2))
	} else {
		grDesign = NULL
	}
	
	g = do.call(grid::grobTree, c(list(grtext), list(grDesign))) #, list(vp = vp)
	
	g
	
}

tmapGridCompHeight_text = function(comp, o) {
	textS = if (comp$text == "") 0 else comp$size #* o$scale
	textP = comp$padding[c(3,1)] * textS * o$lin
	textH = textS * o$lin
	comp$Hin = sum(textP[1], textH, textP[2])
	comp
}

tmapGridCompWidth_text = function(comp, o) {
	textS = if (comp$text == "") 0 else comp$size #* o$scale
	textP = comp$padding[c(2,4)] * textS * o$lin
	textW = textS * strwidth(comp$text, units = "inch", family = comp$fontfamily, font = fontface2nr(comp$fontface))
	comp$Win = sum(textP[1], textW, textP[2])
	comp
}





