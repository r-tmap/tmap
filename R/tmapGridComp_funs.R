tmapGridCompPrepare_text = function(comp, o) {
	if (is.null(comp$text)) {
		cmp = within(comp, {
			show = FALSE
		})
		return(cmp)
	}
	n = length(comp$text)
	lst = lapply(seq_len(n), function(i){
		within(comp, {
			color[is.na(color)] = o$attr.color
			color = do.call("process_color", c(list(col=color), o$pc))
			size = size * o$scale
			if (!is.numeric(fontface)) fontface[is.na(fontface)] = o$text.fontface
			fontfamily[is.na(fontfamily)] =o$text.fontfamily
			#text = lapply(text, rep, length.out=o$n)
			text = text[i]
			if (is.na(text)) text = ""
			show = nonempty_text(text)
		})
	})
	class(lst) = c("tm_multi_comp", "list")
	lst
}


#' @export
tmapGridCompPrepare.tm_title = function(comp, o) {
	tmapGridCompPrepare_text(comp, o)
}

#' @export
tmapGridCompHeight.tm_title = function(comp, o) {
	tmapGridCompHeight_text(comp, o)
}

#' @export
tmapGridCompWidth.tm_title = function(comp, o) {
	tmapGridCompWidth_text(comp, o)
}

#' @export
tmapGridCompPlot.tm_title = function(comp, o, fH, fW) {
	tmapGridCompPlot_text(comp, o)
}

#' @export
tmapGridCompPrepare.tm_credits = function(comp, o) {
	tmapGridCompPrepare_text(comp, o)
}

#' @export
tmapGridCompHeight.tm_credits = function(comp, o) {
	tmapGridCompHeight_text(comp, o)
}

#' @export
tmapGridCompWidth.tm_credits = function(comp, o) {
	tmapGridCompWidth_text(comp, o)
}

#' @export
tmapGridCompPlot.tm_credits = function(comp, o, fH, fW) {
	tmapGridCompPlot_text(comp, o, fH, fW)
}

#' @export
tmapGridCompPrepare.tm_compass = function(comp, o) {
	typs = c("arrow", "4star", "8star", "radar", "rose")
	within(comp, {

		if (!(type %in% typs)) cli::cli_abort("{.field [tm_compass]} wrong compass type, should be one of: {.str {typs}}")
		text.size = text.size * o$scale

		asp = if (type == "arrow") 0.5 else 1

		show = TRUE
		if (is.na(type)) type = o$type
		if (is.na(size)) size = switch(type, arrow=2, radar=6, rose=6, 4)
		nlines = size + ifelse(show.labels==0, 0, ifelse(show.labels==1, 1, 2))
	})
}

#' @export
tmapGridCompHeight.tm_compass = function(comp, o) {


	textS = comp$text.size #* o$scale
	#textP = comp$padding[c(3,1)] * textS * o$lin

	marH = comp$margins[c(3,1)] * textS * o$lin
	hs = c(marH[1], comp$nlines * textS * o$lin, marH[2])


	sides = switch(comp$position$align.v, top = "second", bottom = "first", "both")
	hsu = set_unit_with_stretch(hs, sides = sides)

	Hin = sum(hs)
	comp$flexRow = NA
	comp$Hin = Hin #  sum(textP[1], textH, textP[2])
	comp$hsu = hsu
	comp
}

#' @export
tmapGridCompWidth.tm_compass = function(comp, o) {



	textS = comp$text.size #* o$scale
	#textP = comp$padding[c(3,1)] * textS * o$lin


	marW = comp$margins[c(2,4)] * textS * o$lin
	ws = c(marW[1], comp$nlines * textS * o$lin * comp$asp, marW[2])

	sides = switch(comp$position$align.h, left = "second", right = "first", "both")
	wsu = set_unit_with_stretch(ws, sides = sides)

	comp$flexCol = NA
	comp$Win = sum(ws)
	comp$wsu = wsu
	comp
}

#' @export
tmapGridCompPlot.tm_compass = function(comp, o, fH, fW) {

	u = 1/(comp$nlines)
	#vpComp = viewport(x=u, y=u, height=1-2*u, width=1-2*u, just=c("left", "bottom"))

	light = comp$color.light
	dark = comp$color.dark


	wsu = comp$wsu
	hsu = comp$hsu

	vp = grid::viewport(layout = grid::grid.layout(ncol = length(wsu),
												   nrow = length(hsu),
												   widths = wsu,
												   heights = hsu))


	if (comp$type=="4star") {
		s = c(.5, .5, .57, .5, .5, .43, 0, .5, .43, 1, .5, .57)
		x = list(rep.int(s, 2))
		y = list(s[c(10:12, 10:12, 1:3, 1:3, 7:9, 7:9, 4:6, 4:6)])
		id = rep(1:8, each=3)
		fill = c(dark, light, dark, light, light, dark, light, dark)
	} else if (comp$type=="8star") {
		s = c(.5, .5, .56, .5, .5, .44, 0, .5, .38, 1, .5, .62)
		s2 = c(.5, .62, .7, .5, .56, .7, .5, .38, .3, .5, .44, .3)
		x = list(c(rep.int(s, 2), rep.int(s2, 2)))
		y = list(c(s[c(10:12, 10:12, 1:3, 1:3, 7:9, 7:9, 4:6, 4:6)], s2[c(4:6, 1:3, 10:12, 7:9, 10:12, 7:9, 4:6, 1:3)]))
		id = rep(1:16, each=3)
		fill = c(dark, light, dark, light, light, dark, light, dark)
	} else if (comp$type=="arrow") {
		x = list(c(.5, .9, .5, .5, .1, .5))
		y = list(c(1, 0, .2, 1, 0, .2))
		id = rep(1:2, each=3)
		fill = c(dark, light)
	} else if (comp$type=="radar") {
		cr = c(.45, .42, .2, .17, .1)
		LWD = (o$lineH * 24) * comp$lwd

		cd = seq(1/8, 15/8, by=.25) * pi
		cd2 = seq(1/4, 7/4, by=.5) * pi
		cd3 = seq(0, 1.75, by=.25) * pi

		x = list(.5,
				  unlist(lapply(.5 + sin(cd) * cr[1], c, .5), use.names = FALSE),
				  .5 + c(0, cr[1]-.005, 0, -cr[1]+.005, 0, 0, 0, 0),
				  unlist(lapply(.5 + sin(cd2) * cr[1], c, .5), use.names = FALSE),
				  .5 + unlist(mapply(c, sin(cd3) * cr[4], sin(cd3) * cr[5], SIMPLIFY=FALSE), use.names = FALSE))

		y = list(.5,
				  unlist(lapply(.5 + cos(cd) * cr[1], c, .5), use.names = FALSE),
				  .5 + c(0, 0, 0, 0, 0, cr[1]-.005, 0, -cr[1]+.005),
				  unlist(lapply(.5 + cos(cd2) * cr[1], c, .5), use.names = FALSE),
				  .5 + unlist(mapply(c, cos(cd3) * cr[4], cos(cd3) * cr[5], SIMPLIFY=FALSE), use.names = FALSE))

	} else if (comp$type=="rose") {
		cr = c(.45, .42, .2, .17, .1)
		LWD = (o$lineH * 24) * comp$lwd
		cd = seq(1/8, 15/8, by=.25) * pi
		cd2 = seq(1/4, 7/4, by=.5) * pi
		cd3 = seq(0, 1.75, by=.25) * pi

		b = cr[4]
		a = 0.4142136 * b # 1/16th circleL
		s = c(.5, .5, .5+a, .5, .5, .5-a, 0, .5, .5-b, 1, .5, .5+b)
		s2 = c(.5, .5+b, .78, .5, .5+a, .78, .5, .5-b, .22, .5, .5-a, .22)

		id = rep(1:16, each=3)
		fill = c(dark, light, dark, light, light, dark, light, dark)


		x = list(.5,
				  unlist(lapply(.5 + sin(cd) * cr[1], c, .5), use.names = FALSE),
				  .5 + unlist(mapply(c, sin(cd3) * cr[4], sin(cd3) * cr[5], SIMPLIFY=FALSE), use.names = FALSE),
				  c(rep.int(s, 2), rep.int(s2, 2)))

		y = list(.5,
				  unlist(lapply(.5 + cos(cd) * cr[1], c, .5), use.names = FALSE),
				  .5 + unlist(mapply(c, cos(cd3) * cr[4], cos(cd3) * cr[5], SIMPLIFY=FALSE), use.names = FALSE),
				  c(s[c(10:12, 10:12, 1:3, 1:3, 7:9, 7:9, 4:6, 4:6)], s2[c(4:6, 1:3, 10:12, 7:9, 10:12, 7:9, 4:6, 1:3)]))

	}


	# rescale
	resc = function(a) (a-.5)*(comp$size/comp$nlines) + .5

	x = lapply(x, resc)
	y = lapply(y, resc)
	if (comp$type %in% c("radar", "rose")) cr = cr * (comp$size/comp$nlines)

	if (comp$north!=0) {
		drotate = comp$north/180*pi - .5*pi

		xy = mapply(function(a,b){
			d = atan2(b-.5, a-.5)
			r = sqrt((a-.5)^2 + (b-.5)^2)

			list(x=r * sin(d+drotate) + .5,
				 y=r * cos(d+drotate) + .5)
		}, x, y, SIMPLIFY=FALSE)
		x = lapply(xy, "[[", 1)
		y = lapply(xy, "[[", 2)
	} else drotate = -.5*pi


	# shift compass to south direction
	if (comp$show.labels==1) {
		x = lapply(x, function(a) a - (u/2) * sin(drotate + .5*pi))
		y = lapply(y, function(b) b - (u/2) * cos(drotate + .5*pi))
	}



	grobBG = if (getOption("tmap.design.mode")) rectGrob(gp=gpar(fill="#CAB2D6")) else NULL

	grobLabels = if (comp$show.labels==0) {
		NULL
	} else {
		selection = if (comp$show.labels==1) {
			c(TRUE, rep.int(FALSE, 7))
		} else if (comp$show.labels==2) {
			rep.int(c(TRUE, FALSE), 4)
		} else rep.int(TRUE, 8)

		labels = comp$cardinal.directions[c(1, 1, 2, 3, 3, 3, 4, 1)]
		labels[c(2,4,6,8)] = paste(labels[c(2,4,6,8)], comp$cardinal.directions[c(2, 2, 4, 4)], sep="")
		labels = labels[selection]

		lr = (1-u)/2
		ld = (seq(0, 1.75, by=.25) * pi)[selection]

		lx = lr * sin(ld+drotate + .5*pi) + .5
		ly = lr * cos(ld+drotate + .5*pi) + .5
		textGrob(labels, x=lx, y=ly, just=c("center", "center"), rot=0, gp=gpar(col=comp$text.color, cex=comp$text.size, fontface=o$text.fontface, fontfamily=o$text.fontfamily)) # -drotate/pi*180 - 90
	}

	grobComp = if (comp$type %in% c("arrow", "4star", "8star")) {
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
			polygonGrob(x=x[[4]], y=y[[4]], id=id, gp=gpar(lwd=1*LWD, col=dark, fill=fill)),
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
						 grobComp,
						 grobLabels),
		  name="compass")
	})
	grid::grobTree(compass, vp = vp)
}






#' @export
tmapGridCompPrepare.tm_scalebar = function(comp, o) {
	show.messages = o$show.messages
	show.warnings = o$show.warnings
	within(comp, {
		if (all(c("breaks", "width") %in% call) && show.warnings) {
			message("For 'tm_scalebar()', 'breaks' and 'width' are not supposed to be used together; normally, setting the exact width is not needed when breaks have been specified.", call. = FALSE)
		}
		if ("breaks" %in% call) {
			if (breaks[1] != 0) {
				if (show.warnings) warning("First scalebar breaks value should be 0.", call. = FALSE)
				breaks = c(0, breaks)
			}
		}

		# if (is.na(width))
		# 	width = .25
		# else if (width > 1) {
		# 	if (show.messages) message("Scale bar width set to 0.25 of the map width")
		# 	width = .25
		# }

		if (is.na(text.color)) text.color = o$attr.color
		text.size = text.size * o$scale
		lwd = lwd * o$scale
		show = TRUE
	})
}

#' @export
tmapGridCompHeight.tm_scalebar = function(comp, o) {
	h = 2.75 * o$lin * comp$text.size

	textS = comp$text.size #* o$scale
	#textP = comp$padding[c(3,1)] * textS * o$lin


	marH = comp$margins[c(3,1)] * textS * o$lin
	hs = c(marH[1], h, marH[2])


	sides = switch(comp$position$align.v, top = "second", bottom = "first", "both")
	hsu = set_unit_with_stretch(hs, sides = sides)

	Hin = sum(hs)
	comp$flexRow = NA
	comp$Hin = Hin #  sum(textP[1], textH, textP[2])
	comp$hsu = hsu
	comp
}

#' @export
tmapGridCompWidth.tm_scalebar = function(comp, o) {
	#w = comp$width * o$lin * comp$text.size

	textS = comp$text.size #* o$scale
	#textP = comp$padding[c(3,1)] * textS * o$lin

	marW = comp$margins[c(2,4)] * textS * o$lin

	W = comp$width * textS * o$lin

	ws = c(marW[1], W, marW[2])

	sides = switch(comp$position$align.h, left = "second", right = "first", "both")
	wsu = set_unit_with_stretch(ws, sides = sides)


	comp$Win = sum(ws)
	comp$wsu = wsu

	# in case breaks are used: adjust the legend width later (in tmapGridComp)
	comp$WnativeID = 3
	if (!is.null(comp$breaks)) {
		comp$WnativeRange = tail(comp$breaks, 1) - comp$breaks[1]# + (comp$breaks[2] - comp$breaks[1]) * 2
		#comp$Wextra_text_inch = text_width_inch(paste0("  ", cmp$units$unit)) + text_width_inch(paste0(tail(comp$breaks, 1), comp$breaks[1])) / 2
	}

	comp
}

#' @export
tmapGridCompPlot.tm_scalebar = function(comp, o, fH, fW) {
	light = comp$color.light
	dark = comp$color.dark


	wsu = comp$wsu
	hsu = comp$hsu

	vp = grid::viewport(layout = grid::grid.layout(ncol = length(wsu),
												   nrow = length(hsu),
												   widths = wsu,
												   heights = hsu))


	unit = comp$units$unit
	unit.size = 1/comp$units$to

	xrange = fW * comp$cpi

	# xrange is the range of the viewport in terms of coordinates
	# xrange2 is the same but with units (e.g. km instead of m)
	# W is the targeted space for the scalebar


	W = as.numeric(wsu[3])

	crop_factor = W / fW
	just = 0

	if (is.na(unit.size)) {
		if (o$show.warnings) warning("Unable to determine shape coordinate units. Please check if the \"+units\" part of the projection is present. Otherwise, specify coords.unit or unit.size")
	} else if (!comp$units$projected && ((comp$bbox[4]-comp$bbox[2]) > 30)) {
		if (o$show.messages) message("Scale bar set for latitude ", gsub("long@lat(.+)$", "\\1", unit), " and will be different at the top and bottom of the map.")
	}

	xrange2 = xrange/unit.size


	if (is.null(comp$breaks)) {
		# determine resolution only (unselect steps that do not fit later (with 'sel'))
		for (i in 10:1) {
			tcks = pretty(c(0, xrange2*crop_factor), i)
			tcks3 = (tcks / xrange2) * fW
			tcksL = format(tcks, trim=TRUE)
			labW = text_width_inch(tcksL) * comp$text.size
			tickW = tcks3[-1] - head(tcks3, -1)
			if (all(tickW > labW[-1])) {
				sbW = W - labW
				break
			}
		}
		ticks2 = tcks
	} else {
		ticks2 = comp$breaks
		tcksL = format(ticks2, trim=TRUE)

		labW = text_width_inch(tcksL) * comp$text.size
		sbW = W - labW
	}

	ticks3 = ticks2 / xrange2 * fW
	sel = which(ticks3 <= sbW)

	if (!is.null(comp$breaks) && length(sel) != length(ticks3)) {
		warning("Not all scale bar breaks could be plotted. Try increasing the scale bar width or descreasing the font size", call. = FALSE)
	}

	ticks3 = ticks3[sel]
	ticks2 = ticks2[sel]

	ticks2Labels = format(ticks2, trim=TRUE)
	ticksWidths = text_width_inch(ticks2Labels)
	unitWidth = text_width_inch(unit)

	labels = c(ticks2Labels, unit)
	labelsW = c(ticksWidths, unitWidth)

	n = length(ticks2)

	widths = ticks3[2:n] - ticks3[1:(n-1)]
	size = min(comp$text.size, widths/max(ticksWidths))
	x = ticks3[1:(n-1)]  + ticksWidths[1]*size #+ .5*widths[1]

	lineHeight = convertHeight(unit(1, "lines"), "inch", valueOnly=TRUE) * size

	unitWidth = text_width_inch(unit) * size

	xtext = x[1] + c(ticks3, ticks3[n] + .5*ticksWidths[n]*size + .5*unitWidth)# + widths*.5 + unitWidth*.5) #+ position[1]


	# if "unit" text is clipped, remove last label and move unit to previous label
	if (!comp$allow_clipping) {
		xright = xtext + labelsW / 2
		if (tail(xright, 1) > W) {
			labels = c(head(labels, -2), unit)
			xtext = x[1] + c(head(ticks3, -1), ticks3[n-1] + .5*ticksWidths[n-1]*size + .5*unitWidth)# + widths*.5 + unitWidth*.5) #+ position[1]
		}
	}


	grobBG = if (getOption("tmap.design.mode")) rectGrob(gp=gpar(fill="#CAB2D6")) else NULL


	# other grid cells are aligns (1 and 5) and margins (2 and 4)
	scalebar = gridCell(3,3, {
		gTree(children=gList(

			grobBG,
			rectGrob(x=unit(x, "inch"), y=unit(1.5*lineHeight, "inch"), width = unit(widths, "inch"), height=unit(lineHeight*.5, "inch"), just=c("left", "bottom"), gp=gpar(col=dark, fill=c(light, dark), lwd=comp$lwd)),
			textGrob(label=labels, x = unit(xtext, "inch"), y = unit(lineHeight, "inch"), just=c("center", "center"), gp=gpar(col=comp$text.color, cex=size, fontface=comp$text.fontface, fontfamily=comp$text.fontfamily))
			), name="scalebar")
	})

	grid::grobTree(scalebar, vp = vp)
	#scalebar
}










tmapGridCompPlot_text = function(comp, o, fH, fW) {

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
							 gp = grid::gpar(col = comp$color, cex = textS, fontface = comp$fontface, fontfamily = comp$fontfamily, alpha = comp$alpha))

	if (getOption("tmap.design.mode")) {
		grDesign = grid::rectGrob(gp=gpar(fill=NA,col="#000000", lwd=1))
	} else {
		grDesign = NULL
	}

	#grtext = rectGrob(gp=gpar(col = "green", fill= NA))


	g = do.call(grid::grobTree, c(list(grtext), list(grDesign))) #, list(vp = vp)

	g

}


correct_nlines = function(n) {
	# Linear model applied based on this empirical data:
	# (results may depend on device)
	#
	# y = sapply(1:50, function(i) {
	# 	s = paste(rep("text", i), collapse = "\n")
	# 	convertHeight(stringHeight(s), "inch", valueOnly = TRUE)
	# })
	# df = data.frame(x = 1:50, y = y / 0.2) #0.2 is the lineheight (par "cin")
	# lm(y~x, df)
	#
	-.6035 + n * 1.2
}


tmapGridCompHeight_text = function(comp, o) {
	textS = if (comp$text == "") 0 else comp$size #* o$scale
	textP = comp$padding[c(3,1)] * textS * o$lin
	textH = textS * o$lin

	nlines = number_text_lines(comp$text)

	nlines2 = correct_nlines(nlines)
	comp$flexRow = NA

	hs = c(textP[1], textH * nlines2, textP[2])
	h = sum(hs)

	sides = switch(comp$position$align.v, top = "second", bottom = "first", "both")
	hsu = set_unit_with_stretch(hs, sides = sides)

	comp$Hin = h
	comp$hsu = hsu
	comp
}

# borrowed from treemap (wraps text to 1-5 lines)
wrapText = function(txt, nlines) {
	if (nlines == 1) {
		txt
	} else {
		# create some wrappings, with slightly different widths:
		results <- lapply(1:5, FUN=function(pos, nlines, txt) {
			strwrap(txt, width = pos+(nchar(txt)/nlines))}, nlines, txt)
		lengths = sapply(results, length)

		# find the best match
		diff = nlines - lengths
		diff[diff < 0] = 1000
		id = which.min(diff)[1]

		paste(results[[id]], collapse = "\n")
	}
}




tmapGridCompWidth_text = function(comp, o) {
	textS = if (comp$text == "") 0 else comp$size #* o$scale
	textP = comp$padding[c(2,4)] * textS * o$lin
	textW = textS * graphics::strwidth(comp$text, units = "inch", family = comp$fontfamily, font = fontface2nr(comp$fontface))


	if (!is.na(comp$width)) {
		textPgs = strsplit(comp$text, "\n")[[1]]
		text2 = do.call(paste, c(lapply(textPgs, function(p) {
			textW = textS * graphics::strwidth(p, units = "inch", family = comp$fontfamily, font = fontface2nr(comp$fontface))
			w = sum(textP[1], textW, textP[2])
			nlines = round(w / (comp$width * textS * o$lin))
			wrapText(p, nlines)
		}), list(sep = "\n")))

		textW2 = textS * graphics::strwidth(text2, units = "inch", family = comp$fontfamily, font = fontface2nr(comp$fontface))
		wsu2 = c(textP[1], textW2, textP[2])
		ws = sum(textP[1], textW2, textP[2])
		comp$text = text2
	} else {
		ws = c(textP[1], textW, textP[2])
	}


	sides = switch(comp$position$align.h, left = "second", right = "first", "both")
	wsu = set_unit_with_stretch(ws, sides = sides)

	comp$Win = sum(ws)
	comp$wsu = wsu

	comp$flexCol = NA
	comp
}






#' @export
tmapGridCompPrepare.tm_logo = function(comp, o) {
	comp$logo = lapply(comp$file, function(lf){
		tmap_icons(lf)
	})
	comp$asp = vapply(comp$logo, function(lg) {
		lg$iconWidth / lg$iconHeight
	}, FUN.VALUE = numeric(1))
	comp$show = TRUE
	comp
}

#' @export
tmapGridCompHeight.tm_logo = function(comp, o) {
	marH = comp$margins[c(3,1)] * o$lin
	hs = c(marH[1], comp$height * o$lin, marH[2])

	sides = switch(comp$position$align.v, top = "second", bottom = "first", "both")
	hsu = set_unit_with_stretch(hs, sides = sides)

	Hin = sum(hs)
	comp$flexRow = NA
	comp$Hin = Hin #  sum(textP[1], textH, textP[2])
	comp$hsu = hsu
	comp
}

#' @export
tmapGridCompWidth.tm_logo = function(comp, o) {
	k = length(comp$asp)
	comp$width = comp$height * comp$asp

	marW = comp$margins[c(2,4)] * o$lin
	ws = c(marW[1],
		   comp$width[1] * o$lin,
		   {if (k > 1) unlist(lapply(comp$width[-1], function(w) c(comp$between_margin, w) * o$lin)) else NULL},
		   marW[2])

	sides = switch(comp$position$align.h, left = "second", right = "first", "both")
	wsu = set_unit_with_stretch(ws, sides = sides)
	comp$col_ids = seq(3L, by = 2L, length.out = k)
	comp$flexCol = NA
	comp$Win = sum(ws)
	comp$wsu = wsu
	comp
}

#' @export
tmapGridCompPlot.tm_logo = function(comp, o, fH, fW) {

	k = length(comp$logo)

	wsu = comp$wsu
	hsu = comp$hsu

	vp = grid::viewport(layout = grid::grid.layout(ncol = length(wsu),
												   nrow = length(hsu),
												   widths = wsu,
												   heights = hsu))
	gLogos = mapply(function(logo, col) {
		grobLogo = pngGrob(logo$iconUrl, fix.borders = TRUE, n=2, height.inch=as.numeric(comp$hsu[3]), target.dpi=96)
		rdim = dim(grobLogo$raster)
		grobLogo$raster = matrix(do.call("process_color", c(list(as.vector(grobLogo$raster)), o$pc)), nrow = rdim[1], ncol=rdim[2])
		gridCell(3L, col, grobLogo)
	}, comp$logo, comp$col_ids, SIMPLIFY = FALSE)

	grobBG = if (getOption("tmap.design.mode")) rectGrob(gp=gpar(fill="#CAB2D6")) else NULL
	gBG = gridCell(3L, 3L:(length(wsu) - 2L), grobBG)

	do.call(grid::grobTree, c(list(gBG), gLogos, list(vp = vp)))

}


#' @export
tmapGridCompPrepare.tm_inset_tmap = function(comp, o) {
	asp = comp$width / comp$height

	comp$x = tmap_grob(comp$x, asp = asp)
	.TMAP$is_first_inset = FALSE
	class(comp)[1] = "tm_inset_grob"

	b = .TMAP$geo_ref$bbx

	comp$bbox = .TMAP$geo_ref$bbx_frame
	comp$show = TRUE
	comp
}


#' @export
tmapGridCompPrepare.tm_inset_grob = function(comp, o) {
	comp$show = TRUE
	comp
}

#' @export
tmapGridCompPrepare.tm_inset_gg = function(comp, o) {
	rlang::check_installed("ggplot2", reason = "for plotting ggplot2 charts")
	comp$x = ggplot2::ggplotGrob(comp$x)
	class(comp)[1] = "tm_inset_grob"
	comp$show = TRUE
	comp
}



#' @export
tmapGridCompHeight.tm_inset_grob = function(comp, o) {
	marH = comp$margins[c(3,1)] * o$lin
	hs = c(marH[1], comp$height * o$lin, marH[2])

	sides = switch(comp$position$align.v, top = "second", bottom = "first", "both")
	hsu = set_unit_with_stretch(hs, sides = sides)

	Hin = sum(hs)
	comp$flexRow = NA
	comp$Hin = Hin #  sum(textP[1], textH, textP[2])
	comp$hsu = hsu
	comp
}

#' @export
tmapGridCompWidth.tm_inset_grob = function(comp, o) {
	marW = comp$margins[c(2,4)] * o$lin

	ws = c(marW[1], comp$width * o$lin, marW[2])

	sides = switch(comp$position$align.h, left = "second", right = "first", "both")
	wsu = set_unit_with_stretch(ws, sides = sides)

	comp$flexCol = NA
	comp$Win = sum(ws)
	comp$wsu = wsu
	comp
}

#' @export
tmapGridCompPrepare.tm_inset_image = function(comp, o) {
	comp$file = comp$x
	comp = tmapGridCompPrepare.tm_logo(comp, o)
	class(comp) = c("tm_logo", class(comp))
	comp
}



#' @export
tmapGridCompPlot.tm_inset_grob = function(comp, o, fH, fW) {


	wsu = comp$wsu
	hsu = comp$hsu

	vp = grid::viewport(layout = grid::grid.layout(ncol = length(wsu),
												   nrow = length(hsu),
												   widths = wsu,
												   heights = hsu))

	grobBG = if (getOption("tmap.design.mode")) rectGrob(gp=gpar(fill="#CAB2D6")) else NULL
	gBG = gridCell(3L, 3L:(length(wsu) - 2L), grobBG)

	g = gridCell(3L, 3L, comp$x)

	do.call(grid::grobTree, c(list(gBG, g), list(vp = vp)))

}



#' @export
tmapGridCompPrepare.tm_inset_map = function(comp, o) {
	limit_lat = if ("limit_latitude_3857" %in% names(o)) o$limit_latitude_3857 else c(-90, 90)

	if (is.null(comp$x)) {
		comp$x = sf::st_bbox(c(xmin = -180, xmax = 180, ymin = limit_lat[1], ymax = limit_lat[2]), crs = 4326)
		comp$crs = tmap_options()$crs_global
	}

	bbpoly = sf::st_transform(tmaptools::bb_poly(comp$x), o$crs_step4)

	b = tmaptools::bb(bbpoly, asp.target = comp$width / comp$height)

	comp$bbox = b
	comp$show = TRUE
	comp
}

#' @export
tmapGridCompHeight.tm_inset_map = function(comp, o) {
	marH = comp$margins[c(3,1)] * o$lin
	hs = c(marH[1], comp$height * o$lin, marH[2])

	sides = switch(comp$position$align.v, top = "second", bottom = "first", "both")
	hsu = set_unit_with_stretch(hs, sides = sides)

	Hin = sum(hs)
	comp$flexRow = NA
	comp$Hin = Hin #  sum(textP[1], textH, textP[2])
	comp$hsu = hsu
	comp
}

#' @export
tmapGridCompWidth.tm_inset_map = function(comp, o) {
	marW = comp$margins[c(2,4)] * o$lin

	ws = c(marW[1], comp$width * o$lin, marW[2])

	sides = switch(comp$position$align.h, left = "second", right = "first", "both")
	wsu = set_unit_with_stretch(ws, sides = sides)

	comp$flexCol = NA
	comp$Win = sum(ws)
	comp$wsu = wsu
	comp
}

#' @export
tmapGridCompPlot.tm_inset_map = function(comp, o, fH, fW) {


	wsu = comp$wsu
	hsu = comp$hsu

	vp = grid::viewport(layout = grid::grid.layout(ncol = length(wsu),
												   nrow = length(hsu),
												   widths = wsu,
												   heights = hsu))

	grobBG = if (getOption("tmap.design.mode")) rectGrob(gp=gpar(fill="#CAB2D6")) else NULL
	gBG = gridCell(3L, 3L:(length(wsu) - 2L), grobBG)

	vp = grid::viewport(layout.pos.row = 3L, layout.pos.col = 3L)

	#comp$tm$o = prepreprocess_meta(comp$tm$o, vp)
	comp$tm$o$asp = as.numeric(wsu[3]) / as.numeric(hsu[3])

	#comp$tm$tmo$group1$layers$layer1$shpDT$shpTM[[1]]$bbox$x = comp$bbox
	comp$tm$o$inset = TRUE
	comp$tm$o$bbox = comp$bbox

	g = step4_plot(comp$tm, vp, return.asp = FALSE, show = FALSE, in.shiny = FALSE, knit = FALSE, knit_opts = list(), args = list())

	#g = gridCell(3L, 3L, grid::rectGrob(gp=gpar(fill="red")))#comp$x)

	do.call(grid::grobTree, c(list(gBG, g), list(vp = vp)))

}



#' @export
tmapGridCompPrepare.tm_minimap = function(comp, o) {
	comp$show = TRUE

	comp
}

#' @export
tmapGridCompHeight.tm_minimap = function(comp, o) {
	tmapGridCompHeight.tm_inset_grob(comp, o)
}

#' @export
tmapGridCompWidth.tm_minimap = function(comp, o) {
	tmapGridCompWidth.tm_inset_grob(comp, o)
}

#' @export
tmapGridCompPlot.tm_minimap = function(comp, o, fH, fW) {
	poly = tmaptools::bb_poly(comp$bbox)

	if (sf::st_is_longlat(comp$bbox)) {
		center = c(mean(comp$bbox[c("xmin", "xmax")]),
				   mean(comp$bbox[c("ymin", "ymax")]))
	} else {
		center = suppressWarnings(round(sf::st_transform(sf::st_centroid(poly), crs = 4326)[[1]][], 3))
	}

	# bound lat to -30,-10 or 10,30
	center[2] = if (center[2] >= 0) {
		max(min(center[2], 30), 10)
	} else {
		max(min(center[2], -10), -30)
	}



	#"+proj=ortho +lat_0=10 +lon_0=0"

	lastcalln = x = get("last_map_new", envir = .TMAP)

	tm = tm_shape(World) +
		tm_crs(ortho_lonlat(center[1], center[2]), bbox = "FULL")+
		tm_polygons(col = NULL, fill = "#2CA02C") +
	tm_shape(poly) +
		tm_polygons(fill = NULL, col = "#EE8866", lwd = 3) +
		#tm_polygons(fill = NULL, col = "#EE8866", lwd = 3, lty = "dotted") +
		tm_graticules(labels.show = FALSE, col = "#000000", lwd = 0.5) +
		tm_layout(bg.color = "#88CCEE",
				  earth_boundary = TRUE,
				  frame = FALSE,
				  space = FALSE) +
		tm_options(space_overlay = o$space_overlay)

	asp = comp$width / comp$height


	comp$x = tmap_grob(tm, asp = asp)

	assign("last_map_new", lastcalln, envir = .TMAP)


	class(comp)[1] = "tm_inset_grob"
	tmapGridCompPlot.tm_inset_grob(comp, o, fH, fW)
}


