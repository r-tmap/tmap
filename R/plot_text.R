plot_text <- function(co.native, g, gt, lineNatH, lineNatW, just=c("center", "center"), bbx = bbx, bg.margin=.10) {
	lineHnative <- lineNatH * gt$scale
	lineWnative <- lineNatW * gt$scale
	
	npol <- nrow(co.native)
	with(g, {
		if (!any(text_sel)) {
			return(NULL)
		}
		
		co.native[, 1] <- co.native[, 1] + text.xmod * lineWnative
		co.native[, 2] <- co.native[, 2] + text.ymod * lineHnative

		
		grobText <- textGrob(text[text_sel], x=unit(co.native[text_sel,1], "native"), y=unit(co.native[text_sel,2], "native"), just=just, gp=gpar(col=text.color[text_sel], cex=text.size[text_sel], fontface=text.fontface, fontfamily=text.fontfamily))
		nlines <- rep(1, length(text))
		
		
		lineH <- npc_to_native(convertHeight(unit(text.size[text_sel], "lines"), "native", valueOnly=TRUE), scale = bbx[c(2,4)])
		lineW <- npc_to_native(convertWidth(unit(text.size[text_sel], "lines"), "native", valueOnly=TRUE), scale = bbx[c(1,3)])
		
		tGH <- npc_to_native(mapply(text[text_sel], text.size[text_sel], nlines[text_sel], FUN=function(x,y,z){
			convertHeight(grobHeight(textGrob(x, gp=gpar(cex=y, fontface=text.fontface, fontfamily=text.fontfamily))),"native", valueOnly=TRUE) * z/(z-0.25)}, USE.NAMES=FALSE), scale = bbx[c(2,4)])
		
		tGW <- npc_to_native(mapply(text[text_sel], text.size[text_sel], FUN=function(x,y){
			convertWidth(grobWidth(textGrob(x, gp=gpar(cex=y, fontface=text.fontface, fontfamily=text.fontfamily))),"native", valueOnly=TRUE)}, USE.NAMES=FALSE), scale = bbx[c(1,3)])
		

		justx <- .5 - just[1]
		justy <- .6 - just[2]
		
		tGX <- grobText$x + unit(tGW * justx, "native")
		tGY <- grobText$y + unit(tGH * justy, "native")
		
		tGH <- unit(tGH + lineH * bg.margin, "native")
		tGW <- unit(tGW + lineW * bg.margin, "native")
		grobTextBG <- rectGrob(x=tGX, y=tGY, width=tGW, height=tGH, gp=gpar(fill=text.bg.color, col=NA))
		# 		} else {
		# 			grobTextBG <- NULL
		# 		}
		
		if (text.shadow) {
			grobTextSh <- textGrob(text[text_sel], x=unit(co.native[text_sel,1]+lineW * .05, "native"), y=unit(co.native[text_sel,2]- lineH * .05, "native"), just=just, gp=gpar(col=text.shadowcol[text_sel], cex=text.size[text_sel], fontface=text.fontface, fontfamily=text.fontfamily))
		} else {
			grobTextSh <- NULL
		}
		
		gList(grobTextBG, grobTextSh, grobText)
	})
}

################!!!!! Functions below needed for Advanced text options !!!!####################

.grob2Poly <- function(g) {
	x <- convertX(g$x, unitTo = "native", valueOnly = TRUE)
	y <- convertY(g$y, unitTo = "native", valueOnly = TRUE)
	if (inherits(g, "rect")) {
		w <- convertWidth(g$width, unitTo = "native", valueOnly = TRUE)
		h <- convertHeight(g$height, unitTo = "native", valueOnly = TRUE)
		x1 <- x - .5*w
		x2 <- x + .5*w
		y1 <- y - .5*h
		y2 <- y + .5*h
		polys <- mapply(function(X1, X2, Y1, Y2) {
			st_polygon(list(cbind(c(X1, X2, X2, X1, X1),
						  c(Y2, Y2, Y1, Y1, Y2))))
		}, x1, x2, y1, y2, SIMPLIFY=FALSE)
		st_union(st_sfc(polys))
	} else if (inherits(g, "polygon")) {
		xs <- split(x, g$id)
		ys <- split(y, g$id)
		
		polys <- mapply(function(xi, yi) {
			co <- cbind(xi, yi)
			st_polygon(list(rbind(co, co[1,])))
		}, xs, ys, SIMPLIFY = FALSE)
		st_union(st_sfc(polys))
	} # else return(NULL)

}

polylineGrob2sfLines <- function(gL) {
	if (is.null(gL$id)) {
		ids = unlist(mapply(rep, 1L:length(gL$id.lengths), gL$id.lengths, SIMPLIFY = FALSE, USE.NAMES = FALSE))
	} else {
		ids = gL$id
	}
	coords <- mapply(cbind, split(as.numeric(gL$x), ids), split(as.numeric(gL$y), ids), SIMPLIFY = FALSE)
	
	st_sf(geometry = st_sfc(st_multilinestring(coords)))
}

npc_to_native <- function(x, scale) {
	x * (scale[2] - scale[1])# + scale[1]
}

native_to_npc_to_native <- function(x, scale) {
	#(x - scale[1]) / (scale[2] - scale[1])
	(x) / (scale[2] - scale[1])
}

.rectGrob2pathGrob <- function(rg, angles) {
	x <- convertX(rg$x, "inch", valueOnly=TRUE)
	y <- convertY(rg$y, "inch", valueOnly=TRUE)
	w <- convertWidth(rg$width, "inch", valueOnly=TRUE)
	h <- convertHeight(rg$height, "inch", valueOnly=TRUE)

	a <- atan2(h, w)
	as <- as.vector(vapply(a, function(a)c(a,pi-a, pi+a,-a), numeric(4)))

	as2 <- as + rep(angles * pi / 180, each=4)

	dst <- rep(sqrt((w/2)^2+(h/2)^2), each=4)

	xs <- rep(x, each=4) + cos(as2) * dst
	ys <- rep(y, each=4) + sin(as2) * dst

	xs2 <- convertX(unit(xs, "inch"), "npc")
	ys2 <- convertY(unit(ys, "inch"), "npc")

	id <- rep(1:length(x), each=4)

	w2 <- w + (h-w) * abs(sin(angles*pi/180))
	h2 <- h + (w-h) * abs(sin(angles*pi/180))

	w3 <- convertWidth(unit(w2, "inch"), "npc")
	h3 <- convertHeight(unit(h2, "inch"), "npc")

	list(poly=polygonGrob(xs2, ys2, id=id, gp=rg$gp),
		 rect=rectGrob(rg$x, rg$y, width = w3, height=h3))
}

.get_direction_angle <- function(co) {
	p1 <- co[1,]
	p2 <- co[nrow(co),]

	a <- atan2(p2[2] - p1[2], p2[1] - p1[1]) * 180 / pi
	if (a < 0) a <- a + 360
	a
}


.editGrob <- function(tg, sel, shiftX, shiftY, angles) {
	nt <- length(sel)
	angles <- rep(angles, length.out=nt)
	if (any(angles!=0)) {
		if (inherits(tg, "rect")) {
			tg <- .rectGrob2pathGrob(tg, angles)$poly
		}
	}
	tgx <- convertX(tg$x, "native", valueOnly = TRUE)
	tgy <- convertY(tg$y, "native", valueOnly = TRUE)

	if (inherits(tg, "polygon")) {
		sel4 <- rep(sel, each=4)
		tg$x <- unit(tgx + rep(shiftX, each=4), "native")[sel4]
		tg$y <- unit(tgy + rep(shiftY, each=4), "native")[sel4]
		tg$id <- rep(1:sum(sel), each=4)
	} else {
		tg$x <- unit(tgx + shiftX, "native")[sel]
		tg$y <- unit(tgy + shiftY, "native")[sel]
		if (inherits(tg, "rect")) {
			tg$height <- tg$height[sel]
			tg$width <- tg$width[sel]
		} else if (inherits(tg, "text")) {
			tg$label <- tg$label[sel]
			tg$rot <- angles[sel]
		}
	}
	tg$gp <- do.call("gpar", lapply(unclass(tg$gp)[names(tg$gp)!="font"], function(g) {
		if (length(g)==nt) g[sel] else g
	}))
	tg
}
