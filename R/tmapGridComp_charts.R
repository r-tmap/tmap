#' @method tmapGridCompPrepare tm_chart
#' @export
tmapGridCompPrepare.tm_chart = function(comp, o) {
	comp
}


#' @method tmapGridCompWidth tm_chart
#' @export
tmapGridCompWidth.tm_chart = function(comp, o) {
	
	textS = comp$text.size #* o$scale
	#textP = comp$padding[c(3,1)] * textS * o$lin
	
	comp$margins = c(0.02, 0.02, 0.02, 0.02)
	comp$nlines = 10
	
	marW = comp$margins[c(2,4)] * textS * o$lin
	ws = c(marW[1], comp$nlines * textS * o$lin, marW[2])
	
	sides = switch(comp$position$align.h, left = "second", right = "first", "both")
	wsu = set_unit_with_stretch(ws, sides = sides)
	
	comp$Win = sum(ws)
	comp$wsu = wsu
	comp
	
}


#' @method tmapGridCompHeight tm_chart
#' @export
tmapGridCompHeight.tm_chart = function(comp, o) {

	textS = comp$text.size #* o$scale
	#textP = comp$padding[c(3,1)] * textS * o$lin
	
	comp$margins = c(0.02, 0.02, 0.02, 0.02)
	comp$nlines = 10
	
	
	marH = comp$margins[c(3,1)] * textS * o$lin
	hs = c(marH[1], comp$nlines * textS * o$lin, marH[2])
	
	
	sides = switch(comp$position$align.v, top = "second", bottom = "first", "both")
	hsu = set_unit_with_stretch(hs, sides = sides)
	
	Hin = sum(hs)
	
	comp$Hin = Hin #  sum(textP[1], textH, textP[2])
	comp$hsu = hsu
	
	comp
	
}


#' @method tmapGridLegPlot tm_chart_histogram
#' @export
tmapGridLegPlot.tm_chart_histogram = function(comp, o, fH, fW) {
	u = 1/(comp$nlines)
	#vpComp = viewport(x=u, y=u, height=1-2*u, width=1-2*u, just=c("left", "bottom"))
	
	wsu = comp$wsu
	hsu = comp$hsu
	
	vp = grid::viewport(layout = grid::grid.layout(ncol = length(wsu),
												   nrow = length(hsu), 
												   widths = wsu,
												   heights = hsu))
	
	grobBG = if (getOption("tmap.design.mode")) rectGrob(gp=gpar(fill="orange")) else NULL
	
	#grobRect = rectGrob(gp=gpar(fill="purple"))
	
	df = data.frame(x = comp$x1)
	df$xcat = cut(df$x, breaks = comp$breaks, include.lowest = TRUE, right = FALSE)
	
	if (comp$na.show) {
		na.value = tail(comp$vvalues, 1)
		vvalues = head(comp$vvalues, -1)
	} else {
		vvalues = comp$vvalues
		df = df[!is.na(df$xcat), ]
		na.value = "#000000"
	}
	
	g = ggplot2::ggplot(df, ggplot2::aes(x = xcat, fill = xcat)) + ggplot2::geom_bar(stat = "count", na.rm = TRUE) + ggplot2::scale_fill_manual(values = vvalues, na.value = na.value) + theme_chart(plot.axis.y = TRUE)
	
	g2 = ggplot2::ggplotGrob(g)
	
	# other grid cells are aligns (1 and 5) and margins (2 and 4)
	histogram = gridCell(3,3, {
		gTree(children=gList(grobBG, 
							 g2), 
			  name="compass")
	})
	
	grid::grobTree(histogram, vp = vp)
}



#' @method tmapGridLegPlot tm_chart_donut
#' @export
tmapGridLegPlot.tm_chart_donut = function(comp, o, fH, fW) {
	u = 1/(comp$nlines)
	#vpComp = viewport(x=u, y=u, height=1-2*u, width=1-2*u, just=c("left", "bottom"))
	
	wsu = comp$wsu
	hsu = comp$hsu
	
	vp = grid::viewport(layout = grid::grid.layout(ncol = length(wsu),
												   nrow = length(hsu), 
												   widths = wsu,
												   heights = hsu))
	
	grobBG = if (getOption("tmap.design.mode")) rectGrob(gp=gpar(fill="orange")) else NULL
	
	#grobRect = rectGrob(gp=gpar(fill="purple"))
	
	df = data.frame(x = comp$x1)
	
	if (length(comp$breaks) == 0) browser()
	
	df$xcat = cut(df$x, breaks = comp$breaks, include.lowest = TRUE, right = FALSE)
	
	if (comp$na.show) {
		na.value = tail(comp$vvalues, 1)
		vvalues = head(comp$vvalues, -1)
	} else {
		vvalues = comp$vvalues
		df = df[!is.na(df$xcat), ]
		na.value = "#000000"
	}
	
	
	a = as.data.frame(table(df$xcat, useNA = {if (comp$na.show) "always" else "no"}))
	
	hsize = 2
	
	g = ggplot2::ggplot(a, ggplot2::aes(x = hsize, y = Freq, fill = Var1)) +
		ggplot2::geom_bar(stat = "identity", width = 1, color = "#000000") +
		ggplot2::coord_polar(theta = "y", start = 0) +
		#ggplot2::theme_void() +
		#ggplot2::geom_text(ggplot2::aes(label = scales::percent(Percent), y = Count/2), position = ggplot2::position_stack(vjust = 0.5)) +
		ggplot2::scale_fill_manual(values = vvalues, na.value = na.value) + 
		theme_chart() +
		ggplot2::xlim(c(0, hsize + 0.5))
		#ggplot2::theme(
	#		legend.position = "right",
#			legend.title = ggplot2::element_blank()
#		)
	
	
	g2 = ggplot2::ggplotGrob(g)
	
	# other grid cells are aligns (1 and 5) and margins (2 and 4)
	chart = gridCell(3,3, {
		gTree(children=gList(grobBG, 
							 g2), 
			  name="compass")
	})
	
	grid::grobTree(chart, vp = vp)
}


