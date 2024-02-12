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
	marW = comp$margins[c(2,4)] * textS * o$lin
	
	if (is.na(comp$width)) {
		comp$nlines = 10
	} else {
		comp$nlines = comp$width
		
	}
	body = comp$nlines * textS * o$lin
	ws = c(marW[1], body, marW[2])

	sides = switch(comp$position$align.h, left = "second", right = "first", "both")
	wsu = set_unit_with_stretch(ws, sides = sides)
	comp$flexCol = NA
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
	comp$flexRow = NA
	comp$Hin = Hin #  sum(textP[1], textH, textP[2])
	comp$hsu = hsu
	
	comp
	
}


#' @method tmapGridLegPlot tm_chart_histogram
#' @export
tmapGridLegPlot.tm_chart_histogram = function(comp, o, fH, fW) {
	u = 1/(comp$nlines)
	#vpComp = viewport(x=u, y=u, height=1-2*u, width=1-2*u, just=c("left", "bottom"))
	scale = o$scale * comp$scale
	
	textsize = o$chart.text.size * scale
	
	wsu = comp$wsu
	hsu = comp$hsu
	
	vp = grid::viewport(layout = grid::grid.layout(ncol = length(wsu),
												   nrow = length(hsu), 
												   widths = wsu,
												   heights = hsu))
	
	grobBG = if (getOption("tmap.design.mode")) rectGrob(gp=gpar(fill="orange")) else NULL
	
	#grobRect = rectGrob(gp=gpar(fill="purple"))
	
	df = data.frame(x = comp$x1)
	if (is.null(comp$breaks)) {
		breaks = comp$breaks_def
		ids = 1L:(length(breaks) - 1L)
	} else {
		breaks = comp$breaks
		subbreaks = (all(comp$breaks_def %in% breaks))
		
		break_mids = (breaks[-1] + head(breaks, -1)) / 2
		
		ids = as.integer(cut(break_mids, comp$breaks_def, include.lowest = TRUE, right = FALSE))
	}
	
	df$xcat = cut(df$x, breaks = breaks, include.lowest = TRUE, right = FALSE)
	
	vvalues = comp$vvalues
	
	if (comp$na.show) {
		tab = as.data.frame(table(df$xcat, useNA = "always"))
		tab$color = factor(c(ids, length(vvalues)), levels = seq_along(vvalues))
		pal = structure(vvalues, names = levels(tab$color))
	} else {
		tab = as.data.frame(table(df$xcat, useNA = "no"))
		tab$color = factor(ids, levels = seq_along(vvalues))
		pal = structure(vvalues, names = levels(tab$color))
	}

	
	g = ggplot2::ggplot(tab, ggplot2::aes(x = Var1, y = Freq, fill = color)) +
		ggplot2::geom_bar(width = 1, lwd = lwd_to_mm(scale),color = "#000000", stat = "identity", na.rm = TRUE) + 
		ggplot2::scale_fill_manual(values = pal) + 
		theme_chart(plot.axis.x = comp$plot.axis.x, plot.axis.y = comp$plot.axis.y, scale = scale, text.color = o$chart.text.color, text.size = textsize)
	
	g = g + comp$extra.ggplot2
	
	g2 = ggplot2::ggplotGrob(g)
	
	# other grid cells are aligns (1 and 5) and margins (2 and 4)
	histogram = gridCell(3,3, {
		gTree(children=gList(grobBG, 
							 g2), 
			  name="compass")
	})
	
	grid::grobTree(histogram, vp = vp)
}

## borrowed from https://github.com/CMAP-REPOS/cmapplot
# .lwd <- ggplot2::.pt / ggplot2::.stroke

lwd_to_mm = function(value, unit = "bigpts") {
	grid::convertUnit(grid::unit(value, unit), "mm", valueOnly = TRUE)
}


# if (FALSE) {
# 	library(ggplot2)
# 	ggplot(mpg, aes(class)) +
# 		geom_bar(fill = NA, color = "black", linewidth = gg_lwd_convert(12))
# 	grid.lines(y = c(0.5, 0.5), gp=gpar(lwd = 12))
# 		
# }




#' @method tmapGridLegPlot tm_chart_donut
#' @export
tmapGridLegPlot.tm_chart_donut = function(comp, o, fH, fW) {
	u = 1/(comp$nlines)
	#vpComp = viewport(x=u, y=u, height=1-2*u, width=1-2*u, just=c("left", "bottom"))
	scale = o$scale * comp$scale
	textsize = o$chart.text.size * scale
	
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
		ggplot2::geom_bar(stat = "identity", width = 1, color = "#000000", linewidth = lwd_to_mm(scale)) +
		ggplot2::coord_polar(theta = "y", start = 0) +
		#ggplot2::theme_void() +
		#ggplot2::geom_text(ggplot2::aes(label = scales::percent(Percent), y = Count/2), position = ggplot2::position_stack(vjust = 0.5)) +
		ggplot2::scale_fill_manual(values = vvalues, na.value = na.value) + 
		theme_chart(plot.grid.y = FALSE, scale = scale, text.color = o$chart.text.color, text.size = textsize) +
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


