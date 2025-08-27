#' @method tmapGridCompPrepare tm_chart
#' @export
tmapGridCompPrepare.tm_chart = function(comp, o) {
	comp
}

#' @method tmapGridCompPrepare tm_chart_bar
#' @export
tmapGridCompPrepare.tm_chart_bar = function(comp, o) {
	if (nlevels(comp$tab$color) == 1) comp$plot.axis.x = TRUE
	comp
}

#' @method tmapGridCompPrepare tm_chart_histogram
#' @export
tmapGridCompPrepare.tm_chart_histogram = function(comp, o) {
	if (nlevels(comp$tab$color) == 1) comp$plot.axis.x = TRUE
	comp
}



#' @method tmapGridCompWidth tm_chart
#' @export
tmapGridCompWidth.tm_chart = function(comp, o) {

	textS = comp$text.size #* o$scale
	#textP = comp$padding[c(3,1)] * textS * o$lin

	comp$margins = c(0.02, 0.02, 0.02, 0.02)
	marW = comp$margins[c(2,4)] * textS * o$lin

	comp$nlines = get_vector_id(comp$width, paste(comp$type, {if (comp$predefined) "min" else "max"}, sep = "."))

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
	comp$nlines = get_vector_id(comp$height, paste(comp$type, {if (comp$predefined) "min" else "max"}, sep = "."))


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

#' @method tmapGridCompPlot tm_chart_histogram
#' @export
tmapGridCompPlot.tm_chart_histogram = function(comp, o, fH, fW) {
	tmapGridCompPlot.tm_chart_bar(comp, o, fH, fW)
}

#' @method tmapGridCompPlot tm_chart_bar
#' @export
tmapGridCompPlot.tm_chart_bar = function(comp, o, fH, fW) {
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

	grobBG = if (getOption("tmap.design.mode")) rectGrob(gp=gpar(fill="#CAB2D6")) else NULL

	g = ggplot2::ggplot(comp$tab, ggplot2::aes(x = bin, y = freq, fill = color)) +
		ggplot2::geom_bar(width = 1, lwd = lwd_to_mm(scale),color = "#000000", stat = "identity", na.rm = TRUE) +
		ggplot2::scale_fill_manual(values = comp$pal) +
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

#' @export
#' @rdname tmap_internal
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




#' @method tmapGridCompPlot tm_chart_donut
#' @export
tmapGridCompPlot.tm_chart_donut = function(comp, o, fH, fW) {
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

	grobBG = if (getOption("tmap.design.mode")) rectGrob(gp=gpar(fill="#CAB2D6")) else NULL

	hsize = 2

	g = ggplot2::ggplot(comp$tab, ggplot2::aes(x = hsize, y = freq, fill = color)) +
		ggplot2::geom_bar(stat = "identity", width = 1, color = "#000000", linewidth = lwd_to_mm(scale)) +
		ggplot2::coord_polar(theta = "y", start = 0) +
		#ggplot2::theme_void() +
		#ggplot2::geom_text(ggplot2::aes(label = scales::percent(Percent), y = Count/2), position = ggplot2::position_stack(vjust = 0.5)) +
		ggplot2::scale_fill_manual(values = comp$pal) +
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




#' @method tmapGridCompPlot tm_chart_violin
#' @export
tmapGridCompPlot.tm_chart_violin = function(comp, o, fH, fW) {
	scale = o$scale * comp$scale
	textsize = o$chart.text.size * scale

	wsu = comp$wsu
	hsu = comp$hsu

	vp = grid::viewport(layout = grid::grid.layout(ncol = length(wsu),
												   nrow = length(hsu),
												   widths = wsu,
												   heights = hsu))

	grobBG = if (getOption("tmap.design.mode")) rectGrob(gp=gpar(fill="#CAB2D6")) else NULL

	hsize = 2

	g = ggplot2::ggplot(comp$df, ggplot2::aes(x = x, y = y)) +
		ggplot2::geom_violin(fill = comp$object.color) +
		theme_chart(plot.axis.x = comp$plot.axis.x, plot.axis.y = comp$plot.axis.y, scale = scale, text.color = o$chart.text.color, text.size = textsize)

	g2 = ggplot2::ggplotGrob(g)

	# other grid cells are aligns (1 and 5) and margins (2 and 4)
	chart = gridCell(3,3, {
		gTree(children=gList(grobBG,
							 g2),
			  name="compass")
	})

	grid::grobTree(chart, vp = vp)
}


#' @method tmapGridCompPlot tm_chart_box
#' @export
tmapGridCompPlot.tm_chart_box = function(comp, o, fH, fW) {
	scale = o$scale * comp$scale
	textsize = o$chart.text.size * scale

	wsu = comp$wsu
	hsu = comp$hsu

	vp = grid::viewport(layout = grid::grid.layout(ncol = length(wsu),
												   nrow = length(hsu),
												   widths = wsu,
												   heights = hsu))

	grobBG = if (getOption("tmap.design.mode")) rectGrob(gp=gpar(fill="#CAB2D6")) else NULL

	hsize = 2

	g = ggplot2::ggplot(comp$df, ggplot2::aes(x = x, y = y)) +
		ggplot2::geom_boxplot(fill = comp$object.color) +
		theme_chart(plot.axis.x = comp$plot.axis.x, plot.axis.y = comp$plot.axis.y, scale = scale, text.color = o$chart.text.color, text.size = textsize)

	g2 = ggplot2::ggplotGrob(g)

	# other grid cells are aligns (1 and 5) and margins (2 and 4)
	chart = gridCell(3,3, {
		gTree(children=gList(grobBG,
							 g2),
			  name="compass")
	})

	grid::grobTree(chart, vp = vp)
}


#' @method tmapGridCompPlot tm_chart_heatmap
#' @export
tmapGridCompPlot.tm_chart_heatmap = function(comp, o, fH, fW) {
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

	grobBG = if (getOption("tmap.design.mode")) rectGrob(gp=gpar(fill="#CAB2D6")) else NULL

	g = ggplot2::ggplot(comp$tab, ggplot2::aes(x = bin2, y = bin1, fill = freq)) +
		ggplot2::geom_tile() +
		ggplot2::scale_fill_distiller(type = "seq",
							  direction = 1,
							  palette = "Greys") +
		theme_chart(plot.axis.x = TRUE, plot.axis.y = TRUE, scale = scale, text.color = o$chart.text.color, text.size = textsize, plot.grid.x = FALSE, plot.grid.y = FALSE) #comp$plot.axis.x

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
