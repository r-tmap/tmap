# NEED TO SYNC THIS WITH tmap_options
# generate all options with: 
# args = intersect(names(tmap_options_mode("view")),names(tmap_options_mode("plot")))
# cat(paste(args, collapse = ", "))


#' Layout options
#' 
#' Set of tmap options that are directly related to the layout.
#' 
#' @param modes,crs,facet.max,facet.flip,raster.max.cells,show.messages,show.warnings,output.format,output.size,output.dpi,output.dpi.animation,value.const,value.na,value.null,value.blank,values.var,values.range,value.neutral,scales.var,scale.misc.args,label.format,label.na,scale,asp,bg.color,outer.bg.color,frame,frame.lwd,frame.r,frame.double.line,outer.margins,inner.margins,inner.margins.extra,meta.margins,meta.auto.margins,between.margin,component.offset,component.stack.margin,grid.mark.height,xylab.height,coords.height,xlab.show,xlab.text,xlab.size,xlab.color,xlab.rotation,xlab.space,xlab.fontface,xlab.fontfamily,xlab.side,ylab.show,ylab.text,ylab.size,ylab.color,ylab.rotation,ylab.space,ylab.fontface,ylab.fontfamily,ylab.side,panel.type,panel.wrap.pos,panel.xtab.pos,unit,color.sepia.intensity,color.saturation,color.vision.deficiency.sim,text.fontface,text.fontfamily,legend.show,legend.design,legend.orientation,legend.position,legend.width,legend.height,legend.stack,legend.group.frame,legend.resize.as.group,legend.reverse,legend.na.show,legend.title.color,legend.title.size,legend.title.fontface,legend.title.fontfamily,legend.xlab.color,legend.xlab.size,legend.xlab.fontface,legend.xlab.fontfamily,legend.ylab.color,legend.ylab.size,legend.ylab.fontface,legend.ylab.fontfamily,legend.text.color,legend.text.size,legend.text.fontface,legend.text.fontfamily,legend.frame,legend.frame.lwd,legend.frame.r,legend.bg.color,legend.bg.alpha,legend.settings.standard.portrait,legend.settings.standard.landscape,title.show,title.size,title.color,title.fontface,title.fontfamily,title.bg.color,title.bg.alpha,title.padding,title.frame,title.frame.lwd,title.frame.r,title.stack,title.position,title.group.frame,title.resize.as.group,credits.show,credits.size,credits.color,credits.fontface,credits.fontfamily,credits.bg.color,credits.bg.alpha,credits.padding,credits.frame,credits.frame.lwd,credits.frame.r,credits.stack,credits.position,credits.width,credits.heigth,credits.group.frame,credits.resize.as.group,compass.north,compass.type,compass.text.size,compass.size,compass.show.labels,compass.cardinal.directions,compass.text.color,compass.color.dark,compass.color.light,compass.lwd,compass.bg.color,compass.bg.alpha,compass.margins,compass.show,compass.stack,compass.position,compass.frame,compass.frame.lwd,compass.frame.r,compass.group.frame,compass.resize.as.group,scalebar.show,scalebar.breaks,scalebar.width,scalebar.text.size,scalebar.text.color,scalebar.color.dark,scalebar.color.light,scalebar.lwd,scalebar.position,scalebar.bg.color,scalebar.bg.alpha,scalebar.size,scalebar.margins,scalebar.stack,scalebar.frame,scalebar.frame.lwd,scalebar.frame.r,scalebar.group.frame,scalebar.resize.as.group,grid.show,grid.labels.pos,grid.x,grid.y,grid.n.x,grid.n.y,grid.crs,grid.col,grid.lwd,grid.alpha,grid.labels.show,grid.labels.size,grid.labels.col,grid.labels.rot,grid.labels.format,grid.labels.cardinal,grid.labels.margin.x,grid.labels.margin.y,grid.labels.space.x,grid.labels.space.y,grid.labels.inside.frame,grid.ticks,grid.lines,grid.ndiscr,mouse_coordinates.stack,mouse_coordinates.position,mouse_coordinates.show,panel.show,panel.labels,panel.label.size,panel.label.color,panel.label.fontface,panel.label.fontfamily,panel.label.bg.color,panel.label.height,panel.label.rot,qtm.scalebar,qtm.minimap,qtm.mouse.coordinates,earth.boundary,earth.boundary.color,earth.boundary.lwd,earth.datum,space.color,attr.color,max.categories,legend.hist.bg.color,legend.hist.bg.alpha,legend.hist.size,legend.hist.height,legend.hist.width,attr.outside,attr.outside.position,attr.outside.size,attr.position,attr.just,basemap.server,basemap.alpha,basemap.zoom,overlays,overlays.alpha,alpha,colorNA,symbol.size.fixed,dot.size.fixed,text.size.variable,bbox,check.and.fix,set.bounds,set.view,set.zoom.limits,name,basemap.show see\code{\link{tmap_options}}
#' @param title,main.title deprecated
#' @param ... used to catch other deprecated arguments
#' @rdname tm_layout
#' @example ./examples/tm_layout.R 
#' @export 
tm_layout = function(
		modes, crs, facet.max, facet.flip, raster.max.cells, show.messages, show.warnings, output.format, output.size, output.dpi, output.dpi.animation, value.const, value.na, value.null, value.blank, values.var, values.range, value.neutral, scales.var, scale.misc.args, label.format, label.na, scale, asp, bg.color, outer.bg.color, frame, frame.lwd, frame.r, frame.double.line, outer.margins, inner.margins, inner.margins.extra, meta.margins, meta.auto.margins, between.margin, component.offset, component.stack.margin, grid.mark.height, xylab.height, coords.height, xlab.show, xlab.text, xlab.size, xlab.color, xlab.rotation, xlab.space, xlab.fontface, xlab.fontfamily, xlab.side, ylab.show, ylab.text, ylab.size, ylab.color, ylab.rotation, ylab.space, ylab.fontface, ylab.fontfamily, ylab.side, panel.type, panel.wrap.pos, panel.xtab.pos, unit, color.sepia.intensity, color.saturation, color.vision.deficiency.sim, text.fontface, text.fontfamily, legend.show, legend.design, legend.orientation, legend.position, legend.width, legend.height, legend.stack, legend.group.frame, legend.resize.as.group, legend.reverse, legend.na.show, legend.title.color, legend.title.size, legend.title.fontface, legend.title.fontfamily, legend.xlab.color, legend.xlab.size, legend.xlab.fontface, legend.xlab.fontfamily, legend.ylab.color, legend.ylab.size, legend.ylab.fontface, legend.ylab.fontfamily, legend.text.color, legend.text.size, legend.text.fontface, legend.text.fontfamily, legend.frame, legend.frame.lwd, legend.frame.r, legend.bg.color, legend.bg.alpha, legend.settings.standard.portrait, legend.settings.standard.landscape, title.show, title.size, title.color, title.fontface, title.fontfamily, title.bg.color, title.bg.alpha, title.padding, title.frame, title.frame.lwd, title.frame.r, title.stack, title.position, title.group.frame, title.resize.as.group, credits.show, credits.size, credits.color, credits.fontface, credits.fontfamily, credits.bg.color, credits.bg.alpha, credits.padding, credits.frame, credits.frame.lwd, credits.frame.r, credits.stack, credits.position, credits.width, credits.heigth, credits.group.frame, credits.resize.as.group, compass.north, compass.type, compass.text.size, compass.size, compass.show.labels, compass.cardinal.directions, compass.text.color, compass.color.dark, compass.color.light, compass.lwd, compass.bg.color, compass.bg.alpha, compass.margins, compass.show, compass.stack, compass.position, compass.frame, compass.frame.lwd, compass.frame.r, compass.group.frame, compass.resize.as.group, scalebar.show, scalebar.breaks, scalebar.width, scalebar.text.size, scalebar.text.color, scalebar.color.dark, scalebar.color.light, scalebar.lwd, scalebar.position, scalebar.bg.color, scalebar.bg.alpha, scalebar.size, scalebar.margins, scalebar.stack, scalebar.frame, scalebar.frame.lwd, scalebar.frame.r, scalebar.group.frame, scalebar.resize.as.group, grid.show, grid.labels.pos, grid.x, grid.y, grid.n.x, grid.n.y, grid.crs, grid.col, grid.lwd, grid.alpha, grid.labels.show, grid.labels.size, grid.labels.col, grid.labels.rot, grid.labels.format, grid.labels.cardinal, grid.labels.margin.x, grid.labels.margin.y, grid.labels.space.x, grid.labels.space.y, grid.labels.inside.frame, grid.ticks, grid.lines, grid.ndiscr, mouse_coordinates.stack, mouse_coordinates.position, mouse_coordinates.show, panel.show, panel.labels, panel.label.size, panel.label.color, panel.label.fontface, panel.label.fontfamily, panel.label.bg.color, panel.label.height, panel.label.rot, qtm.scalebar, qtm.minimap, qtm.mouse.coordinates, earth.boundary, earth.boundary.color, earth.boundary.lwd, earth.datum, space.color, attr.color, max.categories, legend.hist.bg.color, legend.hist.bg.alpha, legend.hist.size, legend.hist.height, legend.hist.width, attr.outside, attr.outside.position, attr.outside.size, attr.position, attr.just, basemap.server, basemap.alpha, basemap.zoom, overlays, overlays.alpha, alpha, colorNA, symbol.size.fixed, dot.size.fixed, text.size.variable, bbox, check.and.fix, set.bounds, set.view, set.zoom.limits, name, basemap.show,
		title = NULL,
		main.title = NULL,
		...
		
	) {
	args = lapply(as.list(match.call()[-1]), eval, envir = parent.frame())
	if (!is.null(title) || (!is.null(main.title))) {
		title1 = if (!is.null(title)) {
			title.args = args[substr(names(args), 1, 5) == "title"]
			title.args$title = NULL
			names(title.args) = substr(names(title.args), 7, nchar(names(title.args)))
			warning("The 'title' argument of 'tm_layout()' is deprecated as of tmap 4.0. Please use 'tm_title()' instead.", call. = FALSE)
			if (!("position" %in% names(title.args))) title.args$position = tm_pos_in("left", "top")
			args$title = NULL
			do.call(tm_title, c(list(text = title), title.args))
		} else {
			NULL
		}
		title2 = if (!is.null(main.title)) {
			main.title.args = args[substr(names(args), 1, 5) == "main.title"]
			main.title.args$main.title = NULL
			names(main.title.args) = substr(names(main.title.args), 7, nchar(names(main.title.args)))
			warning("The 'main.title' argument of 'tm_layout()' is deprecated as of tmap 4.0. Please use 'tm_title()' instead.", call. = FALSE)
			args$main.title = NULL
			do.call(tm_title, c(list(text = main.title), main.title.args))
		} else {
			NULL
		}
		do.call(tm_options, args) + title1 + title2
	} else {
		do.call(tm_options, args)
	}
}

#' View mode options
#' 
#' View mode options. These options are specific to the view mode.
#' 
#' @param use.WebGL use webGL layers with leafgl
#' @param control.position position of the control attribute
#' @param control.bases base layers
#' @param control.overlays overlay layers
#' @param set.bounds logical that determines whether maximum bounds are set, or a bounding box. Not applicable in plot mode. In view mode, this is passed on to \code{\link[leaflet:setMaxBounds]{setMaxBounds}}
#' @param set.view numeric vector that determines the view. Either a vector of three: lng, lat, and zoom, or a single value: zoom. See \code{\link[leaflet:setView]{setView}}. Only applicable if \code{bbox} is not specified
#' @param set.zoom.limits numeric vector of two that set the minimum and maximum zoom levels (see \code{\link[leaflet:tileOptions]{tileOptions}}).
#' @param leaflet.options options passed on to \code{\link[leaflet:leafletOptions]{leafletOptions}}
#' @export
tm_view = function(use.WebGL,
				   control.position, 
				   control.bases,
				   control.overlays,
				   set.bounds,
				   set.view,
				   set.zoom.limits,
				   leaflet.options) {
	args = lapply(as.list(match.call()[-1]), eval, envir = parent.frame())
	do.call(tm_options, args)
}

#' Plot mode options
#' 
#' Plot mode options. This option is specific to the plot mode.
#' 
#' @param use.gradient Use gradient fill using \code{\link[grid:linearGradient]{linearGradient}}
#' @export
tm_plot = function(use.gradient) {
	args = lapply(as.list(match.call()[-1]), eval, envir = parent.frame())
	do.call(tm_options, args)
}


