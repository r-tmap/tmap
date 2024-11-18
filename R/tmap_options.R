# NEED TO SYNC THIS WITH tmap_options
# generate all options with:
# args = intersect(names(tmap_options_mode("view")),names(tmap_options_mode("plot")))
# cat(paste(args, collapse = ", "))
# x = sapply(args, function(a) paste0("#' @param ", a, " `r opt_inf(\"", a, "\")`"))
# cat(paste(x, collapse = "\n"))
opt_inf = function(a) {
	"test123"
}

#' tmap options
#'
#' tmap options
#'
#' @param ... List of tmap options to be set, or option names (characters) to be returned (see details)
#' @param crs Map crs (see [tm_shape()]). `NA` means the crs is specified in [tm_shape()]. The crs that is used by the transformation functions is defined in [tm_shape()].
#' @param facet.max Maximum number of facets
#' @param facet.flip Should facets be flipped (in case of facet wrap)? This can also be set via [tm_facets_flip()]
#' @param free.scales For backward compatibility: if this value is set, it will be used to impute the free arguments in the layer functions
#' @param raster.max_cells Maximum number of raster grid cells
#' @param show.messages Show messages?
#' @param show.warnings Show warnings?
#' @param output.format Output format
#' @param output.size Output size
#' @param output.dpi Output dpi
#' @param output.dpi.animation Output dpi for animations
#' @param value.const Default visual value constants e.g. the default fill color for `tm_shape(World) + tm_polygons()`. A list is required with per visual variable a value.
#' @param value.na Default visual values that are used to visualize NA data values. A list is required with per visual variable a value.
#' @param value.null Default visual values that are used to visualize null (out-of-scope) data values. A list is required with per visual variable a value.
#' @param value.blank Default visual values that correspond to blank. For color these are `"#00000000"` meaning transparent. A list is required with per visual variable a value.
#' @param values.var Default values when a data variable to mapped to a visual variable, e.g. a color palette. A list is required with per visual variable a value.
#' @param values.range Default range for values. See `values.range` of [tm_scale_categorical()]. A list is required with per visual variable a value.
#' @param value.neutral Default values for when a data variable to mapped to a visual variable, e.g. a color palette. A list is required with per visual variable a value.
#' @param values.scale Default scales (as in object sizes) for values. See `values.range` of [tm_scale_categorical()]. A list is required with per visual variable a value.
#' @param scales.var Default scale functions per visual variable and type of data variable. A list is required with per visual variable per data type.
#' @param scale.misc.args Default values of scale function-specific arguments. A list is required with per scale function and optional per visual variable.
#' @param continuous.nclass_per_legend_break The number of continuous legend breaks within one 'unit' (label).  The dafault value is 50.
#' @param continuous.nclasses the number of classes of a continuous scale. Should be odd.  The dafault value is 101.
#' @param label.format Format for the labels (was `legend.format` in tmap v3).
#' @param label.na Default label for missing values.
#' @param scale Overall scale of the map
#' @param asp Aspect ratio of each map. When `asp` is set to `NA` (default) the aspect ratio will be adjusted to the used shapes. When set to 0 the aspect ratio is
#'   adjusted to the size of the device divided by the number of columns and rows.
#' @param bg.color Background color of the map.
#' @param outer.bg.color Background color of map outside the frame.
#' @param frame `r opt_inf("frame")`
#' @param frame.lwd `r opt_inf("frame.lwd")`
#' @param frame.r `r opt_inf("frame.r")`
#' @param frame.double_line `r opt_inf("frame.double_line")`
#' @param outer.margins `r opt_inf("outer.margins")`
#' @param inner.margins `r opt_inf("inner.margins")`
#' @param inner.margins.extra `r opt_inf("inner.margins.extra")`
#' @param meta.margins `r opt_inf("meta.margins")`
#' @param meta.auto_margins `r opt_inf("meta.auto_margins")`
#' @param between_margin `r opt_inf("between_margin")`
#' @param panel.margin `r opt_inf("panel.margin")`
#' @param component.offset `r opt_inf("component.offset")`
#' @param component.stack_margin `r opt_inf("component.stack_margin")`
#' @param grid.mark.height `r opt_inf("grid.mark.height")`
#' @param xylab.height `r opt_inf("xylab.height")`
#' @param coords.height `r opt_inf("coords.height")`
#' @param xlab.show `r opt_inf("xlab.show")`
#' @param xlab.text `r opt_inf("xlab.text")`
#' @param xlab.size `r opt_inf("xlab.size")`
#' @param xlab.color `r opt_inf("xlab.color")`
#' @param xlab.rotation `r opt_inf("xlab.rotation")`
#' @param xlab.space `r opt_inf("xlab.space")`
#' @param xlab.fontface `r opt_inf("xlab.fontface")`
#' @param xlab.fontfamily `r opt_inf("xlab.fontfamily")`
#' @param xlab.side `r opt_inf("xlab.side")`
#' @param ylab.show `r opt_inf("ylab.show")`
#' @param ylab.text `r opt_inf("ylab.text")`
#' @param ylab.size `r opt_inf("ylab.size")`
#' @param ylab.color `r opt_inf("ylab.color")`
#' @param ylab.rotation `r opt_inf("ylab.rotation")`
#' @param ylab.space `r opt_inf("ylab.space")`
#' @param ylab.fontface `r opt_inf("ylab.fontface")`
#' @param ylab.fontfamily `r opt_inf("ylab.fontfamily")`
#' @param ylab.side `r opt_inf("ylab.side")`
#' @param panel.type `r opt_inf("panel.type")`
#' @param panel.wrap.pos `r opt_inf("panel.wrap.pos")`
#' @param panel.xtab.pos `r opt_inf("panel.xtab.pos")`
#' @param unit `r opt_inf("unit")`
#' @param color.sepia_intensity `r opt_inf("color.sepia_intensity")`
#' @param color.saturation `r opt_inf("color.saturation")`
#' @param color_vision_deficiency_sim `r opt_inf("color_vision_deficiency_sim")`
#' @param text.fontface `r opt_inf("text.fontface")`
#' @param text.fontfamily `r opt_inf("text.fontfamily")`
#' @param component.position `r opt_inf("component.position")`
#' @param component.autoscale `r opt_inf("component.autoscale")`
#' @param legend.show `r opt_inf("legend.show")`
#' @param legend.design `r opt_inf("legend.design")`
#' @param legend.orientation `r opt_inf("legend.orientation")`
#' @param legend.position `r opt_inf("legend.position")`
#' @param legend.width `r opt_inf("legend.width")`
#' @param legend.height `r opt_inf("legend.height")`
#' @param legend.stack `r opt_inf("legend.stack")`
#' @param legend.group.frame `r opt_inf("legend.group.frame")`
#' @param legend.resize_as_group `r opt_inf("legend.resize_as_group")`
#' @param legend.reverse `r opt_inf("legend.reverse")`
#' @param legend.na.show `r opt_inf("legend.na.show")`
#' @param legend.title.color `r opt_inf("legend.title.color")`
#' @param legend.title.size `r opt_inf("legend.title.size")`
#' @param legend.title.fontface `r opt_inf("legend.title.fontface")`
#' @param legend.title.fontfamily `r opt_inf("legend.title.fontfamily")`
#' @param legend.xlab.color `r opt_inf("legend.xlab.color")`
#' @param legend.xlab.size `r opt_inf("legend.xlab.size")`
#' @param legend.xlab.fontface `r opt_inf("legend.xlab.fontface")`
#' @param legend.xlab.fontfamily `r opt_inf("legend.xlab.fontfamily")`
#' @param legend.ylab.color `r opt_inf("legend.ylab.color")`
#' @param legend.ylab.size `r opt_inf("legend.ylab.size")`
#' @param legend.ylab.fontface `r opt_inf("legend.ylab.fontface")`
#' @param legend.ylab.fontfamily `r opt_inf("legend.ylab.fontfamily")`
#' @param legend.text.color `r opt_inf("legend.text.color")`
#' @param legend.text.size `r opt_inf("legend.text.size")`
#' @param legend.text.fontface `r opt_inf("legend.text.fontface")`
#' @param legend.text.fontfamily `r opt_inf("legend.text.fontfamily")`
#' @param legend.frame `r opt_inf("legend.frame")`
#' @param legend.frame.lwd `r opt_inf("legend.frame.lwd")`
#' @param legend.frame.r `r opt_inf("legend.frame.r")`
#' @param legend.bg.color `r opt_inf("legend.bg.color")`
#' @param legend.bg.alpha `r opt_inf("legend.bg.alpha")`
#' @param legend.only `r opt_inf("legend.only")`
#' @param legend.settings.standard.portrait `r opt_inf("legend.settings.standard.portrait")`
#' @param legend.settings.standard.landscape `r opt_inf("legend.settings.standard.landscape")`
#' @param chart.show `r opt_inf("chart.show")`
#' @param chart.plot.axis.x `r opt_inf("chart.plot.axis.x")`
#' @param chart.plot.axis.y `r opt_inf("chart.plot.axis.y")`
#' @param chart.position `r opt_inf("chart.position")`
#' @param chart.width `r opt_inf("chart.width")`
#' @param chart.height `r opt_inf("chart.height")`
#' @param chart.stack `r opt_inf("chart.stack")`
#' @param chart.group.frame `r opt_inf("chart.group.frame")`
#' @param chart.resize_as_group `r opt_inf("chart.resize_as_group")`
#' @param chart.reverse `r opt_inf("chart.reverse")`
#' @param chart.na.show `r opt_inf("chart.na.show")`
#' @param chart.title.color `r opt_inf("chart.title.color")`
#' @param chart.title.size `r opt_inf("chart.title.size")`
#' @param chart.title.fontface `r opt_inf("chart.title.fontface")`
#' @param chart.title.fontfamily `r opt_inf("chart.title.fontfamily")`
#' @param chart.xlab.color `r opt_inf("chart.xlab.color")`
#' @param chart.xlab.size `r opt_inf("chart.xlab.size")`
#' @param chart.xlab.fontface `r opt_inf("chart.xlab.fontface")`
#' @param chart.xlab.fontfamily `r opt_inf("chart.xlab.fontfamily")`
#' @param chart.ylab.color `r opt_inf("chart.ylab.color")`
#' @param chart.ylab.size `r opt_inf("chart.ylab.size")`
#' @param chart.ylab.fontface `r opt_inf("chart.ylab.fontface")`
#' @param chart.ylab.fontfamily `r opt_inf("chart.ylab.fontfamily")`
#' @param chart.text.color `r opt_inf("chart.text.color")`
#' @param chart.text.size `r opt_inf("chart.text.size")`
#' @param chart.text.fontface `r opt_inf("chart.text.fontface")`
#' @param chart.text.fontfamily `r opt_inf("chart.text.fontfamily")`
#' @param chart.frame `r opt_inf("chart.frame")`
#' @param chart.frame.lwd `r opt_inf("chart.frame.lwd")`
#' @param chart.frame.r `r opt_inf("chart.frame.r")`
#' @param chart.bg.color `r opt_inf("chart.bg.color")`
#' @param chart.bg.alpha `r opt_inf("chart.bg.alpha")`
#' @param chart.object.color `r opt_inf("chart.object.color")`
#' @param title.show `r opt_inf("title.show")`
#' @param title.size `r opt_inf("title.size")`
#' @param title.color `r opt_inf("title.color")`
#' @param title.fontface `r opt_inf("title.fontface")`
#' @param title.fontfamily `r opt_inf("title.fontfamily")`
#' @param title.bg.color `r opt_inf("title.bg.color")`
#' @param title.bg.alpha `r opt_inf("title.bg.alpha")`
#' @param title.padding `r opt_inf("title.padding")`
#' @param title.frame `r opt_inf("title.frame")`
#' @param title.frame.lwd `r opt_inf("title.frame.lwd")`
#' @param title.frame.r `r opt_inf("title.frame.r")`
#' @param title.stack `r opt_inf("title.stack")`
#' @param title.position `r opt_inf("title.position")`
#' @param title.width `r opt_inf("title.width")`
#' @param title.group.frame `r opt_inf("title.group.frame")`
#' @param title.resize_as_group `r opt_inf("title.resize_as_group")`
#' @param credits.show `r opt_inf("credits.show")`
#' @param credits.size `r opt_inf("credits.size")`
#' @param credits.color `r opt_inf("credits.color")`
#' @param credits.fontface `r opt_inf("credits.fontface")`
#' @param credits.fontfamily `r opt_inf("credits.fontfamily")`
#' @param credits.bg.color `r opt_inf("credits.bg.color")`
#' @param credits.bg.alpha `r opt_inf("credits.bg.alpha")`
#' @param credits.padding `r opt_inf("credits.padding")`
#' @param credits.frame `r opt_inf("credits.frame")`
#' @param credits.frame.lwd `r opt_inf("credits.frame.lwd")`
#' @param credits.frame.r `r opt_inf("credits.frame.r")`
#' @param credits.stack `r opt_inf("credits.stack")`
#' @param credits.position `r opt_inf("credits.position")`
#' @param credits.width `r opt_inf("credits.width")`
#' @param credits.heigth `r opt_inf("credits.heigth")`
#' @param credits.group.frame `r opt_inf("credits.group.frame")`
#' @param credits.resize_as_group `r opt_inf("credits.resize_as_group")`
#' @param compass.north `r opt_inf("compass.north")`
#' @param compass.type `r opt_inf("compass.type")`
#' @param compass.text.size `r opt_inf("compass.text.size")`
#' @param compass.size `r opt_inf("compass.size")`
#' @param compass.show.labels `r opt_inf("compass.show.labels")`
#' @param compass.cardinal.directions `r opt_inf("compass.cardinal.directions")`
#' @param compass.text.color `r opt_inf("compass.text.color")`
#' @param compass.color.dark `r opt_inf("compass.color.dark")`
#' @param compass.color.light `r opt_inf("compass.color.light")`
#' @param compass.lwd `r opt_inf("compass.lwd")`
#' @param compass.bg.color `r opt_inf("compass.bg.color")`
#' @param compass.bg.alpha `r opt_inf("compass.bg.alpha")`
#' @param compass.margins `r opt_inf("compass.margins")`
#' @param compass.show `r opt_inf("compass.show")`
#' @param compass.stack `r opt_inf("compass.stack")`
#' @param compass.position `r opt_inf("compass.position")`
#' @param compass.frame `r opt_inf("compass.frame")`
#' @param compass.frame.lwd `r opt_inf("compass.frame.lwd")`
#' @param compass.frame.r `r opt_inf("compass.frame.r")`
#' @param compass.group.frame `r opt_inf("compass.group.frame")`
#' @param compass.resize_as_group `r opt_inf("compass.resize_as_group")`
#' @param logo.height `r opt_inf("logo.height")`
#' @param logo.margins `r opt_inf("logo.margins")`
#' @param logo.between_margin `r opt_inf("logo.between_margin")`
#' @param logo.show `r opt_inf("logo.show")`
#' @param logo.stack `r opt_inf("logo.stack")`
#' @param logo.position `r opt_inf("logo.position")`
#' @param logo.frame `r opt_inf("logo.frame")`
#' @param logo.frame.lwd `r opt_inf("logo.frame.lwd")`
#' @param logo.frame.r `r opt_inf("logo.frame.r")`
#' @param logo.group.frame `r opt_inf("logo.group.frame")`
#' @param logo.resize_as_group `r opt_inf("logo.resize_as_group")`
#' @param scalebar.show `r opt_inf("scalebar.show")`
#' @param scalebar.breaks `r opt_inf("scalebar.breaks")`
#' @param scalebar.width `r opt_inf("scalebar.width")`
#' @param scalebar.text.size `r opt_inf("scalebar.text.size")`
#' @param scalebar.text.color `r opt_inf("scalebar.text.color")`
#' @param scalebar.color.dark `r opt_inf("scalebar.color.dark")`
#' @param scalebar.color.light `r opt_inf("scalebar.color.light")`
#' @param scalebar.lwd `r opt_inf("scalebar.lwd")`
#' @param scalebar.bg.color `r opt_inf("scalebar.bg.color")`
#' @param scalebar.bg.alpha `r opt_inf("scalebar.bg.alpha")`
#' @param scalebar.size `r opt_inf("scalebar.size")`
#' @param scalebar.margins `r opt_inf("scalebar.margins")`
#' @param scalebar.stack `r opt_inf("scalebar.stack")`
#' @param scalebar.position `r opt_inf("scalebar.position")`
#' @param scalebar.frame `r opt_inf("scalebar.frame")`
#' @param scalebar.frame.lwd `r opt_inf("scalebar.frame.lwd")`
#' @param scalebar.frame.r `r opt_inf("scalebar.frame.r")`
#' @param scalebar.group.frame `r opt_inf("scalebar.group.frame")`
#' @param scalebar.resize_as_group `r opt_inf("scalebar.resize_as_group")`
#' @param grid.show `r opt_inf("grid.show")`
#' @param grid.labels.pos `r opt_inf("grid.labels.pos")`
#' @param grid.x `r opt_inf("grid.x")`
#' @param grid.y `r opt_inf("grid.y")`
#' @param grid.n.x `r opt_inf("grid.n.x")`
#' @param grid.n.y `r opt_inf("grid.n.y")`
#' @param grid.crs `r opt_inf("grid.crs")`
#' @param grid.col `r opt_inf("grid.col")`
#' @param grid.lwd `r opt_inf("grid.lwd")`
#' @param grid.alpha `r opt_inf("grid.alpha")`
#' @param grid.labels.show `r opt_inf("grid.labels.show")`
#' @param grid.labels.size `r opt_inf("grid.labels.size")`
#' @param grid.labels.col `r opt_inf("grid.labels.col")`
#' @param grid.labels.rot `r opt_inf("grid.labels.rot")`
#' @param grid.labels.format `r opt_inf("grid.labels.format")`
#' @param grid.labels.cardinal `r opt_inf("grid.labels.cardinal")`
#' @param grid.labels.margin.x `r opt_inf("grid.labels.margin.x")`
#' @param grid.labels.margin.y `r opt_inf("grid.labels.margin.y")`
#' @param grid.labels.space.x `r opt_inf("grid.labels.space.x")`
#' @param grid.labels.space.y `r opt_inf("grid.labels.space.y")`
#' @param grid.labels.inside_frame `r opt_inf("grid.labels.inside_frame")`
#' @param grid.ticks `r opt_inf("grid.ticks")`
#' @param grid.lines `r opt_inf("grid.lines")`
#' @param grid.ndiscr `r opt_inf("grid.ndiscr")`
#' @param mouse_coordinates.stack `r opt_inf("mouse_coordinates.stack")`
#' @param mouse_coordinates.position `r opt_inf("mouse_coordinates.position")`
#' @param mouse_coordinates.show `r opt_inf("mouse_coordinates.show")`
#' @param minimap.server `r opt_inf("minimap.server")`
#' @param minimap.toggle `r opt_inf("minimap.toggle")`
#' @param minimap.stack `r opt_inf("minimap.stack")`
#' @param minimap.position `r opt_inf("minimap.position")`
#' @param minimap.show `r opt_inf("minimap.show")`
#' @param panel.show `r opt_inf("panel.show")`
#' @param panel.labels `r opt_inf("panel.labels")`
#' @param panel.label.size `r opt_inf("panel.label.size")`
#' @param panel.label.color `r opt_inf("panel.label.color")`
#' @param panel.label.fontface `r opt_inf("panel.label.fontface")`
#' @param panel.label.fontfamily `r opt_inf("panel.label.fontfamily")`
#' @param panel.label.bg.color `r opt_inf("panel.label.bg.color")`
#' @param panel.label.frame `r opt_inf("panel.label.frame")`
#' @param panel.label.frame.lwd `r opt_inf("panel.label.frame.lwd")`
#' @param panel.label.frame.r `r opt_inf("panel.label.frame.r")`
#' @param panel.label.height `r opt_inf("panel.label.height")`
#' @param panel.label.rot `r opt_inf("panel.label.rot")`
#' @param bbox `r opt_inf("bbox")`
#' @param set.bounds `r opt_inf("set.bounds")`
#' @param set.view `r opt_inf("set.view")`
#' @param set.zoom.limits `r opt_inf("set.zoom.limits")`
#' @param qtm.scalebar `r opt_inf("qtm.scalebar")`
#' @param qtm.minimap `r opt_inf("qtm.minimap")`
#' @param qtm.mouse_coordinates `r opt_inf("qtm.mouse_coordinates")`
#' @param earth.boundary `r opt_inf("earth.boundary")`
#' @param earth.boundary.color `r opt_inf("earth.boundary.color")`
#' @param earth.boundary.lwd `r opt_inf("earth.boundary.lwd")`
#' @param earth.datum `r opt_inf("earth.datum")`
#' @param space.color `r opt_inf("space.color")`
#' @param check_and_fix `r opt_inf("check_and_fix")`
#' @param basemap.show `r opt_inf("basemap.show")`
#' @param basemap.server `r opt_inf("basemap.server")`
#' @param basemap.alpha `r opt_inf("basemap.alpha")`
#' @param basemap.zoom `r opt_inf("basemap.zoom")`
#' @param tiles.show `r opt_inf("tiles.show")`
#' @param tiles.server `r opt_inf("tiles.server")`
#' @param tiles.alpha `r opt_inf("tiles.alpha")`
#' @param tiles.zoom `r opt_inf("tiles.zoom")`
#' @param attr.color `r opt_inf("attr.color")`
#' @param title,main.title deprecated See [tm_title()]
#' @param view.legend.position deprecated. Use `legend.position` instead.
#' @name tmap_options
#' @rdname tmap_options
#' @export
tmap_options = function(..., crs, facet.max, facet.flip, free.scales, raster.max_cells, show.messages, show.warnings, output.format, output.size, output.dpi, output.dpi.animation, value.const, value.na, value.null, value.blank, values.var, values.range, value.neutral, values.scale, scales.var, scale.misc.args, continuous.nclass_per_legend_break, continuous.nclasses, label.format, label.na, scale, asp, bg.color, outer.bg.color, frame, frame.lwd, frame.r, frame.double_line, outer.margins, inner.margins, inner.margins.extra, meta.margins, meta.auto_margins, between_margin, panel.margin, component.offset, component.stack_margin, grid.mark.height, xylab.height, coords.height, xlab.show, xlab.text, xlab.size, xlab.color, xlab.rotation, xlab.space, xlab.fontface, xlab.fontfamily, xlab.side, ylab.show, ylab.text, ylab.size, ylab.color, ylab.rotation, ylab.space, ylab.fontface, ylab.fontfamily, ylab.side, panel.type, panel.wrap.pos, panel.xtab.pos, unit, color.sepia_intensity, color.saturation, color_vision_deficiency_sim, text.fontface, text.fontfamily, component.position, component.autoscale, legend.show, legend.design, legend.orientation, legend.position, legend.width, legend.height, legend.stack, legend.group.frame, legend.resize_as_group, legend.reverse, legend.na.show, legend.title.color, legend.title.size, legend.title.fontface, legend.title.fontfamily, legend.xlab.color, legend.xlab.size, legend.xlab.fontface, legend.xlab.fontfamily, legend.ylab.color, legend.ylab.size, legend.ylab.fontface, legend.ylab.fontfamily, legend.text.color, legend.text.size, legend.text.fontface, legend.text.fontfamily, legend.frame, legend.frame.lwd, legend.frame.r, legend.bg.color, legend.bg.alpha, legend.only, legend.settings.standard.portrait, legend.settings.standard.landscape, chart.show, chart.plot.axis.x, chart.plot.axis.y, chart.position, chart.width, chart.height, chart.stack, chart.group.frame, chart.resize_as_group, chart.reverse, chart.na.show, chart.title.color, chart.title.size, chart.title.fontface, chart.title.fontfamily, chart.xlab.color, chart.xlab.size, chart.xlab.fontface, chart.xlab.fontfamily, chart.ylab.color, chart.ylab.size, chart.ylab.fontface, chart.ylab.fontfamily, chart.text.color, chart.text.size, chart.text.fontface, chart.text.fontfamily, chart.frame, chart.frame.lwd, chart.frame.r, chart.bg.color, chart.bg.alpha, chart.object.color, title.show, title.size, title.color, title.fontface, title.fontfamily, title.bg.color, title.bg.alpha, title.padding, title.frame, title.frame.lwd, title.frame.r, title.stack, title.position, title.width, title.group.frame, title.resize_as_group, credits.show, credits.size, credits.color, credits.fontface, credits.fontfamily, credits.bg.color, credits.bg.alpha, credits.padding, credits.frame, credits.frame.lwd, credits.frame.r, credits.stack, credits.position, credits.width, credits.heigth, credits.group.frame, credits.resize_as_group, compass.north, compass.type, compass.text.size, compass.size, compass.show.labels, compass.cardinal.directions, compass.text.color, compass.color.dark, compass.color.light, compass.lwd, compass.bg.color, compass.bg.alpha, compass.margins, compass.show, compass.stack, compass.position, compass.frame, compass.frame.lwd, compass.frame.r, compass.group.frame, compass.resize_as_group, logo.height, logo.margins, logo.between_margin, logo.show, logo.stack, logo.position, logo.frame, logo.frame.lwd, logo.frame.r, logo.group.frame, logo.resize_as_group, scalebar.show, scalebar.breaks, scalebar.width, scalebar.text.size, scalebar.text.color, scalebar.color.dark, scalebar.color.light, scalebar.lwd, scalebar.bg.color, scalebar.bg.alpha, scalebar.size, scalebar.margins, scalebar.stack, scalebar.position, scalebar.frame, scalebar.frame.lwd, scalebar.frame.r, scalebar.group.frame, scalebar.resize_as_group, grid.show, grid.labels.pos, grid.x, grid.y, grid.n.x, grid.n.y, grid.crs, grid.col, grid.lwd, grid.alpha, grid.labels.show, grid.labels.size, grid.labels.col, grid.labels.rot, grid.labels.format, grid.labels.cardinal, grid.labels.margin.x, grid.labels.margin.y, grid.labels.space.x, grid.labels.space.y, grid.labels.inside_frame, grid.ticks, grid.lines, grid.ndiscr, mouse_coordinates.stack, mouse_coordinates.position, mouse_coordinates.show, minimap.server, minimap.toggle, minimap.stack, minimap.position, minimap.show, panel.show, panel.labels, panel.label.size, panel.label.color, panel.label.fontface, panel.label.fontfamily, panel.label.bg.color, panel.label.frame, panel.label.frame.lwd, panel.label.frame.r, panel.label.height, panel.label.rot, bbox, set.bounds, set.view, set.zoom.limits, qtm.scalebar, qtm.minimap, qtm.mouse_coordinates, earth.boundary, earth.boundary.color, earth.boundary.lwd, earth.datum, space.color, check_and_fix, basemap.show, basemap.server, basemap.alpha, basemap.zoom, tiles.show, tiles.server, tiles.alpha, tiles.zoom, attr.color,
						title = NULL,
						main.title = NULL,
						view.legend.position) {
	o = get("tmapOptions", envir = .TMAP)
	nms = names(o)
	show.warnings = o$show.warnings

	## case 1: option list is given. E.g. opts = list(crs = 3035, panel.show = TRUE); tmap_options(opts)
	## case 2: option name is given. E.g. tmap_options("crs", "panel.show")
	## case 3: named options are set. E.g. tmap_options(crs = 3035, panel.show = TRUE)
	## case 4: tmap_options is called without arguments


	# get current style name (default: white), and set new style name (with "(modified)")
	sty_cur = getOption("tmap.style")
	sty_new = if (substr(sty_cur, nchar(sty_cur) - 9, nchar(sty_cur)) == "(modified)") sty_cur else paste(sty_cur, "(modified)")

	args = lapply(as.list(rlang::call_match(dots_expand = TRUE)[-1]), eval, envir = parent.frame())
	set_new_style = FALSE

	#lst = list(...)
	if (length(args) >= 1 && is.null(names(args[1]))) {
		arg = args[[1]]
		if (is.list(arg)) {
			if (length(args) > 1 && show.warnings) warning("Only the first argument is used; the other arguments are ignored.")
			## case 1: option list is given
			args = arg

			style_attr = attr(args, "style")
			if (!is.null(style_attr)) {
				sty_new = style_attr
				set_new_style = TRUE
			}

		} else {
			## case 2: option name is given
			args = sapply(args, "[", 1)
			if (!all(args %in% nms) && show.warnings) warning("The following options do not exist: ", paste(setdiff(args, nms), collapse = ", "))
			args = intersect(args, nms)
			return(o[args])
		}
	}

	mode_opts = setdiff(unique(unlist(lapply(o$modes, names))), "name")

	all_opts = union(mode_opts, names(.defaultTmapOptions))

	unknown_args = setdiff(names(args), all_opts)
	if (length(unknown_args) == 1) {
		stop("the following option does not exist: ", unknown_args)
	} else if (length(unknown_args) > 1) {
		stop("the following options do not exist: ", paste(unknown_args, collapse = ", "))
	}

	if (!length(args)) {
		# case 4
		return(o)
	} else {
		# case 1 and 3
		if (is.null(names(args))) {
			stop("Option list is unnamed")
		}
		backup = o[names(args)]
		o[names(args)] = args # check_named_items(args, backup)

		options(tmap.style=sty_new)
		attr(o, "style") = sty_new
		attr(o, "specified") = names(args)
		assign("tmapOptions", o, envir = .TMAP)

		if (set_new_style) {
			if (o$show.messages) message("tmap options successfully loaded as style \"", sty_new, "\"")
			styles = get("tmapStyles", envir = .TMAP)
			styles[[sty_new]] = suppressMessages(tmap_options_diff())
			assign("tmapStyles", styles, envir = .TMAP)
		}
		invisible(backup)
	}
}

#' @name tmap_options_mode
#' @param mode mode, e.g. `"plot"` or `"view"`
#' @param default.options return the default options or the current options?
#' @rdname tmap_options
#' @export
tmap_options_mode = function(mode = NA, default.options = FALSE) {
	o = if (default.options) .defaultTmapOptions else get("tmapOptions", envir = .TMAP)

	if (is.na(mode)) mode = getOption("tmap.mode")
	opt2 = o$modes[[mode]]

	specified = attr(o, "specified")
	int_opt = setdiff(intersect(names(o), names(opt2)), specified)
	diff_opt = setdiff(names(opt2), names(o))

	if (length(int_opt)) o[int_opt] = opt2[int_opt]
	if (length(diff_opt)) o = c(o, opt2[diff_opt])
	o
}


tmap_option = function(name, type = NULL) {
	get_option_class(tmap_options()[[name]], class = type, spatial_class = FALSE)
}



tmap_graphics_name = function(mode) {
	if (missing(mode)) mode = getOption("tmap.mode")
	get("tmapOptions", envir = .TMAP)$modes[[mode]]$name
}

tmapOption = function(...) {
	structure(list(...), class = "tmapOption")
}




#' Internal tmap function to add a default value for the layer functions
#'
#' Internal tmap function to add a default value for the layer functions
#'
#' @param option, one of: `"value.const"`, `"value.na"`, `"value.blank"`, `"values.var"`, `'values.range'`, `"value.neutral"`, `"scales.var"`
#' @param id name of the visual variable with layer, in the format `"x.y"`,
#'   where `x` is the visual variable and `y` is the layer.
#'   It is also possible to set `x` only; then it applies to all layer functions.
#' @param value value
#' @keywords internal
#' @export
tmapAddLayerOptions = function(option, id, value) {
	if (!(option %in% c("value.const", "value.na", "value.blank", "values.var", "values.range", "value.neutral", "scales.var"))) {
		stop("Unknown option")
	}
	o = tmap_option(option)
	o[[id]] = value
	o2 = structure(list(o), names = option)
	tmap_options(o2)
}

#' @rdname tmap_options
#' @export
tmap_options_diff <- function() {
	.tmapOptions <- get("tmapOptions", envir = .TMAP)
	iden <- mapply(identical, .tmapOptions, .defaultTmapOptions)

	if (all(iden)) {
		message("current tmap options are similar to the default tmap options (style \"white\")")
	} else {
		message("current tmap options (style \"", attr(.tmapOptions, "style"), "\") that are different from default tmap options (style \"white\"):")
		.tmapOptions[!iden]
	}
}

#' @rdname tmap_options
#' @export
tmap_options_reset <- function() {
	assign("tmapOptions", .defaultTmapOptions, envir = .TMAP)
	options(tmap.style="white")
	message("tmap options successfully reset")
	invisible(NULL)
}

#' @export
#' @param style style, see [tmap_style()] for available styles
#' @rdname tmap_options
tmap_options_save <- function(style) {
	show.messages <- get("tmapOptions", envir = .TMAP)$show.messages

	stylediff <- suppressMessages(tmap_options_diff())

	.tmapOptions <- get("tmapOptions", envir = .TMAP)

	if (is.null(stylediff)) {
		if (show.messages) message("current style is the same as the default style, so nothing to save")
		return(invisible(.tmapOptions))
	}

	options(tmap.style=style)
	attr(.tmapOptions, "style") <- style
	assign("tmapOptions", .tmapOptions, envir = .TMAP)

	styles <- get("tmapStyles", envir = .TMAP)
	styles[[style]] <- suppressMessages(tmap_options_diff())
	assign("tmapStyles", styles, envir = .TMAP)

	if (show.messages) message("current tmap options saved as style \"", style, "\"")
	invisible(.tmapOptions)
}



#' Internal method for submitting a new mode
#'
#' Internal method for submitting a new mode
#'
#' @param id id of the mode: please use lowercase and one-word. This will be used with [tmap_mode()].
#' @param name name of the mode: please use title case. This will be used to recognize internal functions, e.g. `tmapLeafletInit`
#' @param ... mode specific options
#' @export
#' @keywords internal
tmapMode = function(id, name, ...) {
	modes = tmap_options("modes")$modes

	modes[[id]] = c(list(name = name), list(...))
	tmap_options(modes = modes)
}


