# To generate all options:
# args = intersect(names(tmap_options_mode("view")),names(tmap_options_mode("plot")))
# cat(paste(args, collapse = ", "))
# x = sapply(args, function(a) paste0("#' @param ", a, " `r .doc_opt(\"", a, "\")`"))
# cat(paste(x, collapse = "\n"))
#
# To generate option topics (the 'what', last part)
# nms = names(tmap_options())
# nms_s = strsplit(nms, split = ".", fixed = TRUE)
# nms_lst = sapply(nms_s, tail, 1)
# whats = sort(unique(nms_lst))
# cat(paste0("c(", paste(sapply(whats, function(w) paste0(w, " = \"", w, "\"")), collapse = ",\n"), ")"))
#' tmap options
#'
#' Get or set the tmap options globally. For map specific options, we recommend to use [tm_options()] or [tm_layout()] via which the layout-related options can be set. [tmap_options()] functions similar to [base::options()].
#'
#' @param ... List of tmap options to be set, or option names (characters) to be returned (see details)
#' @param crs Map crs (see [tm_shape()]). `NA` means the crs is specified in [tm_shape()]. The crs that is used by the transformation functions is defined in [tm_shape()].
#' @param facet.max Maximum number of facets
#' @param facet.flip Should facets be flipped (in case of facet wrap)? This can also be set via [tm_facets_flip()]
#' @param free.scales For backward compatibility: if this value is set, it will be used to impute the free arguments in the layer functions
#' @param raster.max_cells Maximum number of raster grid cells. Can be mode specific `c(plot = 3000, view = 1000, 1000)` (the last value is the fall back default)
#' @param raster.warp Should rasters be warped or transformed in case a different projection (crs) is used? Warping creates a new regular raster in the target crs, whereas transforming creates a (usually non-regular) raster in the target crs. The former is lossy, but much faster and is therefore the default.
#' When a different projection (crs) is used, a (usually) regular raster will be
#' @param show.messages Show messages?
#' @param show.warnings Show warnings?
#' @param output.format Output format
#' @param output.size Output size
#' @param output.dpi Output dpi
#' @param animation.dpi Output dpi for animations
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
#' @param continuous.nclass_per_legend_break The number of continuous legend breaks within one 'unit' (label).  The default value is 50.
#' @param continuous.nclasses the number of classes of a continuous scale. Should be odd.  The default value is 101.
#' @param label.format Format for the labels (was `legend.format` in tmap v3).
#' @param label.na Default label for missing values.
#' @param scale Overall scale of the map
#' @param asp Aspect ratio of each map. When `asp` is set to `NA` (default) the aspect ratio will be adjusted to the used shapes. When set to 0 the aspect ratio is
#'   adjusted to the size of the device divided by the number of columns and rows.
#' @param bg.color Background color of the map.
#' @param outer.bg.color Background color of map outside the frame.
#' @param frame Overall frame of the map
#' @param frame.lwd `r .doc_opt("frame.lwd")`
#' @param frame.r `r .doc_opt("frame.r")`
#' @param frame.double_line `r .doc_opt("frame.double_line")`
#' @param outer.margins `r .doc_opt("outer.margins")`
#' @param inner.margins `r .doc_opt("inner.margins")`
#' @param inner.margins.extra `r .doc_opt("inner.margins.extra")`
#' @param meta.margins `r .doc_opt("meta.margins")`
#' @param meta.auto_margins `r .doc_opt("meta.auto_margins")`
#' @param between_margin Margin between the map.
#' @param panel.margin `r .doc_opt("panel.margin")`
#' @param component.offset `r .doc_opt("component.offset")`
#' @param component.stack_margin `r .doc_opt("component.stack_margin")`
#' @param grid.mark.height `r .doc_opt("grid.mark.height")`
#' @param xylab.height `r .doc_opt("xylab.height")`
#' @param coords.height `r .doc_opt("coords.height")`
#' @param xlab.show `r .doc_opt("xlab.show")`
#' @param xlab.text `r .doc_opt("xlab.text")`
#' @param xlab.size `r .doc_opt("xlab.size")`
#' @param xlab.color `r .doc_opt("xlab.color")`
#' @param xlab.rotation `r .doc_opt("xlab.rotation")`
#' @param xlab.space `r .doc_opt("xlab.space")`
#' @param xlab.fontface `r .doc_opt("xlab.fontface")`
#' @param xlab.fontfamily `r .doc_opt("xlab.fontfamily")`
#' @param xlab.alpha `r .doc_opt("xlab.alpha")`
#' @param xlab.side `r .doc_opt("xlab.side")`
#' @param ylab.show `r .doc_opt("ylab.show")`
#' @param ylab.text `r .doc_opt("ylab.text")`
#' @param ylab.size `r .doc_opt("ylab.size")`
#' @param ylab.color `r .doc_opt("ylab.color")`
#' @param ylab.rotation `r .doc_opt("ylab.rotation")`
#' @param ylab.space `r .doc_opt("ylab.space")`
#' @param ylab.fontface `r .doc_opt("ylab.fontface")`
#' @param ylab.fontfamily `r .doc_opt("ylab.fontfamily")`
#' @param ylab.alpha `r .doc_opt("ylab.alpha")`
#' @param ylab.side `r .doc_opt("ylab.side")`
#' @param panel.type `r .doc_opt("panel.type")`
#' @param panel.wrap.pos The panel positions for wrapped facets created with [tm_facets_grid()]. One of `"left"`, `"right"`, `"top"` (default) or `"bottom"`.
#' @param panel.xtab.pos The panel positions for grid facets created with [tm_facets_grid()]. Vector of two, where the first determines the locations of row panels (`"left"` or `"right"`) and the second the location of column panels ( `"top"` or `"bottom")
#' @param unit Unit of the coordinate
#' @param color.sepia_intensity `r .doc_opt("color.sepia_intensity")`
#' @param color.saturation `r .doc_opt("color.saturation")`
#' @param color_vision_deficiency_sim `Color vision deficiency simulation
#' @param text.fontface `r .doc_opt("text.fontface")`
#' @param text.fontfamily `r .doc_opt("text.fontfamily")`
#' @param text.alpha `r .doc_opt("text.alpha")`
#' @param component.position `r .doc_opt("component.position")`
#' @param component.autoscale `r .doc_opt("component.autoscale")`
#' @param legend.show `r .doc_opt("legend.show")`
#' @param legend.design `r .doc_opt("legend.design")`
#' @param legend.orientation `r .doc_opt("legend.orientation")`
#' @param legend.position `r .doc_opt("legend.position")`
#' @param legend.width `r .doc_opt("legend.width")`
#' @param legend.height `r .doc_opt("legend.height")`
#' @param legend.stack `r .doc_opt("legend.stack")`
#' @param legend.group.frame `r .doc_opt("legend.group.frame")`
#' @param legend.resize_as_group `r .doc_opt("legend.resize_as_group")`
#' @param legend.reverse `r .doc_opt("legend.reverse")`
#' @param legend.na.show `r .doc_opt("legend.na.show")`
#' @param legend.title.color `r .doc_opt("legend.title.color")`
#' @param legend.title.size `r .doc_opt("legend.title.size")`
#' @param legend.title.fontface `r .doc_opt("legend.title.fontface")`
#' @param legend.title.fontfamily `r .doc_opt("legend.title.fontfamily")`
#' @param legend.title.alpha `r .doc_opt("legend.title.alpha")`
#' @param legend.xlab.color `r .doc_opt("legend.xlab.color")`
#' @param legend.xlab.size `r .doc_opt("legend.xlab.size")`
#' @param legend.xlab.fontface `r .doc_opt("legend.xlab.fontface")`
#' @param legend.xlab.fontfamily `r .doc_opt("legend.xlab.fontfamily")`
#' @param legend.xlab.alpha `r .doc_opt("legend.xlab.alpha")`
#' @param legend.ylab.color `r .doc_opt("legend.ylab.color")`
#' @param legend.ylab.size `r .doc_opt("legend.ylab.size")`
#' @param legend.ylab.fontface `r .doc_opt("legend.ylab.fontface")`
#' @param legend.ylab.fontfamily `r .doc_opt("legend.ylab.fontfamily")`
#' @param legend.ylab.alpha `r .doc_opt("legend.ylab.alpha")`
#' @param legend.text.color `r .doc_opt("legend.text.color")`
#' @param legend.text.size `r .doc_opt("legend.text.size")`
#' @param legend.text.fontface `r .doc_opt("legend.text.fontface")`
#' @param legend.text.fontfamily `r .doc_opt("legend.text.fontfamily")`
#' @param legend.text.alpha `r .doc_opt("legend.text.alpha")`
#' @param legend.frame `r .doc_opt("legend.frame")`
#' @param legend.frame.lwd `r .doc_opt("legend.frame.lwd")`
#' @param legend.frame.r `r .doc_opt("legend.frame.r")`
#' @param legend.bg.color `r .doc_opt("legend.bg.color")`
#' @param legend.bg.alpha `r .doc_opt("legend.bg.alpha")`
#' @param legend.only `r .doc_opt("legend.only")`
#' @param legend.absolute_fontsize `r .doc_opt("legend.absolute_fontsize")`
#' @param legend.settings.standard.portrait `r .doc_opt("legend.settings.standard.portrait")`
#' @param legend.settings.standard.landscape `r .doc_opt("legend.settings.standard.landscape")`
#' @param chart.show `r .doc_opt("chart.show")`
#' @param chart.plot.axis.x `r .doc_opt("chart.plot.axis.x")`
#' @param chart.plot.axis.y `r .doc_opt("chart.plot.axis.y")`
#' @param chart.position `r .doc_opt("chart.position")`
#' @param chart.width `r .doc_opt("chart.width")`
#' @param chart.height `r .doc_opt("chart.height")`
#' @param chart.stack `r .doc_opt("chart.stack")`
#' @param chart.group.frame `r .doc_opt("chart.group.frame")`
#' @param chart.resize_as_group `r .doc_opt("chart.resize_as_group")`
#' @param chart.reverse `r .doc_opt("chart.reverse")`
#' @param chart.na.show `r .doc_opt("chart.na.show")`
#' @param chart.title.color `r .doc_opt("chart.title.color")`
#' @param chart.title.size `r .doc_opt("chart.title.size")`
#' @param chart.title.fontface `r .doc_opt("chart.title.fontface")`
#' @param chart.title.fontfamily `r .doc_opt("chart.title.fontfamily")`
#' @param chart.title.alpha `r .doc_opt("chart.title.alpha")`
#' @param chart.xlab.color `r .doc_opt("chart.xlab.color")`
#' @param chart.xlab.size `r .doc_opt("chart.xlab.size")`
#' @param chart.xlab.fontface `r .doc_opt("chart.xlab.fontface")`
#' @param chart.xlab.fontfamily `r .doc_opt("chart.xlab.fontfamily")`
#' @param chart.xlab.alpha `r .doc_opt("chart.xlab.alpha")`
#' @param chart.ylab.color `r .doc_opt("chart.ylab.color")`
#' @param chart.ylab.size `r .doc_opt("chart.ylab.size")`
#' @param chart.ylab.fontface `r .doc_opt("chart.ylab.fontface")`
#' @param chart.ylab.fontfamily `r .doc_opt("chart.ylab.fontfamily")`
#' @param chart.ylab.alpha `r .doc_opt("chart.ylab.alpha")`
#' @param chart.text.color `r .doc_opt("chart.text.color")`
#' @param chart.text.size `r .doc_opt("chart.text.size")`
#' @param chart.text.fontface `r .doc_opt("chart.text.fontface")`
#' @param chart.text.fontfamily `r .doc_opt("chart.text.fontfamily")`
#' @param chart.text.alpha `r .doc_opt("chart.text.alpha")`
#' @param chart.frame `r .doc_opt("chart.frame")`
#' @param chart.frame.lwd `r .doc_opt("chart.frame.lwd")`
#' @param chart.frame.r `r .doc_opt("chart.frame.r")`
#' @param chart.bg.color `r .doc_opt("chart.bg.color")`
#' @param chart.bg.alpha `r .doc_opt("chart.bg.alpha")`
#' @param chart.object.color `r .doc_opt("chart.object.color")`
#' @param title.size `r .doc_opt("title.size")`
#' @param title.color `r .doc_opt("title.color")`
#' @param title.fontface `r .doc_opt("title.fontface")`
#' @param title.fontfamily `r .doc_opt("title.fontfamily")`
#' @param title.alpha `r .doc_opt("title.alpha")`
#' @param title.bg.color `r .doc_opt("title.bg.color")`
#' @param title.bg.alpha `r .doc_opt("title.bg.alpha")`
#' @param title.padding `r .doc_opt("title.padding")`
#' @param title.frame `r .doc_opt("title.frame")`
#' @param title.frame.lwd `r .doc_opt("title.frame.lwd")`
#' @param title.frame.r `r .doc_opt("title.frame.r")`
#' @param title.stack `r .doc_opt("title.stack")`
#' @param title.position `r .doc_opt("title.position")`
#' @param title.width `r .doc_opt("title.width")`
#' @param title.group.frame `r .doc_opt("title.group.frame")`
#' @param title.resize_as_group `r .doc_opt("title.resize_as_group")`
#' @param credits.size `r .doc_opt("credits.size")`
#' @param credits.color `r .doc_opt("credits.color")`
#' @param credits.fontface `r .doc_opt("credits.fontface")`
#' @param credits.fontfamily `r .doc_opt("credits.fontfamily")`
#' @param credits.alpha `r .doc_opt("credits.alpha")`
#' @param credits.bg.color `r .doc_opt("credits.bg.color")`
#' @param credits.bg.alpha `r .doc_opt("credits.bg.alpha")`
#' @param credits.padding `r .doc_opt("credits.padding")`
#' @param credits.frame `r .doc_opt("credits.frame")`
#' @param credits.frame.lwd `r .doc_opt("credits.frame.lwd")`
#' @param credits.frame.r `r .doc_opt("credits.frame.r")`
#' @param credits.stack `r .doc_opt("credits.stack")`
#' @param credits.position `r .doc_opt("credits.position")`
#' @param credits.width `r .doc_opt("credits.width")`
#' @param credits.height `r .doc_opt("credits.height")`
#' @param credits.group.frame `r .doc_opt("credits.group.frame")`
#' @param credits.resize_as_group `r .doc_opt("credits.resize_as_group")`
#' @param compass.north `r .doc_opt("compass.north")`
#' @param compass.type `r .doc_opt("compass.type")`
#' @param compass.text.size `r .doc_opt("compass.text.size")`
#' @param compass.size `r .doc_opt("compass.size")`
#' @param compass.show.labels `r .doc_opt("compass.show.labels")`
#' @param compass.cardinal.directions `r .doc_opt("compass.cardinal.directions")`
#' @param compass.text.color `r .doc_opt("compass.text.color")`
#' @param compass.color.dark `r .doc_opt("compass.color.dark")`
#' @param compass.color.light `r .doc_opt("compass.color.light")`
#' @param compass.lwd `r .doc_opt("compass.lwd")`
#' @param compass.bg.color `r .doc_opt("compass.bg.color")`
#' @param compass.bg.alpha `r .doc_opt("compass.bg.alpha")`
#' @param compass.margins `r .doc_opt("compass.margins")`
#' @param compass.stack `r .doc_opt("compass.stack")`
#' @param compass.position `r .doc_opt("compass.position")`
#' @param compass.frame `r .doc_opt("compass.frame")`
#' @param compass.frame.lwd `r .doc_opt("compass.frame.lwd")`
#' @param compass.frame.r `r .doc_opt("compass.frame.r")`
#' @param compass.group.frame `r .doc_opt("compass.group.frame")`
#' @param compass.resize_as_group `r .doc_opt("compass.resize_as_group")`
#' @param logo.height `r .doc_opt("logo.height")`
#' @param logo.margins `r .doc_opt("logo.margins")`
#' @param logo.between_margin `r .doc_opt("logo.between_margin")`
#' @param logo.stack `r .doc_opt("logo.stack")`
#' @param logo.position `r .doc_opt("logo.position")`
#' @param logo.frame `r .doc_opt("logo.frame")`
#' @param logo.frame.lwd `r .doc_opt("logo.frame.lwd")`
#' @param logo.frame.r `r .doc_opt("logo.frame.r")`
#' @param logo.group.frame `r .doc_opt("logo.group.frame")`
#' @param logo.resize_as_group `r .doc_opt("logo.resize_as_group")`
#' @param scalebar.breaks `r .doc_opt("scalebar.breaks")`
#' @param scalebar.width `r .doc_opt("scalebar.width")`
#' @param scalebar.text.size `r .doc_opt("scalebar.text.size")`
#' @param scalebar.text.color `r .doc_opt("scalebar.text.color")`
#' @param scalebar.color.dark `r .doc_opt("scalebar.color.dark")`
#' @param scalebar.color.light `r .doc_opt("scalebar.color.light")`
#' @param scalebar.lwd `r .doc_opt("scalebar.lwd")`
#' @param scalebar.bg.color `r .doc_opt("scalebar.bg.color")`
#' @param scalebar.bg.alpha `r .doc_opt("scalebar.bg.alpha")`
#' @param scalebar.size `r .doc_opt("scalebar.size")`
#' @param scalebar.margins `r .doc_opt("scalebar.margins")`
#' @param scalebar.stack `r .doc_opt("scalebar.stack")`
#' @param scalebar.position `r .doc_opt("scalebar.position")`
#' @param scalebar.frame `r .doc_opt("scalebar.frame")`
#' @param scalebar.frame.lwd `r .doc_opt("scalebar.frame.lwd")`
#' @param scalebar.frame.r `r .doc_opt("scalebar.frame.r")`
#' @param scalebar.group.frame `r .doc_opt("scalebar.group.frame")`
#' @param scalebar.resize_as_group `r .doc_opt("scalebar.resize_as_group")`
#' @param grid.show `r .doc_opt("grid.show")`
#' @param grid.labels.pos `r .doc_opt("grid.labels.pos")`
#' @param grid.x `r .doc_opt("grid.x")`
#' @param grid.y `r .doc_opt("grid.y")`
#' @param grid.n.x `r .doc_opt("grid.n.x")`
#' @param grid.n.y `r .doc_opt("grid.n.y")`
#' @param grid.crs `r .doc_opt("grid.crs")`
#' @param grid.col `r .doc_opt("grid.col")`
#' @param grid.lwd `r .doc_opt("grid.lwd")`
#' @param grid.alpha `r .doc_opt("grid.alpha")`
#' @param grid.labels.show `r .doc_opt("grid.labels.show")`
#' @param grid.labels.size `r .doc_opt("grid.labels.size")`
#' @param grid.labels.col `r .doc_opt("grid.labels.col")`
#' @param grid.labels.rot `r .doc_opt("grid.labels.rot")`
#' @param grid.labels.format `r .doc_opt("grid.labels.format")`
#' @param grid.labels.cardinal `r .doc_opt("grid.labels.cardinal")`
#' @param grid.labels.margin.x `r .doc_opt("grid.labels.margin.x")`
#' @param grid.labels.margin.y `r .doc_opt("grid.labels.margin.y")`
#' @param grid.labels.space.x `r .doc_opt("grid.labels.space.x")`
#' @param grid.labels.space.y `r .doc_opt("grid.labels.space.y")`
#' @param grid.labels.inside_frame `r .doc_opt("grid.labels.inside_frame")`
#' @param grid.ticks `r .doc_opt("grid.ticks")`
#' @param grid.lines `r .doc_opt("grid.lines")`
#' @param grid.ndiscr `r .doc_opt("grid.ndiscr")`
#' @param mouse_coordinates.stack `r .doc_opt("mouse_coordinates.stack")`
#' @param mouse_coordinates.position `r .doc_opt("mouse_coordinates.position")`
#' @param mouse_coordinates.show `r .doc_opt("mouse_coordinates.show")`
#' @param minimap.server `r .doc_opt("minimap.server")`
#' @param minimap.toggle `r .doc_opt("minimap.toggle")`
#' @param minimap.stack `r .doc_opt("minimap.stack")`
#' @param minimap.position `r .doc_opt("minimap.position")`
#' @param minimap.show `r .doc_opt("minimap.show")`
#' @param panel.show `r .doc_opt("panel.show")`
#' @param panel.labels `r .doc_opt("panel.labels")`
#' @param panel.label.size `r .doc_opt("panel.label.size")`
#' @param panel.label.color `r .doc_opt("panel.label.color")`
#' @param panel.label.fontface `r .doc_opt("panel.label.fontface")`
#' @param panel.label.fontfamily `r .doc_opt("panel.label.fontfamily")`
#' @param panel.label.alpha `r .doc_opt("panel.label.alpha")`
#' @param panel.label.bg.color `r .doc_opt("panel.label.bg.color")`
#' @param panel.label.frame `r .doc_opt("panel.label.frame")`
#' @param panel.label.frame.lwd `r .doc_opt("panel.label.frame.lwd")`
#' @param panel.label.frame.r `r .doc_opt("panel.label.frame.r")`
#' @param panel.label.height `r .doc_opt("panel.label.height")`
#' @param panel.label.rot `r .doc_opt("panel.label.rot")`
# For set_bounds, set_view, set_zoom_limits
#' @inheritParams tm_view
#' @param qtm.scalebar `r .doc_opt("qtm.scalebar")`
#' @param qtm.minimap `r .doc_opt("qtm.minimap")`
#' @param qtm.mouse_coordinates `r .doc_opt("qtm.mouse_coordinates")`
#' @param earth_boundary The earth boundary
#' @param earth_boundary.color `r .doc_opt("earth_boundary.color")`
#' @param earth_boundary.lwd `r .doc_opt("earth_boundary.lwd")`
#' @param earth_datum  Earth datum
#' @param space.color `r .doc_opt("space.color")`
#' @param check_and_fix Should attempt to fix an invalid shapefile
#' @param basemap.show `r .doc_opt("basemap.show")`
#' @param basemap.server `r .doc_opt("basemap.server")`
#' @param basemap.alpha `r .doc_opt("basemap.alpha")`
#' @param basemap.zoom `r .doc_opt("basemap.zoom")`
#' @param tiles.show `r .doc_opt("tiles.show")`
#' @param tiles.server `r .doc_opt("tiles.server")`
#' @param tiles.alpha `r .doc_opt("tiles.alpha")`
#' @param tiles.zoom `r .doc_opt("tiles.zoom")`
#' @param attr.color `r .doc_opt("attr.color")`
#' @param crs_extra Only used internally (work in progress)
#' @param crs_global The used crs for world maps
#' @param title deprecated See [tm_title()]
#' @param main.title deprecated See [tm_title()]
#' @param main.title.size,main.title.color,main.title.fontface,main.title.fontfamily,main.title.position deprecated. Use the `title.` options instead.
#' @param fontface,fontfamily renamed to `text.fontface` and `text.fontfamily`
#' @inheritParams tm_plot
# For bbox
#' @inheritParams tm_shape
#' @export
#' @example ./examples/tmap_options.R
#' @name tmap_options
tmap_options = function(..., crs, facet.max, facet.flip, free.scales, raster.max_cells, raster.warp, show.messages, show.warnings, output.format, output.size, output.dpi, animation.dpi, value.const, value.na, value.null, value.blank, values.var, values.range, value.neutral, values.scale, scales.var, scale.misc.args, continuous.nclass_per_legend_break, continuous.nclasses, label.format, label.na, scale, asp, bg.color, outer.bg.color, frame, frame.lwd, frame.r, frame.double_line, outer.margins, inner.margins, inner.margins.extra, meta.margins, meta.auto_margins, between_margin, panel.margin, component.offset, component.stack_margin, grid.mark.height, xylab.height, coords.height, xlab.show, xlab.text, xlab.size, xlab.color, xlab.rotation, xlab.space, xlab.fontface, xlab.fontfamily, xlab.alpha, xlab.side, ylab.show, ylab.text, ylab.size, ylab.color, ylab.rotation, ylab.space, ylab.fontface, ylab.fontfamily, ylab.alpha, ylab.side, panel.type, panel.wrap.pos, panel.xtab.pos, unit, color.sepia_intensity, color.saturation, color_vision_deficiency_sim, text.fontface, text.fontfamily, text.alpha, component.position, component.autoscale, legend.show, legend.design, legend.orientation, legend.position, legend.width, legend.height, legend.stack, legend.group.frame, legend.resize_as_group, legend.reverse, legend.na.show, legend.title.color, legend.title.size, legend.title.fontface, legend.title.fontfamily, legend.title.alpha, legend.xlab.color, legend.xlab.size, legend.xlab.fontface, legend.xlab.fontfamily, legend.xlab.alpha, legend.ylab.color, legend.ylab.size, legend.ylab.fontface, legend.ylab.fontfamily, legend.ylab.alpha, legend.text.color, legend.text.size, legend.text.fontface, legend.text.fontfamily, legend.text.alpha, legend.frame, legend.frame.lwd, legend.frame.r, legend.bg.color, legend.bg.alpha, legend.only, legend.absolute_fontsize, legend.settings.standard.portrait, legend.settings.standard.landscape, chart.show, chart.plot.axis.x, chart.plot.axis.y, chart.position, chart.width, chart.height, chart.stack, chart.group.frame, chart.resize_as_group, chart.reverse, chart.na.show, chart.title.color, chart.title.size, chart.title.fontface, chart.title.fontfamily, chart.title.alpha, chart.xlab.color, chart.xlab.size, chart.xlab.fontface, chart.xlab.fontfamily, chart.xlab.alpha, chart.ylab.color, chart.ylab.size, chart.ylab.fontface, chart.ylab.fontfamily, chart.ylab.alpha, chart.text.color, chart.text.size, chart.text.fontface, chart.text.fontfamily, chart.text.alpha, chart.frame, chart.frame.lwd, chart.frame.r, chart.bg.color, chart.bg.alpha, chart.object.color, title.size, title.color, title.fontface, title.fontfamily, title.alpha, title.bg.color, title.bg.alpha, title.padding, title.frame, title.frame.lwd, title.frame.r, title.stack, title.position, title.width, title.group.frame, title.resize_as_group, credits.size, credits.color, credits.fontface, credits.fontfamily, credits.alpha, credits.bg.color, credits.bg.alpha, credits.padding, credits.frame, credits.frame.lwd, credits.frame.r, credits.stack, credits.position, credits.width, credits.height, credits.group.frame, credits.resize_as_group, compass.north, compass.type, compass.text.size, compass.size, compass.show.labels, compass.cardinal.directions, compass.text.color, compass.color.dark, compass.color.light, compass.lwd, compass.bg.color, compass.bg.alpha, compass.margins, compass.stack, compass.position, compass.frame, compass.frame.lwd, compass.frame.r, compass.group.frame, compass.resize_as_group, logo.height, logo.margins, logo.between_margin, logo.stack, logo.position, logo.frame, logo.frame.lwd, logo.frame.r, logo.group.frame, logo.resize_as_group, scalebar.breaks, scalebar.width, scalebar.text.size, scalebar.text.color, scalebar.color.dark, scalebar.color.light, scalebar.lwd, scalebar.bg.color, scalebar.bg.alpha, scalebar.size, scalebar.margins, scalebar.stack, scalebar.position, scalebar.frame, scalebar.frame.lwd, scalebar.frame.r, scalebar.group.frame, scalebar.resize_as_group, grid.show, grid.labels.pos, grid.x, grid.y, grid.n.x, grid.n.y, grid.crs, grid.col, grid.lwd, grid.alpha, grid.labels.show, grid.labels.size, grid.labels.col, grid.labels.rot, grid.labels.format, grid.labels.cardinal, grid.labels.margin.x, grid.labels.margin.y, grid.labels.space.x, grid.labels.space.y, grid.labels.inside_frame, grid.ticks, grid.lines, grid.ndiscr, mouse_coordinates.stack, mouse_coordinates.position, mouse_coordinates.show, minimap.server, minimap.toggle, minimap.stack, minimap.position, minimap.show, panel.show, panel.labels, panel.label.size, panel.label.color, panel.label.fontface, panel.label.fontfamily, panel.label.alpha, panel.label.bg.color, panel.label.frame, panel.label.frame.lwd, panel.label.frame.r, panel.label.height, panel.label.rot, bbox, qtm.scalebar, qtm.minimap, qtm.mouse_coordinates, earth_boundary, earth_boundary.color, earth_boundary.lwd, earth_datum, space.color, check_and_fix, basemap.show, basemap.server, basemap.alpha, basemap.zoom, tiles.show, tiles.server, tiles.alpha, tiles.zoom, attr.color,
						crs_extra,
						crs_global,
						use_gradient, # plot mode
						use_browser, use_WebGL, control.position, control.bases, control.overlays, control.collapse, set_bounds, # view mode
						set_view, set_zoom_limits, use_circle_markers, leaflet.options, # view mode
						title = NULL,
						main.title = NULL,
						main.title.size = NULL,
						main.title.color = NULL,
						main.title.fontface = NULL,
						main.title.fontfamily = NULL,
						main.title.position = NULL,
						fontface = NULL,
						fontfamily = NULL
						) {
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

	fun = "tmap_options"
	if ("view.legend.position" %in% unknown_args) {
		args$legend.position = args$view.legend.position
		args$view.legend.position = NULL
		v3_opt(fun, "view.legend.position", "legend.position")
	}
	if ("set.bounds" %in% unknown_args) {
		args$set_bounds = args$set.bounds
		args$set.bounds = NULL
		v3_opt(fun, "set.bounds", "set_bounds")
	}
	if ("set.view" %in% unknown_args) {
		args$set_view = args$set.view
		args$set.view = NULL
		v3_opt(fun, "set.view", "set_view")
	}
	if ("set.zoom.limits" %in% unknown_args) {
		args$set_zoom_limits = args$set.zoom.limits
		args$set.zoom.limits = NULL
		v3_opt(fun, "set.zoom.limits", "set_zoom_limits")
	}
	if ("max.raster" %in% unknown_args) {
		args$raster.max_cells = args$max.raster
		args$max.raster = NULL
		v3_opt(fun, "max.raster", "raster.max_cells")
	}
	if ("fontfamily" %in% unknown_args) {
		args$text.fontfamily = args$fontfamily
		args$fontfamily = NULL
		v3_opt(fun, "fontfamily", "text.fontfamily")
	}
	if ("fontface" %in% unknown_args) {
		args$text.fontface = args$fontface
		args$fontface = NULL
		v3_opt(fun, "fontface", "text.fontface")
	}
	if ("overlays" %in% unknown_args) {
		args$tiles.server = args$overlays
		args$overlays = NULL
		v3_opt(fun, "overlays", "tiles.server")
	}
	if ("basemaps" %in% unknown_args) {
		args$basemap.server = args$basemaps
		args$basemaps = NULL
		v3_opt(fun, "basemaps", "basemap.server")
	}


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
		o[names(args)] = complete_options(args, backup) # needed to impute position and value(s) args


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

#' @param mode mode, e.g. `"plot"` or `"view"`
#' @param style style. If specified, the style specific options are returned
#' @param mode.specific Should only mode-specific options be returned? `TRUE` by default.
#' @param default.options return the default options or the current options?
#' @rdname tmap_options
#' @export
tmap_options_mode = function(mode = NA, style = NULL, mode.specific = TRUE, default.options = FALSE) {
	if (default.options) {
		o = .defaultTmapOptions
	} else {
		o = get("tmapOptions", envir = .TMAP)

		if (!is.null(style)) {
			check_style(style)
			s = get("tmapStyles", envir = .TMAP)[[style]]
			o = complete_options(s, o)
		}
	}

	if (is.na(mode)) mode = getOption("tmap.mode")
	opt2 = o$modes[[mode]]

	specified = attr(o, "specified")

	# all general options with mode-specific defaults (except the specified ones tmap_options(bg.color = "red")
	int_opt = setdiff(intersect(names(o), names(opt2)), specified)

	# all mode-specific options
	diff_opt = setdiff(names(opt2), names(o))

	if (length(int_opt)) o[int_opt] = opt2[int_opt]
	if (length(diff_opt)) o = c(o, opt2[diff_opt])

	o$modes = NULL

	if (mode.specific) {
		o[setdiff(names(opt2), "name")]
	} else {
		o
	}
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


