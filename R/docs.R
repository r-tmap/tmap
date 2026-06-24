.doc_opt = function(a) {
	x = strsplit(a, split = ".", fixed = TRUE)[[1]]
	cls = head(x, -1)
	what = tail(x, 1)

	cls_lookup = c(inner = "inner space (inside the frame)",
				   outer = "outer space (outside the frame",
				   item_text = "space between item and text",
				   meta = "'meta' space (reserved for outside map components)")

	what_lookup = c(alpha = "alpha transparency",
					asp = "asp ration",
					bbox = "bounding box",
					breaks = "break values",
					col = "color",
					color = "color",
					crs = "coordinate reference system (CRS)",
					double_line = "double line",
					dpi = "dots per inch",
					earth_boundary = "earth_boundary",
					earth_datum = "earth_datum",
					extra = "extra arguments",
					flip = "flip",
					fontface = "font face",
					fontfamily = "font family",
					format = "format",
					frame = "frame",
					height = "height",
					inside_frame = "inside_frame",
					labels = "labels",
					landscape = "landscape",
					light = "light",
					limits = "limits",
					lines = "lines",
					lwd = "line width",
					margin = "margin",
					margins = "margins",
					max = "max",
					max_cells = "max_cells",
					messages = "messages",
					minimap = "minimap",
					modes = "modes",
					mouse_coordinates = "mouse_coordinates",
					na = "na",
					nclass_per_legend_break = "nclass_per_legend_break",
					nclasses = "nclasses",
					ndiscr = "ndiscr",
					neutral = "neutral",
					north = "north",
					null = "null",
					offset = "offset",
					only = "only",
					orientation = "orientation",
					padding = "padding",
					portrait = "portrait",
					pos = "pos",
					position = "position",
					r = "r (radius)",
					range = "range",
					resize_as_group = "resize_as_group",
					reverse = "reverse",
					rot = "rot",
					rotation = "rotation",
					saturation = "saturation",
					scale = "scale",
					scalebar = "scalebar",
					scales = "scales",
					sepia_intensity = "sepia_intensity",
					server = "server",
					show = "visibility",
					side = "side",
					size = "size",
					space = "space",
					stack = "stack",
					stack_margin = "stack_margin",
					text = "text",
					ticks = "ticks",
					toggle = "toggle",
					type = "type",
					unit = "unit",
					var = "var",
					view = "view",
					warnings = "warnings",
					width = "width",
					absolute_fontsize = "absolute fontsize",
					x = "x",
					y = "y",
					xlab = "`x` label",
					ylab = "`y` label",
					zoom = "zoom")

	extra_lookup = c(margins = " A vector of 4 values: bottom, left, top, right. The unit is the device height (for bottom and top) or width (for left and right).",
					 double_line = " `TRUE` or `FALSE`.",
					 extra = " A list of arguments.",
					 padding = " A vector of 4 values: bottom, left, top, right. The unit is the device height (for bottom and top) or width (for left and right).",
					 position = " A tm_pos object, or a shortcut of two values: horizontal (left, center, right) and vertical (top, center, bottom). See tm_pos for details",
					 space = " In terms of number of text line heights.",
					 fontfamily = " See `graphics::par`, option 'family'.",
					 fontface = " See `graphics::par`, option 'font'.",
					 absolute_fontsize = " So far, only used to calculate legend dimensions",
					 lwd = " See `graphics::par`, option 'lwd'.",
					 show = " `TRUE` or `FALSE`.")

	what_text = unname(what_lookup[what])
	if (is.na(what_text)) what_text = what

	cls_text = sapply(cls, function(x) {
		res = unname(cls_lookup[x])
		if (is.na(res)) x else res
	}, USE.NAMES = FALSE)

	extra_text = unname(extra_lookup[what])
	if (is.na(extra_text)) extra_text = ""


	if (length(cls) == 0L) {
		cls_texts = " (overall)"
	} else {
		cls_texts = paste(" of the", paste(rev(cls_text), collapse = " of the "))
	}
	paste0("The ", what_text, cls_texts, ".", extra_text)
}

.doc_vv = function(x) {
	lookup = c(fill = "fill color",
			   col = "color",
			   fill_alpha = "fill color transparency",
			   col_alpha = "color transparency",
			   lwd = "line width",
			   lty = "line type",
			   size = "size",
			   text = "text",
			   fontface = "font face")
	txt = unname(lookup[x])
	if (is.na(txt)) txt = x
	paste0("Map variable that determines the ", txt, ". See details.")
}

.tmap_article_url = "https://r-tmap.github.io/tmap/articles/"

# Single source of truth: vignette slug -> link label
.tmap_vignettes = c(
	basics_vv            = "Map variables",
	basics_scales        = "Scales",
	basics_modes         = "Modes",
	basics_facets        = "Facets",
	basics_legends       = "Legends",
	basics_basemaps      = "Basemaps",
	basics_components    = "Components",
	basics_charts        = "Charts",
	basics_layout        = "Layout",
	basics_exporting     = "Exporting",
	foundations_gg       = "Grammar of Graphics",
	foundations_crs      = "Map projections (CRS)",
	foundations_units    = "Units",
	examples_choro_World = "Choropleth (World)",
	examples_choro_NLD   = "Choropleth (Netherlands)",
	examples_bubble      = "Bubble map",
	examples_raster      = "Raster map",
	examples_topo_Africa = "Topographic map (Africa)",
	examples_terrain     = "Terrain map",
	examples_gridmaps    = "Gridmap",
	examples_biv_choro   = "Bivariate choropleth",
	versus_ggplot2       = "tmap vs ggplot2",
	versus_mapview       = "tmap vs mapview",
	versus_mapsf         = "tmap vs mapsf",
	ext_mapgl            = "tmap.mapgl",
	ext_cartogram        = "tmap.cartogram",
	ext_glyphs           = "tmap.glyphs",
	ext_networks         = "tmap.networks",
	adv_legends          = "Legends (advanced)",
	adv_inset_maps       = "Inset maps",
	adv_animations       = "Animations",
	adv_shiny            = "Shiny integration",
	adv_ggplot2          = "ggplot2 integration",
	adv_multivariate     = "Multivariate map variables",
	adv_positions        = "Positioning of components",
	adv_comp_group       = "Grouping of components",
	adv_options          = "Options and styles",
	adv_margins          = "Margins and aspect ratio",
	adv_groups           = "Layer groups (view mode)",
	adv_extensions       = "Extending tmap",
	adv_blend            = "Layer blending"
)

# Build a comma-separated list of markdown links from vignette slugs
.doc_links = function(slugs) {
	labels = .tmap_vignettes[slugs]
	if (anyNA(labels)) {
		stop("Unknown tmap vignette slug(s): ",
			 paste(slugs[is.na(labels)], collapse = ", "), call. = FALSE)
	}
	paste0("[", labels, "](", .tmap_article_url, slugs, ")", collapse = ", ")
}

.doc_see_also_layers = function(layer) {
	common = c("basics_vv", "basics_scales", "basics_legends", "basics_facets", "foundations_units", "adv_blend")
	# layer-specific examples — review/adjust these mappings to taste
	examples = switch(layer,
					  polygons = c("examples_choro_World", "examples_choro_NLD", "examples_biv_choro", "examples_gridmaps"),
					  symbols  = c("examples_bubble", "examples_terrain"),
					  lines = "examples_terrain",
					  text = c("examples_topo_Africa", "examples_terrain"),
					  raster   = c("examples_raster", "examples_terrain"),
					  character(0)  # default: common links only (lines, text, ...)
	)
	.doc_links(c(common, examples))
}

.doc_see_vig = function(x, cap = FALSE) {
	init = ifelse(cap, "V", "v")
	paste0(init, "ignette: ", .doc_links(x))
}

.doc_see_also_comp = function() {
	.doc_links(c("basics_components", "adv_positions", "adv_comp_group", "examples_choro_NLD"))
}

.doc_see_also_scales = function() {
	.doc_links(c("basics_scales", "basics_vv", "adv_multivariate"))
}

.doc_see_also_legend = function(advanced = FALSE, biv = FALSE) {
	biv_txt = if (biv) "examples_biv_choro" else NULL
	if (advanced) {
		.doc_links(c("adv_legends", "adv_positions", biv_txt))
	} else {
		.doc_links(c("basics_legends", "adv_legends", "basics_components", "adv_positions", biv_txt))
	}
}

.doc_see_also_chart = function() {
	.doc_links(c("basics_charts", "basics_components", "adv_positions"))
}

.doc_see_also_layout = function() {
	.doc_links(c("basics_layout", "adv_margins", "adv_options", "examples_choro_World", "examples_terrain"))
}

.doc_see_also_facets = function() {
	.doc_links(c("basics_facets", "adv_animations", "examples_gridmaps"))
}

.doc_see_also_animations = function() {
	.doc_links(c("adv_animations",  "ext_cartogram"))
}

.doc_see_also_modes = function() {
	.doc_links(c("basics_modes"))
}

.doc_see_also_export = function() {
	.doc_links(c("basics_exporting"))
}

.doc_see_also_shiny = function() {
	.doc_links(c("adv_shiny"))
}

.doc_see_also_basemaps = function() {
	.doc_links(c("basics_basemaps", "basics_modes"))
}

.doc_see_also_groups = function() {
	.doc_links(c("adv_groups"))
}

.doc_see_also_shape = function() {
	.doc_links(c("foundations_crs", "examples_choro_NLD", "examples_choro_World", "examples_terrain"))
}

.doc_see_also_insets = function() {
	.doc_links(c("adv_inset_maps", "adv_ggplot2"))
}
