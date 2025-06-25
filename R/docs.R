.doc_opt = function(a) {
	x = strsplit(a, split = ".", fixed = TRUE)[[1]]
	cls = head(x, -1)
	what = tail(x, 1)

	cls_lookup = c(inner = "inner space (inside the frame)",
				   outer = "outer space (outside the frame",
				   item_text = "space between item and text")

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
	paste0("Visual variable that determines the ", txt, ". See details.")
}
