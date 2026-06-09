# Internal helper: generate a short "Unit: ..." sentence for a visual variable,
# for use as inline R code inside roxygen @param tags, e.g.:
#
#   #' @param size,size.scale,... `r .doc_vv("size")` `r .doc_unit("size")`
#
# Plain ASCII only -- no \uXXXX sequences, no Unicode dashes -- so the
# output survives the Rd processing pipeline on all platforms.
.doc_unit = function(aes) {
	lu = list(
		fill        = "Color -- a color name, hex string.",
		col         = "Color -- a color name, hex string.",
		bgcol       = "Color -- a color name, hex string.",
		fill_alpha  = "Proportion -- numeric 0-1 (0 = fully transparent, 1 = fully opaque).",
		col_alpha   = "Proportion -- numeric 0-1 (0 = fully transparent, 1 = fully opaque).",
		bgcol_alpha = "Proportion -- numeric 0-1 (0 = fully transparent, 1 = fully opaque).",

		# Symbol / bubble / square / dot size: typographic lines
		size        = paste0(
			"Typographic lines (\"lines\"); 1 line is approx. 1/6 inch.",
			" Controlled by `values.scale` and `tmap_options(values.scale = ...)`."
		),

		# Circle size: geographic meters
		size.circles = paste0(
			"Meters. Accepts a plain numeric vector (values already in meters) or a",
			" `units` object from the **units** package (any linear unit,",
			" e.g. `units::as_units(50, \"km\")`), which is converted to meters automatically."
		),

		# Text / label size: cex multiplier of the base font size.
		# In plot mode the base size is par("ps") = 12 pt (base R default).
		# In view mode: sizeChar = round(gp$cex * 12) px  (tmapLeafletDataPlot_text.R).
		# Both modes share the same base: 1 unit = 12 pt / 12 px.
		size.text   = paste0(
			"Multiplier of the base font size.",
			" `size = 1` renders at the default font size,",
			" which is 12 pt in plot mode (`par(\"ps\")`) and 12 px in",
			" view mode (consistent by design).",
			" `size = 1.5` renders at 18 pt / px, etc."
		),

		lwd         = paste0(
			"Base R line-width units; 1 lwd is approx. 0.75 pt at 96 dpi."
		),
		lty         = paste0(
			"Integer (1-6) or name: \"solid\", \"dashed\", \"dotted\",",
			" \"dotdash\", \"longdash\", \"twodash\"."
		),
		shape       = "Integer `pch` code (1-25) used as a plotting symbol. See example of `tm_symbols()`",
		angle       = "Degrees, clockwise from north (0-360).",
		fontface    = "\"plain\", \"bold\", \"italic\", or \"bold.italic\".",
		text        = "Character string.",
		xmod        = "Line heights, relative to the label anchor. Positive = right.",
		ymod        = "Line heights, relative to the label anchor. Positive = up."
	)

	info = lu[[aes]]
	if (is.null(info)) return("")
	paste0("*Unit:* ", info)
}


# -------------------------------------------------------------------------
# Details section to add to tm_polygons (inherited by all layer functions
# via  @inherit tm_polygons details).
#
# Paste the block below (without the leading '# ') into the tm_polygons
# roxygen header. All text is plain ASCII so it survives the Rd pipeline.
# -------------------------------------------------------------------------
#
# #' @details
# #' ## Visual variable units
# #'
# #' Every visual variable maps data values to a specific output unit.
# #' Knowing the unit matters when supplying constant values via [tm_const()],
# #' or output ranges via `values.range` / `values.scale` in the scale
# #' functions.
# #'
# #' | Variable | Output unit | Notes |
# #' |---|---|---|
# #' | `fill`, `col`, `bgcol` | color | name, hex, or palette string |
# #' | `fill_alpha`, `col_alpha`, `bgcol_alpha` | proportion 0-1 | 0 = transparent, 1 = opaque |
# #' | `size` (symbols, bubbles, squares, dots) | typographic lines | 1 line approx. 1/6 inch; scaled by `values.scale` |
# #' | `size` (circles) | meters | plain numeric or a `units` object |
# #' | `size` (text, labels) | multiplier | 1 = 12 pt (plot) / 12 px (view) |
# #' | `lwd` | lwd | base R units; 1 lwd approx. 0.75 pt at 96 dpi |
# #' | `lty` | -- | integer 1-6 or name ("solid", "dashed", ...) |
# #' | `shape` | -- | integer `pch` 1-25 or single character |
# #' | `angle` | degrees | 0-360, clockwise from north |
# #' | `fontface` | -- | "plain", "bold", "italic", "bold.italic" |
# #'
# #' ### Symbol size (`size` in `tm_symbols`, `tm_bubbles`, `tm_squares`, `tm_dots`)
# #' "Lines" is a typographic unit: one line is approximately 1/6 inch (the
# #' default base line-height in R graphics). The global multiplier
# #' `tmap_options(values.scale = list(size.bubbles = 1.5))` scales all symbol
# #' sizes without changing the data mapping.
# #'
# #' ### Circle size (`size` in `tm_circles`)
# #' The value is a geographic radius in meters. A plain numeric vector is
# #' interpreted as meters; a `units` object (from the **units** package) is
# #' automatically converted, so `units::as_units(1, "mi")` gives a 1-mile
# #' radius. Because the radius is geographic, circles scale with zoom in
# #' interactive (view) mode -- unlike bubble symbols which keep a fixed screen
# #' size.
# #'
# #' ### Text size (`size` in `tm_text`, `tm_labels`)
# #' The value is a multiplier of the base font size. `size = 1` renders at
# #' 12 pt in plot mode (R's default `par("ps")`) and at 12 px in view
# #' mode (`gp$cex * 12` px, see `tmapLeafletDataPlot.tm_data_text`); the two
# #' modes are consistent by design.
