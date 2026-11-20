# Thumbnails for the tmap cheatsheet (cheatsheet/tmap_cheatsheet.html)
# Run from the package root: source("cheatsheet/thumbnails.R")
# Then build the PDF with: Rscript cheatsheet/build.R
library(tmap)

out = "cheatsheet/img"
dir.create(out, showWarnings = FALSE)

NLD = NLD_muni
Europe = World[World$continent == "Europe" & World$iso_a3 != "RUS", ]
bb_eur = tmaptools::bb(c(-12, 35, 32, 64))

thumb = tm_options(frame = FALSE, legend.show = FALSE, component.autoscale = FALSE, outer.margins = 0, scale = 0.45, inner.margins = c(0.02, 0.02, 0.02, 0.02))
small = function(tm, file, w = 1.25, h = 1.05, theme = thumb) {
	tmap_save(tm + theme, file.path(out, file), width = w, height = h, units = "in", dpi = 300)
}
legend_theme = tm_options(frame = FALSE, outer.margins = 0, scale = 0.42,
	meta.margins = c(0, 0, 0, 0.42),
	legend.position = tm_pos_out("right", "center", pos.h = "left"), legend.frame = FALSE,
	legend.just = c("left", "center"))

# basics
small(
	tm_shape(World) +
		tm_polygons(fill = "HPI") +
	tm_shape(metro) +
		tm_bubbles(size = "pop2020", fill = "gold") +
	tm_crs("auto") +
	tm_title("Happy Planet Index"),
	"basics.png", w = 3.2, h = 1.65,
	theme = tm_options(scale = 0.5, frame = FALSE, component.autoscale = FALSE, legend.position = tm_pos_out("right", "center")))

# layers
# the thumbnails are drawn with scale = 0.45; symbol sizes and line widths are enlarged to compensate
big = tm_scale_continuous(values.scale = 4)
small(tm_shape(NLD) + tm_polygons(fill = "income_high"), "l_polygons.png")
small(tm_shape(NLD) + tm_fill(fill = "urbanity") + tm_shape(NLD_prov) + tm_borders(lwd = 1.5), "l_fill_borders.png")
small(tm_shape(Europe, bbox = bb_eur) + tm_polygons() + tm_shape(metro) + tm_symbols(size = "pop2020", fill = "pop2020", shape = 24, size.scale = big), "l_symbols.png")
small(tm_shape(Europe, bbox = bb_eur) + tm_polygons() + tm_shape(metro) + tm_bubbles(size = "pop2020", size.scale = big), "l_bubbles.png")
small(tm_shape(Europe, bbox = bb_eur) + tm_polygons() + tm_shape(metro) + tm_dots(fill = "red", size = 0.5), "l_dots.png")
small(tm_shape(Europe, bbox = bb_eur) + tm_fill(fill = "gray90") + tm_shape(World_rivers) + tm_lines(col = "steelblue", lwd = "strokelwd", lwd.scale = tm_scale_continuous(values.scale = 3)), "l_lines.png")
small(tm_shape(land) + tm_raster("elevation"), "l_raster.png")
small(tm_shape(NLD_prov) + tm_polygons(fill = "gray95") + tm_text("name", size = 0.8), "l_text.png")
small(tm_shape(Europe, bbox = bb_eur) + tm_polygons(fill = "gray95") + tm_shape(metro) + tm_dots() + tm_labels("name", size = 0.7), "l_labels.png")
small(tm_shape(World, crs = "+proj=eck4") + tm_polygons(fill = "gray95") + tm_graticules(labels.show = FALSE, labels.inside_frame = TRUE), "l_graticules.png")

# scales: legends only, trimmed; shown at physical size in the html (zoom 96 / 300)
leg = function(tm, file, landscape = FALSE) {
	f = file.path(out, file)
	tmap_save(tm + tm_layout(legend.only = TRUE) +
			  	tm_options(scale = 0.7, component.autoscale = FALSE, legend.frame = FALSE, legend.position = tm_pos_in("left", "top")),
			  f, width = 3, height = 2, units = "in", dpi = 300)
	im = magick::image_trim(magick::image_read(f))
	im = magick::image_border(im, "white", "6x6")
	magick::image_write(im, f)
	cat(file, "width_mm =", round(magick::image_info(im)$width / 300 * 25.4, 1), "\n")
}
L = function(o = "portrait") tm_legend(title = "", orientation = o, width = if (o == "landscape") 11 else NA)
W = tm_shape(World)
leg(W + tm_polygons(fill = "continent", fill.scale = tm_scale_categorical(), fill.legend = L()), "s_categorical.png")
leg(W + tm_polygons(fill = "income_grp", fill.scale = tm_scale_ordinal(), fill.legend = L()), "s_ordinal.png")
leg(W + tm_polygons(fill = "HPI", fill.scale = tm_scale_intervals(style = "jenks"), fill.legend = L()), "s_intervals.png")
leg(W + tm_polygons(fill = "life_exp", fill.scale = tm_scale_intervals(style = "sd"), fill.legend = L()), "s_sd.png")
leg(W + tm_polygons(fill = "footprint", fill.scale = tm_scale_continuous(), fill.legend = L("landscape")), "s_continuous.png")
leg(W + tm_polygons(fill = "pop_est_dens", fill.scale = tm_scale_continuous_log10(n = 3), fill.legend = L("landscape")), "s_log10.png")

# facets
small(tm_shape(World, crs = "+proj=eck4") + tm_polygons(fill = c("well_being", "footprint")) + tm_facets_wrap(ncol = 2) + tm_layout(panel.label.size = 1),
	"facets.png", w = 2.6, h = 1.0, theme = tm_options(frame = FALSE, legend.show = FALSE, component.autoscale = FALSE, scale = 0.6))

# components
small(tm_shape(NLD_prov) + tm_polygons(fill = "gray95") +
	tm_title("Title") +
	tm_compass(position = c("right", "top")) +
	tm_scalebar(position = c("left", "bottom"), breaks = c(0, 50, 100)) +
	tm_credits("Credits", position = c("right", "bottom")),
	"components.png", w = 1.7, h = 1.6, theme = tm_options(frame = TRUE, component.autoscale = FALSE, scale = 0.55))

# styles
for (s in c("white", "cobalt", "classic", "natural")) {
	small(tm_shape(Europe, bbox = bb_eur) + tm_polygons(fill = "HPI") + tm_style(s), paste0("style_", s, ".png"),
		w = 0.95, h = 0.8, theme = tm_options(legend.show = FALSE, component.autoscale = FALSE, scale = 0.45))
}
