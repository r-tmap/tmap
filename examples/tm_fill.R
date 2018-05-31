data(World)

# Constant fill
tm_shape(World) + tm_fill("darkolivegreen3") + tm_format("World", title="A green World")

# Borders only
tm_shape(World) + tm_borders()

# Data variable containing colours values
World$isNLD <- ifelse(World$name=="Netherlands", "darkorange", "darkolivegreen3")
tm_shape(World) +
    tm_fill("isNLD") +
tm_layout("Find the Netherlands!")

# Categorical data variable
# if (require(RColorBrewer)) {
# pal <- brewer.pal(10, "Set3")[c(10, 8, 4, 5)]
# tm_shape(Europe) +
# 	tm_polygons("EU_Schengen", palette=pal, title = "European Countries", showNA=FALSE) +
# tm_format_Europe()
# }

tm_shape(World) +
	tm_polygons("economy", title="Economy", id="name") +
	tm_text("iso_a3", size="AREA", scale=1.5) +
	tm_format("World")

# Numeric data variable
tm_shape(World) +
	tm_polygons("HPI", palette="RdYlGn", style="cont", n=8,
		title="Happy Planet Index", id="name") +
	tm_text("iso_a3", size="AREA", scale=1.5) +
tm_format("World") + 
tm_style("grey")

\dontrun{
data(NLD_muni, NLD_prov)
tm_shape(NLD_muni) +
	tm_fill(col="population", convert2density=TRUE, 
		style="kmeans", title = expression("Population (per " * km^2 * ")"), 
		legend.hist=TRUE, id="name") +
	tm_borders("grey25", alpha=.5) + 
tm_shape(NLD_prov) +
	tm_borders("grey40", lwd=2) +
tm_format("NLD_wide", bg.color="white", frame = FALSE, legend.hist.bg.color="grey90")

# Map coloring algorithm
tm_shape(NLD_prov) +
    tm_fill("name", legend.show = FALSE) +
tm_shape(NLD_muni) +
    tm_polygons("MAP_COLORS", palette="Greys", alpha = .25) +
tm_shape(NLD_prov) +
    tm_borders(lwd=2) +
    tm_text("name", shadow=TRUE) +
tm_format("NLD", title="Dutch provinces and\nmunicipalities", bg.color="white")

# Cartogram
if (require(cartogram)) {
	NLD_prov_pop <- cartogram(NLD_prov, "population")
	tm_shape(NLD_prov_pop) +
		tm_polygons("origin_non_west", title = "Non-western origin (%)")
}
}

# TIP: check out these examples in view mode, enabled with tmap_mode("view")
