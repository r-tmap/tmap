data(World, Europe, NLD_muni, NLD_prov)

# Constant fill
tm_shape(World) + tm_fill("darkolivegreen3") + tm_format_World(title="A green World")

# Borders only
tm_shape(Europe) + tm_borders()

# Data variable containing colours values
Europe$isNLD <- ifelse(Europe$name=="Netherlands", "darkorange", "darkolivegreen3")
tm_shape(Europe) +
    tm_fill("isNLD") +
tm_layout("Find the Netherlands!")

# Numeric data variable
tm_shape(NLD_muni) +
    tm_fill(col="population", convert2density=TRUE, 
        style="kmeans", title="Population (per km2)", legend.hist=TRUE, id="name") +
    tm_borders("grey25", alpha=.5) + 
tm_shape(NLD_prov) +
    tm_borders("grey40", lwd=2) +
tm_format_NLD_wide(bg.color="white", frame = FALSE, legend.hist.bg.color="grey90")

tm_shape(World) +
    tm_polygons("HPI", palette="RdYlGn", style="cont", n=8, auto.palette.mapping=FALSE, 
    			title="Happy Planet Index", id="name") +
    tm_text("iso_a3", size="AREA", scale=1.5) +
tm_format_World() + 
tm_style_grey()

# Categorical data variable
pal <- RColorBrewer::brewer.pal(10, "Set3")[c(10, 8, 4, 5)]
tm_shape(Europe) +
	tm_polygons("EU_Schengen", palette=pal, title = "European Countries", showNA=FALSE) +
tm_format_Europe()

tm_shape(World) +
    tm_polygons("economy", title="Economy", id="name") +
    tm_text("iso_a3", size="AREA", scale=1.5) +
tm_format_World()

# Map coloring algorithm
tm_shape(NLD_prov) + 
    tm_fill("name", legend.show = FALSE) + 
tm_shape(NLD_muni) + 
    tm_polygons("MAP_COLORS", palette="Greys", alpha = .25) + 
tm_shape(NLD_prov) + 
    tm_borders(lwd=2) +
    tm_text("name", shadow=TRUE) +
tm_format_NLD(title="Dutch provinces and\nmunicipalities", bg.color="white")

# TIP: check out these examples in view mode, enabled with tmap_mode("view")
