data(World)
data(Europe)
data(NLD_muni)
data(NLD_prov)
data(metro)
data(land)

# Map of Europe
qtm(Europe)

# Choropleth
qtm(World, fill = "economy", text="iso_a3", text.size = "AREA", fill.palette="-Blues", 
    theme = "World", fill.title="Economy")

# Choropleth
tm_shape(World) +
    tm_polygons("pop_est_dens", style="kmeans", palette="YlOrRd", title="Population per km2") +
    tm_text("iso_a3", size="AREA", scale=1.5) +
tm_layout_World()

# Choropleth and bubblemap
metro$growth <- (metro$pop2020 - metro$pop2010) / (metro$pop2010 * 10) * 100
tm_shape(World) +
    tm_polygons("income_grp", style="kmeans", palette="-Blues", title="Income class") +
    tm_text("iso_a3", size="AREA", scale=1.5) +
tm_shape(metro) +
    tm_bubbles("pop2010", col = "growth", border.col = "black", 
        border.alpha = .5, style="fixed", 
        breaks=c(-Inf, seq(0, 6, by=2), Inf),
        palette="-RdYlGn", contrast=1, 
        title.size="Metro population (2010)", 
        title.col="Annual growth rate (%)") + 
tm_layout_World(title="Income and\nurbanization")

# Bubblemap with text labels
tm_shape(Europe) +
    tm_borders() +
    tm_fill() +
tm_shape(metro) +
    tm_bubbles(size="pop2010", col="purple", size.lim=c(0, 1.2e7), 
        title.size="Metro population") +
    tm_text("name", size="pop2010", scale=2, root=3, ymod=-.015 , bg.alpha=0) +
tm_layout_Europe()

# Small multiples choropleth
tm_shape(NLD_muni) +
    tm_borders() +
    tm_facets(by="province") +
    tm_fill("population", style="kmeans", convert2density = TRUE) +
tm_shape(NLD_prov) +
    tm_borders(lwd=4) +
    tm_facets(by="name", free.coords=TRUE, drop.shapes=TRUE) +
tm_layout(legend.show = FALSE)

# Dasymetric map (using a raster)
tm_shape(land, ylim = c(-88,88)) +
    tm_raster("elevation", n=10, style="kmeans", palette = terrain.colors(10), 
        title="Elevation") +
    tm_shape(World) +
        tm_borders() +
tm_layout(inner.margins=0, legend.position = c(.02, .1), 
    bg.color="lightblue")

# Raster map with a dot map
pal8 <- c("#33A02C", "#B2DF8A", "#FDBF6F", "#1F78B4", "#999999", "#E31A1C", "#E6E6E6", "#A6CEE3")
tm_shape(land, ylim = c(-88,88)) +
    tm_raster("cover_cls", palette = pal8, title="Global Land Cover") +
tm_shape(metro) +
    tm_bubbles(size=.02, col="#E31A1C") +
tm_shape(World) +
    tm_borders() +
tm_layout_World(inner.margins=0, 
    legend.position = c("left","bottom"), 
    legend.bg.color = "white", legend.bg.alpha=.2, 
    legend.frame="gray50")
