data(land)
data(World)

pal20 <- c("#003200", "#3C9600", "#006E00", "#556E19", "#00C800", "#8CBE8C",
		     "#467864", "#B4E664", "#9BC832", "#EBFF64", "#F06432", "#9132E6",
		     "#E664E6", "#9B82E6", "#B4FEF0", "#646464", "#C8C8C8", "#FF0000",
		     "#FFFFFF", "#5ADCDC")
tm_shape(land) +
    tm_raster("cover", max.categories = 20, palette=pal20, title="Global Land Cover") + 
tm_layout_World(inner.margins=0, legend.position = c("left","bottom"))

pal8 <- c("#33A02C", "#B2DF8A", "#FDBF6F", "#1F78B4", "#999999", "#E31A1C", "#E6E6E6", "#A6CEE3")
tm_shape(land, ylim = c(-88,88), relative=FALSE) +
    tm_raster("cover_cls", palette = pal8, title="Global Land Cover") +
tm_shape(World) +
    tm_borders() +
tm_layout_World(inner.margins=0,
    legend.position = c("left","bottom"),
    legend.bg.color = "white", legend.bg.alpha=.2, 
    legend.frame="gray50", legend.width=.2)

tm_shape(land, ylim = c(-88,88), relative=FALSE) +
    tm_raster("trees", palette = "Greens", title="Percent Tree Cover") +
tm_shape(World) +
    tm_borders() +
tm_layout(inner.margins=0, 
    legend.position = c("left","bottom"), bg.color="lightblue")

tm_shape(land) +
    tm_raster("black") +
    tm_facets(by="cover_cls") +
tm_layout(inner.margins=0, 
    title.position = c("left", "bottom"), title.bg.color="gray80")
