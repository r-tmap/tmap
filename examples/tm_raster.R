data(land)
data(World)

pal20 <- c("#003200", "#3C9600", "#006E00", "#556E19", "#00C800", "#8CBE8C", "#467864", "#B4E664", "#9BC832", "#EBFF64", "#F06432", "#9132E6", "#E664E6", "#9B82E6", "#B4FEF0", "#646464", "#C8C8C8", "#FF0000", "#FFFFFF", "#5ADCDC")
tm_shape(land) +
	tm_raster("cover", max.categories = 20, palette=pal20) + 
tm_layout("Global Land Cover", inner.margins=0, legend.position = c("left","bottom"))

pal8 <- c("#33A02C", "#B2DF8A", "#FDBF6F", "#1F78B4", "#999999", "#E31A1C", "#E6E6E6", "#A6CEE3")


tm_shape(land, ylim = c(-88,88), relative=FALSE) +
	tm_raster("cover_cls", palette = pal8) +
	tm_shape(World) +
	tm_borders() +
	tm_layout("Global Land Cover", inner.margins=0, title.position=c("left","bottom"), legend.position = c("left","bottom"), legend.bg.color = "#FFFFFF55")


tm_shape(land, ylim = c(-88,88), relative=FALSE) +
	tm_raster("trees", palette = "Greens") +
tm_shape(World) +
	tm_borders() +
tm_layout("Percent Tree Cover", inner.margins=0, title.position=c("left","bottom"), legend.position = c("left","bottom"), bg.color="lightblue")
	
tm_shape(land) +
	tm_raster(c("cover_cls", "trees")) +
tm_shape(World) +
	tm_borders()


tm_shape(land) +
	tm_raster("cover", max.categories = 20) +
tm_facets(by="cover") +
tm_shape(World) +
	tm_borders()

	



## test
tm_shape(land) + tm_raster("cover", max.categories = 20, palette = pal20)
tm_shape(land, ylim=c(-88,88), relative = FALSE) + tm_raster("cover_cls", palette=pal8) + tm_shape(World) + tm_borders() + tm_layout(inner.margins=0)

tm_shape(land) + tm_raster("trees") + tm_shape(world3) + tm_borders()


tm_shape(land, ylim = c(-88,88), relative=FALSE) + 
	tm_raster("cover_cls", palette = pal8) +
tm_shape(World) +
	tm_borders() +
	tm_layout("Global Land Cover", inner.margins=0, title.position=c("left","bottom"), legend.position = c("left","bottom"), legend.bg.color = "#FFFFFF55", legend.frame=TRUE, legend.titles=c(raster="test"))
