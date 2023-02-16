#################################################################################################
#################################################################################################
############################### tm_polygons #####################################################
#################################################################################################
#################################################################################################


data(World)

# Constant fill
tm_shape(World) + tm_fill("darkolivegreen3") + tm_format("World", title="A green World")

tm_shape(World) + tm_polygons(fill = "darkolivegreen3", col = NA) + tm_format("World", title = "A green World")



# Borders only
tm_shape(World) + tm_borders()

# Data variable containing colours values
World$isNLD <- ifelse(World$name=="Netherlands", "darkorange", "darkolivegreen3")
tm_shape(World) +
	tm_fill(I("isNLD")) +
	tm_layout("Find the Netherlands!")

tm_shape(World, projection = "+proj=eck4") +
	tm_polygons("economy", title="Economy", id="name") +
	tm_text("iso_a3", size="AREA", scale=1.5) +
	tm_format("World")

# Numeric data variable
tm_shape(World, projection = "+proj=eck4") +
	tm_polygons("HPI", palette="RdYlGn", style="cont", n=8,
				title="Happy Planet Index", id="name") +
	tm_text("iso_a3", size="AREA", scale=1.5) +
	tm_style("grey") +
	tm_format("World")

\dontrun{
	data(NLD_prov, NLD_muni)	
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

tm_shape(World)
World$HPI[1:10] = NA

tmap_style("v3")
tm_shape(World) + tm_polygons("economy", style = "cat")
tm_shape(World) + tm_polygons("HPI", style = "fixed", breaks = c(0, 20, 35, 42, 50))
tm_shape(World) + tm_polygons("HPI", style = "sd")
tm_shape(World) + tm_polygons("HPI", style = "equal")
tm_shape(World) + tm_polygons("HPI", style = "pretty")
tm_shape(World) + tm_polygons("HPI", style = "quantile")
tm_shape(World) + tm_polygons("HPI", style = "kmeans")
tm_shape(World) + tm_polygons("HPI", style = "hclust")
tm_shape(World) + tm_polygons("HPI", style = "bclust")
tm_shape(World) + tm_polygons("HPI", style = "fisher")
tm_shape(World) + tm_polygons("HPI", style = "jenks")
tm_shape(World) + tm_polygons("HPI", style = "dpih")
tm_shape(World) + tm_polygons("HPI", style = "headtails")
tm_shape(World) + tm_polygons("HPI", style = "log10_pretty")

tm_shape(World) + tm_polygons("HPI", style = "cont")
tm_shape(World) + tm_polygons("HPI", style = "order")
tm_shape(World) + tm_polygons("HPI", style = "log10")


tm_shape(World) + tm_polygons("HPI", style = "cont")

