data(World)

tm_shape(World) + 
    tm_fill("pop_est_dens", style="kmeans", title="Population density") +
	tm_compass(position = c("left", "center")) +
tm_format_World(title="The World", compass.type="radar") + tm_colors_albatross()

# Classic styled world map
data(land)
land_eck4 <- set_projection(land, "eck4") # convert to Eckert IV projection

tm_shape(land_eck4) +
	tm_raster("elevation", breaks=c(-Inf, 250, 500, 1000, 1500, 2000, 2500, 3000, 4000, Inf),  
		palette = terrain.colors(9), title="Elevation", auto.palette.mapping = FALSE) +
tm_shape(World) +
	tm_borders("grey20") +
	tm_grid(projection="longlat", labels.size = .5) +
	tm_text("name", size="AREA") +
tm_compass(position = c(.65, .15), color.light = "grey90") +
tm_credits("Eckert IV projection", position = c(.85, 0)) +
tm_style_classic(inner.margins=c(.04,.03, .02, .01), 
		  bg.color="lightblue", 
		  legend.position = c("left", "bottom"), 
		  legend.frame = TRUE,
		  legend.bg.color="lightblue",
		  earth.boundary = TRUE, space.color="grey90")

# classic choropleth
data(NLD_muni, NLD_prov)
tm_shape(NLD_muni) +
	tm_fill(col="population", convert2density=TRUE, 
			style="kmeans", title="Population (per km2)", legend.hist=FALSE) +
	tm_borders("black", alpha=.5) + 
tm_shape(NLD_prov) +
	tm_borders("grey25", lwd=2) +
tm_format_NLD(inner.margins = c(.02, .15, .06, .15)) + 
tm_scale_bar(position = c("left", "bottom")) +
tm_compass(position=c("right", "bottom")) + 
tm_style_classic()


# Color theme comparison
two_by_two_plot <- function(tm) {
	require(grid)
	grid.newpage()
	pushViewport(viewport(layout = grid.layout(2,2)))
	print(tm + tm_layout("Default theme"), 
		  vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
	print(tm + tm_colors_cobalt(title="Cobalt theme"), 
		  vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
	print(tm + tm_colors_albatross(title="Albatross theme"), 
		  vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
	print(tm + tm_colors_beaver(title="Beaver theme"), 
		  vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
	upViewport()
}

# fixed aesthetics
data(Europe, rivers, metro)
two_by_two_plot(tm_shape(Europe) +
	tm_fill() +
tm_shape(rivers) +
	tm_lines() +
tm_shape(metro) +
	tm_bubbles(size="pop2010") + 
	tm_text("name", size="pop2010", auto.placement = TRUE))

# for choropleth aesthetics
data(NLD_muni, NLD_prov)
two_by_two_plot(tm_shape(NLD_muni) +
	tm_fill("population", convert2density = TRUE, style="kmeans") +
tm_shape(NLD_prov) +
	tm_borders(lwd=2) +
	tm_text("name") +
tm_layout(scale=.7))

