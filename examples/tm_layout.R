data(World, land)

tm_shape(World) + 
    tm_fill("pop_est_dens", style="kmeans", title="Population density") +
tm_format_World(title="The World") + tm_style_albatross(frame.lwd=10)

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
tm_layout(inner.margins=c(.04,.03, .02, .01), 
	earth.boundary = TRUE, 
	space.color="grey90") +
tm_style_classic(bg.color="lightblue") +
tm_legend(position = c("left", "bottom"), 
	frame = TRUE,
	bg.color="lightblue")

\dontrun{
# the last three elements (tm_layout, tm_style_classic, and tm_legend)
# can also be merged into one element function:
tm_style_classic(inner.margins=c(.04,.03, .02, .01), 
	earth.boundary = TRUE, 
	space.color="grey90",
	bg.color="lightblue", 
	legend.position = c("left", "bottom"), 
	legend.frame = TRUE,
	legend.bg.color="lightblue")
}

\dontrun{
# global option tmap.style:
qtm(World, fill="economy", format="World")
options(tmap.style = "col_blind")
qtm(World, fill="economy", format="World")
options(tmap.style = "cobalt")
qtm(World, fill="economy", format="World")
options(tmap.style = "white") # the default
}
