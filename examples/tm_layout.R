data(World)

tm_shape(World) + 
    tm_fill("pop_est_dens", style="kmeans", title="Population density") + 
tm_layout_World("World Population", bg.color="lightblue")

# A custom layout wrapper for Africa
tm_layout_Africa <- function(title=NA,
							 inner.margins = c(.02, .25, .02, .02),
							 draw.frame = FALSE,
							 title.position=c("left", "bottom"), 
							 legend.position = c("left", "bottom"),
							 legend.width = 0.5,
							 bg.color = "lightskyblue2", ...) {
	args <- c(as.list(environment()), list(...))
	do.call("tm_layout", args)
}

Africa <- World[World$continent=="Africa", ]

qtm(Africa, fill="pop_est_dens", fill.style="kmeans", fill.title="Population density") + 
    tm_layout_Africa("Africa")


\dontrun{
# classic styled world map
data(land)
	
# convert to Eckert IV projection
land_eck4 <- set_projection(land, "eck4")

tm_shape(land_eck4) +
	tm_raster("elevation", breaks=c(-Inf, 250, 500, 1000, 1500, 2000, 2500, 3000, 4000, Inf),  palette = terrain.colors(9), title="Elevation") +
tm_shape(World) +
	tm_borders("grey20") +
	tm_grid(projection="longlat", labels.size = .5) +
	tm_text("name", size="AREA") +
	tm_compass(position = c(.67, .1), color.light = "grey90") +
tm_layout(inner.margins=.02, legend.position = c("left", "bottom"), legend.frame = TRUE, 
		  bg.color="lightblue", legend.bg.color="lightblue", earth.boundary = TRUE, space.color="grey90") + 
tm_style_classic()
}