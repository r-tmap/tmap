data(land, World)
# Error unable to warp stars (argument not yet added to tm_shape)
\dontrun{
tm_shape(land, raster.wrap = FALSE) +
  tm_raster(
    "elevation",
    col.scale = tm_scale_intervals(
      breaks = c(-Inf, 250, 500, 1000, 1500, 2000, 2500, 3000, 4000, Inf),
      values = terrain.colors(9), midpoint = NA
    ),
    col.legend = tm_legend(
      title = "Elevation", position = tm_pos_in("left", "bottom"), 
      frame = TRUE, bg.color = "lightblue"
    )
  ) +
  tm_shape(World, is.main = TRUE, crs = "+proj=eck4") +
  tm_borders("grey20") +
  tm_graticules(labels.size = .5) +
  tm_text("name", size = "AREA") +
  # tm_compass(position = c(.65, .15), color.light = "grey90") +
  # tm_credits("Eckert IV projection", position = c("right", "BOTTOM")) +
  tm_style("classic_v3") +
  tm_layout(bg.color = "lightblue", inner.margins = c(0, 0, .02, 0))
}
data(land, World)

tm_shape(World) + 
	tm_fill("pop_est_dens", fill.scale = tm_scale_intervals(style = "kmeans"),
			fill.legend = tm_legend(title = "Population density")) +
	tm_style("albatross_v3", frame.lwd = 10) +
	tm_format("World") +
	tm_title("The World", position = tm_pos_in("left", "top"))


################################
# not working yet:
################################

\dontrun{
	tm_shape(land) +
		tm_raster("elevation",
				  breaks=c(-Inf, 250, 500, 1000, 1500, 2000, 2500, 3000, 4000, Inf),  
				  palette = terrain.colors(9), title="Elevation", midpoint = NA) +
		tm_shape(World, is.master=TRUE, projection = "+proj=eck4") +
		tm_borders("grey20") +
		tm_graticules(labels.size = .5) +
		tm_text("name", size="AREA") +
		tm_compass(position = c(.65, .15), color.light = "grey90") +
		tm_credits("Eckert IV projection", position = c("right", "BOTTOM")) +
		tm_style("classic") +
		tm_layout(bg.color="lightblue",
				  inner.margins=c(.04,.03, .02, .01), 
				  earth.boundary = TRUE, 
				  space.color="grey90") +
		tm_legend(position = c("left", "bottom"), 
				  frame = TRUE,
				  bg.color="lightblue")
}

tm_shape(World, projection="+proj=robin") +
	tm_polygons("HPI", palette="div", n=7, 
				title = "Happy Planet Index") +
	tm_credits("Robinson projection", position = c("right", "BOTTOM")) +
	tm_style("natural", earth.boundary = c(-180, -87, 180, 87), inner.margins = .05) +
	tm_legend(position=c("left", "bottom"), bg.color="grey95", frame=TRUE)

# Example to illustrate the type of titles
tm_shape(World) +
	tm_polygons(c("income_grp", "economy"), title = c("Legend Title 1", "Legend Title 2")) +
	tm_layout(main.title = "Main Title",
			  main.title.position = "center",
			  main.title.color = "blue",
			  title = c("Title 1", "Title 2"),
			  title.color = "red",
			  panel.labels = c("Panel Label 1", "Panel Label 2"),
			  panel.label.color = "purple",
			  legend.text.color = "brown")

\dontrun{
	# global option tmap.style demo
	
	# get current style
	current.style <- tmap_style() 
	
	qtm(World, fill = "economy", format = "World")
	
	tmap_style("col_blind")
	qtm(World, fill = "economy", format = "World")
	
	tmap_style("cobalt")
	qtm(World, fill = "economy", format = "World")
	
	# set to current style
	tmap_style(current.style)
}

# TIP: check out these examples in view mode, enabled with tmap_mode("view")
