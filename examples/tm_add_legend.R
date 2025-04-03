\dontrun{
tm_shape(NLD_muni) +
	tm_borders() +
	tm_basemap("OpenStreetMap") +
	tm_add_legend(labels = c("Motorway", "Primary road", "Secondary road", "Railway"),
				  col = c("#E892A1", "#FCD6A4", "#F8FABF", "#707070"),
				  lty = c("solid", "solid", "solid", "dotted"),
				  lwd = 3,
				  type = "lines",
				  bg.color = "grey92",
				  bg.alpha = 1)
}
