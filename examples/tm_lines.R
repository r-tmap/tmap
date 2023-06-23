data(rivers)

tm_shape(rivers) +
	tm_lines()

tm_shape(rivers) +
	tm_lines(lwd = "scalerank")

tm_shape(rivers) +
	tm_lines(lwd = "scalerank", 
			 lwd.scale = tm_scale_continuous(values.scale = 2, n = 20), 
			 lwd.legend = tm_legend(orientation = "landscape", item.width = 2),
			 col = "type", 
			 col.scale = tm_scale(values = c("darkblue", "darkred")))

tm_shape(rivers) +
	tm_lines(lwd = "scalerank",
			 lty = "scalerank",
			 col = "scalerank",
			 col.scale = tm_scale_categorical(),
			 lty.legend = tm_legend_combine("lwd"),
			 col.legend = tm_legend_combine("lwd"))


### tmap3

tm_shape(World) +
	tm_fill() +
	tm_shape(rivers) +
	tm_lines(col="black", lwd="scalerank", scale=2, legend.lwd.show = FALSE) +
	tm_style("cobalt", title = "Rivers of the World") +
	tm_format("World")

