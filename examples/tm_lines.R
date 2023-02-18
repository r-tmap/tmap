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
			 lty.legend = tm_legend_combine("lwd"))
