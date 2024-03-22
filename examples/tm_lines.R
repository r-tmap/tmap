tm_shape(rivers) +
	tm_lines(lwd = "strokelwd", lwd.scale = tm_scale_asis(values.scale = 0.2, value.neutral = 2),
			 col = "scalerank", col.scale = tm_scale_categorical(values = "seaborn.dark"))

tm_shape(World) +
	tm_lines(col = "continent", col.scale = tm_scale_categorical(values = "seaborn.dark"),
			 lty = "continent",
			 lwd = 1.5,
			 lty.legend = tm_legend_combine("col"))
