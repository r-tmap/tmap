data(World)

tm_shape(World) +
	tm_polygons("HPI", fill.scale = tm_scale_intervals(values = "brewer.rd_yl_gn")) +
	tm_logo(c("https://www.r-project.org/logo/Rlogo.png",
			  system.file("help", "figures", "logo.png", package = "tmap"))) +
	tm_logo("https://happyplanetindex.org/wp-content/themes/hpi/public/images/hpi-logo.svg",
			height=5, position = c("left", "center"))
