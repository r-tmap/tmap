data(World)

tm_shape(World) +
	tm_polygons("HPI", fill.scale = tm_scale_intervals(values = "RdYlGn")) +
	tm_logo(c("https://www.r-project.org/logo/Rlogo.png", 
			  system.file("img/tmap.png", package="tmap"))) +
	tm_logo("http://blog.kulikulifoods.com/wp-content/uploads/2014/10/logo.png", 
			height=5, position = c("left", "top")) +
	tm_format("World")
