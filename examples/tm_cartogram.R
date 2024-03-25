Africa = World[World$continent == "Africa", ]

#step3 L50: to do: trans.isglobal needs another variant trans.apply_from_here
tm_shape(Africa, crs = "+proj=robin") + 
	tm_cartogram(size = "pop_est", options = opt_tm_cartogram(itermax = 15)) +
	tm_text("name")


tm_shape(Africa, crs = "+proj=robin") + 
	tm_cartogram_ncont(size = "pop_est", options = opt_tm_cartogram_ncont()) +
	tm_text("name")

tm_shape(World, crs = "+proj=robin") +
	tm_polygons() +
	tm_cartogram_ncont(size = "pop_est", fill = "yellow")


# to do: make output like this:
W = cartogram_ncont(World |> st_transform("+proj=robin"), weight = "pop_est")
tm_shape(World, crs = "+proj=robin") +
	tm_polygons() +
tm_shape(W) +
	tm_polygons(fill = "yellow")
