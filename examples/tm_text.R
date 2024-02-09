# Need to be updated. The following is just to illustrate bgcol(_alpha)
tm_shape(World) + tm_polygons() +
	tm_text("name", bgcol = "economy", bgcol_alpha = .5, shadow = TRUE)

tm_shape(World) +
  tm_text(text = "name", 
    size = .4, 
    bgcol = "economy")

tm_shape(World) +
  tm_text(text = "name", 
    size = .4, 
    bgcol = "economy", 
    bgcol.scale = tm_scale_categorical(values = cols4all::.P$hcl$cat$set2),
    bgcol_alpha = "pop_est",
    bgcol_alpha.scale = tm_scale_intervals(style = "kmeans"))
