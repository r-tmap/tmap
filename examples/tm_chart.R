tm_shape(World) + 
  tm_polygons ("HPI", 
    fill.scale = tm_scale_intervals(), 
    fill.chart = tm_chart_histogram())

tm_shape(World) + 
	tm_polygons ("economy", 
				 fill.scale = tm_scale_categorical(), 
				 fill.chart = tm_chart_histogram())



tm_shape(World) + 
	tm_polygons ("HPI", 
	fill.scale = tm_scale_intervals(), 
	fill.chart = tm_chart_histogram(position = tm_pos_out("center", "bottom", align.v = "bottom")))

tm_shape(land) +
	tm_raster("trees",
			  col.chart = tm_chart_histogram())


tm_shape(land) +
	tm_raster("trees",
			  col.scale = tm_scale_continuous(),
			  col.chart = tm_chart_histogram())



tm_shape(World) + 
  tm_polygons ("HPI", 
    fill.scale = tm_scale_intervals(), 
    fill.chart = tm_chart_donut())

tm_shape(World) +
	tm_polygons("HPI") +
	tm_compass(size = 6,position = tm_pos_out("right", "center"))
