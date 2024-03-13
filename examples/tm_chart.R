## numerical variable

tm_shape(World) + 
  tm_polygons ("HPI", 
    fill.scale = tm_scale_intervals(), 
    fill.chart = tm_chart_histogram())

tm_shape(World) + 
	tm_polygons ("HPI", 
				 fill.scale = tm_scale_continuous(), 
				 fill.chart = tm_chart_histogram(position = tm_pos_out("center", "bottom"), width = 30))

tm_shape(World) + 
	tm_polygons ("HPI", 
				 fill.scale = tm_scale_intervals(), 
				 fill.chart = tm_chart_donut())

tm_shape(World) + 
	tm_polygons ("HPI", 
				 fill.scale = tm_scale_intervals(), 
				 fill.chart = tm_chart_box())

tm_shape(World) + 
	tm_polygons ("HPI", 
				 fill.scale = tm_scale_intervals(), 
				 fill.chart = tm_chart_violin())

# with additional ggplot2 code
require(ggplot2)
tm_shape(World) + 
	tm_polygons ("HPI", 
				 fill.scale = tm_scale_intervals(), 
				 fill.chart = tm_chart_bar(extra.ggplot2 = theme(panel.grid.major.y = element_line(colour = "red"))))

tm_shape(land) +
	tm_raster("trees",
			  col.chart = tm_chart_histogram())

## categorical variable
tm_shape(World) + 
	tm_polygons ("economy", 
				 fill.scale = tm_scale_categorical(), 
				 fill.chart = tm_chart_bar())

tm_shape(World) + 
	tm_polygons ("economy", 
				 fill.scale = tm_scale_categorical(), 
				 fill.chart = tm_chart_donut())

# bivariate (in development)
tm_shape(World) +
	tm_polygons(tm_mv("HPI", "well_being"), fill.chart = tm_chart_heatmap())
