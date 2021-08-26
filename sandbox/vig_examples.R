#
data(World, metro)

# arguments are grouped into:
tm_shape(World) +
	tm_polygons(fill = "HPI", 
				fill.scale = tm_scale_intervals(values = "RdYlGn"), 
				fill.legend = tm_legend(title = "Happy Planet Index"))


# more aesthetics
tm_shape(World, crs = "+proj=eck4") +
	tm_polygons(fill = "HPI", 
				fill.scale = tm_scale_continuous(values = "RdYlGn"), 
				fill.legend = tm_legend(title = "Happy Planet Index", position = tm_lp_out("left", "center")),
				# col = "life_exp",
				# col.scale = tm_scale_continuous(values = "-grays", value.neutral = "gray30"),
				# col.legend = tm_legend(title = "Life Expectancy", position = tm_lp_out("right", "center")),
				lwd = "pop_est_dens",
				lwd.scale = tm_scale_continuous(value.neutral = 1, values = c(0, 5)),
				lwd.legend = tm_legend(title = "Population Density", position = tm_lp_out("right", "center"), space = 0.5))



# extensions: cartogram example
tm_shape(World, crs = "+proj=eck4") +
	tm_cartogram(size = "pop_est",
				 fill = "HPI", 
				 fill.scale = tm_scale_intervals(values = "RdYlGn"),
				 fill.legend = tm_legend(title = "Happy Planet Index")) +
	tm_place_legends_right(width = 0.2)



# combined legends
tm_shape(metro) +
	tm_symbols(fill = "pop2020",
			   size = "pop2020",
			   size.scale = tm_scale_intervals(),
			   size.legend = tm_legend_combine("fill"))



	




tm_shape(World, crs = "+proj=robin") +
	tm_borders() +
	tm_shape(World) +
	tm_polygons(fill = "life_exp", fill.free = c(FALSE, TRUE), fill.scale = tm_scale_intervals(values = tmap_pals$pals.brewer$brewer.greens)) +
	tm_symbols(size = "gdp_cap_est", size.free = c(TRUE, FALSE), fill = "red") +
	tm_facets_grid("income_grp", "economy") +
	tm_options(meta.margins = c(.2,0,0,.1))