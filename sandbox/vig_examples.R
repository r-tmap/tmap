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
tm_shape(World) +
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


World$HPI_class = cut(World$HPI, breaks = seq(10, 50, by = 10))
World$well_being_class = cut(World$well_being, breaks = seq(2, 8, by = 2))
World$footprint_class = cut(World$footprint, breaks = seq(0, 16, by = 4))

tm_shape(World) +
	tm_polygons(fill = "life_exp") +
	tm_facets_grid("well_being_class", "footprint_class") +
tm_shape(World, is.main = TRUE) +
	tm_borders()
	

tm_shape(World) +
	tm_polygons(fill = "red") +
	tm_polygons(fill = NA, col = "blue")


plot(World$HPI, World$well_being)
plot(World$HPI, World$footprint)
plot(World$HPI, World$inequality)

plot(World$well_being, World$footprint)

plot(World$well_being, World$life_exp)

tm_shape(World) +
	tm_borders() +
tm_shape(World) +
	tm_polygons(fill = "life_exp", fill.free = c(FALSE, TRUE)) +
	tm_symbols(size = "gdp_cap_est", size.free = c(TRUE, FALSE)) +
	tm_facets_grid("income_grp", "economy") +
	tm_options(meta.margins = c(.2,0,0,.1))

