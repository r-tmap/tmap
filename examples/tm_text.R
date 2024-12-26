tm_shape(World, bbox = World) +
	tm_text("name", size="pop_est", col="continent",
			col.scale = tm_scale_categorical(values = "seaborn.dark"),
			col.legend = tm_legend_hide(),
			size.scale = tm_scale_continuous(values.scale = 4),
			size.legend = tm_legend_hide())

metro$upside_down = ifelse(sf::st_coordinates(metro)[,2] < 0, 180, 0)
tm_shape(metro) +
	tm_text(text = "name", size = "pop2020",
			angle = "upside_down", size.legend = tm_legend_hide(),
			col = "upside_down",
			col.scale = tm_scale_categorical(values = c("#9900BB", "#228822")),
			col.legend = tm_legend_hide()) +
	tm_title_out("Which Hemisphere?", position = tm_pos_out("center", "top", pos.v = "bottom"))


metroAfrica = sf::st_intersection(metro, World[World$continent == "Africa", ])
Africa = World[World$continent == "Africa", ]

# to do: update land
library(sf)
st_crs(land) = 4326

tm_shape(land) +
	tm_raster("cover_cls",
			  col.scale = tm_scale(
			  	  values = cols4all::c4a("brewer.pastel1")[c(3,7,7,2,6,1,2,2)]
			  ),
			  col.legend = tm_legend_hide()) +
tm_shape(World_rivers) +
	tm_lines(lwd = "strokelwd", lwd.scale = tm_scale_asis(values.scale = .3),
			 col = cols4all::c4a("brewer.pastel1")[2]) +
    tm_shape(Africa, is.main = TRUE) +
	tm_borders() +
    tm_shape(metroAfrica) +
	tm_symbols(fill = "red", shape = "pop2020", size = "pop2020",
			   size.scale = tm_scale_intervals(
			   	    breaks = c(1, 2, 5, 10, 15, 20, 25) * 1e6,
			   	    values.range = c(0.2,2)
			   ),
			   size.legend = tm_legend("Population in 2020"),
			   shape.scale = tm_scale_intervals(
			   	   breaks = c(1, 2, 5, 10, 15, 20, 25) * 1e6,
			   	   values = c(21, 23, 22, 21, 23, 22)
			   ),
			   shape.legend = tm_legend_combine("size")) +
	tm_labels("name")

tm_shape(metroAfrica) +
	tm_markers(text = "name",
			   dots_fill = "red",
			   dots_size = 0.3)

tm_shape(metroAfrica) +
	tm_markers(text = "name",
			   dots_shape = marker_icon(),
			   dots_col = NA,
			   dots_fill = "red",
			   dots_size = 2,
			   ymod = -0.25,
			   options = opt_tm_markers(point.label = FALSE, remove_overlap = TRUE))
