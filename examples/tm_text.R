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

