metroAfrica = sf::st_intersection(metro, World[World$continent == "Africa", ])
Africa = World[World$continent == "Africa", ]

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
	tm_labels("name", options = opt_tm_labels(remove_overlap = FALSE))

## to do: replace this example:

\dontrun{
	if (require(rnaturalearth)) {

		airports <- ne_download(scale=10, type="airports", returnclass = "sf")
		airplane <- tmap_icons(system.file("img/airplane.png", package = "tmap"))



		current.mode <- tmap_mode("view")

		tm_shape(NLD_prov, crs = 4326) +
			tm_polygons() +
		tm_shape(airports) +
			tm_symbols(shape = airplane,
					   size = "natlscale",
					   size.legend = tm_legend_hide(),
					   id = "name"
					   ) +
			tm_text(text = "name")

		tmap_mode(current.mode)
	}
}

########################
## plot symbol shapes
########################

# create grid of 25 points in the Atlantic
atlantic_grid = cbind(expand.grid(x = -51:-47, y = 20:24), id = seq_len(25))
x = sf::st_as_sf(atlantic_grid, coords = c("x", "y"), crs = 4326)

tm_shape(x, bbox = tmaptools::bb(x, ext = 1.2)) +
	tm_symbols(shape = "id",
			   size = 2,
			   lwd = 2,
			   fill = "orange",
			   col = "black",
			   shape.scale = tm_scale_asis()) +
	tm_text("id", ymod = -2)

# also supported in view mode :-)
