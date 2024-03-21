metroAfrica = sf::st_intersection(metro, World[World$continent == "Africa", ])
Africa = World[World$continent == "Africa", ]

tm_shape(land) +
	tm_raster("cover_cls", 
			  col.scale = tm_scale(values = cols4all::c4a("brewer.pastel1")[c(3,7,7,2,6,1,2,2)]),
			  col.legend = tm_legend_hide()) +
	tm_shape(rivers) +
	tm_lines(lwd = "strokelwd", lwd.scale = tm_scale_asis(values.scale = .3), col = cols4all::c4a("brewer.pastel1")[2]) +
	tm_shape(Africa, is.main = TRUE) + 
	tm_borders() +
	tm_shape(metroAfrica) +
	tm_symbols(fill = "red", shape = "pop2020", size = "pop2020", 
			   size.scale = tm_scale_intervals(breaks = c(1, 2, 5, 10, 15, 20, 25) * 1e6, values.range = c(0.2,2)),
			   size.legend = tm_legend("Population in 2020"),
			   shape.scale = tm_scale_intervals(breaks = c(1, 2, 5, 10, 15, 20, 25) * 1e6, values = c(21, 23, 22, 21, 23, 22)),
			   shape.legend = tm_legend_combine("size")) +
	tm_labels("name", options = opt_tm_labels(remove.overlap = FALSE))


##### tmap v3

data(World, metro)
metro$growth <- (metro$pop2020 - metro$pop2010) / (metro$pop2010 * 10) * 100

tm_shape(World) +
	tm_fill("grey70") +
	tm_shape(metro) +
	tm_bubbles("pop2010", col = "growth", 
			   border.col = "black", border.alpha = .5, 
			   style="fixed", breaks=c(-Inf, seq(0, 6, by=2), Inf),
			   palette="-RdYlBu", contrast=1, 
			   title.size="Metro population", 
			   title.col="Growth rate (%)") + 
	tm_format("World")

tm_shape(metro) +
	tm_symbols(size = "pop2010", col="pop2010", shape="pop2010",
			   legend.format = list(text.align="right", text.to.columns = TRUE)) +
	tm_legend(outside = TRUE, outside.position = "bottom", stack = "horizontal")


if (require(ggplot2) && require(dplyr) && require(tidyr) && require(tmaptools) && require(sf)) {
	data(NLD_prov)
	
	origin_data <- NLD_prov %>% 
		st_set_geometry(NULL) %>% 
		dplyr::mutate(FID = factor(dplyr::row_number())) %>% 
		dplyr::select(FID, origin_native, origin_west, origin_non_west) %>% 
		tidyr::pivot_longer(
			cols = c(origin_native, origin_west, origin_non_west),
			names_to = "origin",
			values_to = "perc",
			names_transform = as.factor
		) %>%
		dplyr::arrange(origin, FID) %>% 
		as.data.frame()
	
	origin_cols <- get_brewer_pal("Dark2", 3)
	
	grobs <- lapply(split(origin_data, origin_data$FID), function(x) {
		ggplotGrob(ggplot(x, aes(x="", y=-perc, fill=origin)) +
				   	geom_bar(width=1, stat="identity") +
				   	scale_y_continuous(expand=c(0,0)) +
				   	scale_fill_manual(values=origin_cols) +
				   	theme_ps(plot.axes = FALSE))
	})
	
	names(grobs) <- NLD_prov$name
	
	tm_shape(NLD_prov) +
		tm_polygons(group = "Provinces") +
		tm_symbols(size = "population", shape="name", 
				   shapes = grobs, 
				   sizes.legend = c(.5, 1,3)*1e6, 
				   scale = 1, 
				   legend.shape.show = FALSE, 
				   legend.size.is.portrait = TRUE, 
				   shapes.legend = 22, 
				   title.size = "Population",
				   group = "Charts",
				   id = "name",
				   popup.vars = c("population", "origin_native",
				   			   "origin_west", "origin_non_west")) +
		tm_add_legend(type = "fill", 
					  group = "Charts",
					  col = origin_cols, 
					  labels = c("Native", "Western", "Non-western"), 
					  title = "Origin") +
		tm_format("NLD")
	
	grobs2 = grobs
	grobs2[[6]] = 21
	
	NLD_prov$population[1:5] = 500000
	tm_shape(NLD_prov) +
		tm_polygons(group = "Provinces") +
		tm_symbols(shape="name", 
				   fill = "red",
				   col = "blue",
				   size = "population",
				   size.scale = tm_scale_continuous(values.scale = 4),
				   shape.scale = tm_scale_categorical(values = grobs2),
				   shape.legend = tm_legend_hide())
	
}



# TIP: check out these examples in view mode, enabled with tmap_mode("view")

\dontrun{
	if (require(rnaturalearth)) {
		
		airports <- ne_download(scale=10, type="airports", returnclass = "sf")
		airplane <- tmap_icons(system.file("img/airplane.png", package = "tmap"))
		
		current.mode <- tmap_mode("view")
		tm_shape(airports) +
			tm_symbols(shape=airplane, size="natlscale",
					   legend.size.show = FALSE, scale=1,
					   border.col = NA, id="name", popup.vars = TRUE) 
		#+	tm_view(set.view = c(lon = 15, lat = 48, zoom = 4))
		tmap_mode(current.mode)
	}
}

#####################################################################################

\dontrun{
	# plot all available symbol shapes:
	if (require(ggplot2)) {
		ggplot(data.frame(p=c(0:25,32:127))) +
			geom_point(aes(x=p%%16, y=-(p%/%16), shape=p), size=5, fill="red") +
			geom_text(mapping=aes(x=p%%16, y=-(p%/%16+0.25), label=p), size=3) +
			scale_shape_identity() +
			theme(axis.title=element_blank(),
				  axis.text=element_blank(),
				  axis.ticks=element_blank(),
				  panel.background=element_blank())
	}
}


tm_shape(metro) + 
	tm_symbols("pop2010", size.scale = tm_scale_continuous(n = 8, values.scale = 3), shape = 22)



data("NLD_prov")
NLD_prov$x = seq(10, by = 5, length.out = 12)

tm_shape(NLD_prov) + 
	tm_symbols(size = "x", size.scale = tm_scale_continuous(values.scale = 4)) +
	tm_text(text = "x")

tm_shape(metro) + 
	tm_symbols("pop2010", fill = "pop2020", 
			   fill.scale = tm_scale_continuous(), 
			   size.scale = tm_scale_continuous(), shape = 22)

