data(Europe, metro, rivers, land)
metro$growth <- (metro$pop2020 - metro$pop2010) / (metro$pop2010 * 10) * 100

print_style <- function(style) {
	on.exit(dev.off())
	require(grid)
	png(paste0("styles_", style, ".png"), width=1920, height=1080)
	tml <- do.call(paste0("tm_style_", style), args=list())
	grid.newpage()
	pushViewport(viewport(layout = grid.layout(3,2)))
	print(tm_shape(Europe) +
		  	tm_polygons() +
		  	tm_text("iso_a3", size="AREA") +
		  	tm_bubbles() +
		  	tm_shape(rivers) +
		  	tm_lines() + 
		  	tm_compass() +
		  	tm_scale_bar() +
		  	tml + tm_format_Europe(title="Fixed aesthetics"),
		  vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
	print(tm_shape(Europe) +
		  	tm_polygons("gdp_cap_est", style="kmeans") + 
		  	tm_text("iso_a3", size="AREA") + tml + tm_format_Europe(title="Choropleth"),
		  vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
	print(tm_shape(Europe) +
		  	tm_polygons("HPI", palette="div", auto.palette.mapping=FALSE, n=9) +
		  	tm_text("iso_a3", size="AREA") + tml + tm_format_Europe(title="Choropleth (diverging)"),
		  vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
	print(tm_shape(Europe) +
		  	tm_polygons("economy") + 
		  	tm_text("iso_a3", size="AREA") + tml + tm_format_Europe(title="Categorical map"),
		  vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
	print(tm_shape(Europe) +
		  	tm_polygons() + 
		  	tm_shape(metro) +
		  	tm_dots() + 
		  	tm_grid(projection = "longlat") +
		  	tml + tm_format_Europe(title="Dot map"),
		  vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
	print(tm_shape(Europe) +
		  	tm_polygons() +
		  	tm_shape(metro) +
		  	tm_bubbles(size = "pop2010", col = "growth", breaks = c(-Inf, -2, -1, -.5, .5, 1, 2, Inf)) + tml + tm_format_Europe(title="Bubble map"),
		  vp = viewport(layout.pos.row = 3, layout.pos.col = 2))
	upViewport()
}

styles <- c("default", "grey", "classic", "bw", "cobalt", "albatross", "beaver", "col_blind")

lapply(styles, print_style)



