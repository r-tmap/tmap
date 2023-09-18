skip_on_cran()
test_that("Aux layers work.", {
	
	data(World,metro,land)
	
	World$pop_class = cut(World$pop_est, breaks = c(0, 10, 100, 1000, Inf) * 1e6, labels = c("Small", "Medium", "Large", "Extra Large"))					   
	World$HPI_class = cut(World$HPI, breaks = seq(10, 50, by = 10))
	World$well_being_class = cut(World$well_being, breaks = seq(2, 8, by = 2))
	World$footprint_class = cut(World$footprint, breaks = seq(0, 16, by = 4))
	
	metro$pop2020_class = cut(metro$pop2020, breaks = c(.5, 1.5, 2.5, 5, 15, 40) * 1e6)
	Africa = World[World$continent == "Africa", ]
})




txt = function(x) print(grid::grid.text(x, gp = gpar(cex = 3)))
v3 = function(e) {
	if (tmapV == "3") {
		print(e)
	} else {
		txt("Only for tmap 3")
	}
	invisible(NULL)
}
v4 = function(e) {
	if (tmapV == "4") {
		print(e)
	} else {
		txt("Only for tmap 4")
	}
	invisible(NULL)
}
txt(paste("Loaded tmap version", tmapV))
```

test_that("Base layer works at different positions", {
	tm_basemap("OpenStreetMap")+
		tm_shape(Africa) +
		tm_polygons("HPI", fill.scale = tm_scale(values = "viridis")) +
		tm_symbols(size = "pop_est", fill = "purple", size.scale = tm_scale(values = tmap_seq(0, 2, "sqrt"))) +
		tm_facets_wrap("well_being_class") +
		tm_shape(metro) +
		tm_symbols(fill = "pop2020") +
		tm_layout(bg.color = "grey95")
	
	tm_shape(Africa) +
		tm_polygons("HPI", fill.scale = tm_scale(values = "viridis")) +
		tm_basemap("OpenStreetMap")+
		tm_symbols(size = "pop_est", fill = "purple", size.scale = tm_scale(values = tmap_seq(0, 2, "sqrt"))) +
		tm_facets_wrap("well_being_class") +
		tm_shape(metro) +
		tm_symbols(fill = "pop2020") +
		tm_layout(bg.color = "grey95")
	
	tm_shape(Africa) +
		tm_polygons("HPI", fill.scale = tm_scale(values = "viridis")) +
		tm_symbols(size = "pop_est", fill = "purple", size.scale = tm_scale(values = tmap_seq(0, 2, "sqrt"))) +
		tm_facets_wrap("well_being_class") +
		tm_shape(metro) +
		tm_basemap("OpenStreetMap")+
		tm_symbols(fill = "pop2020") +
		tm_layout(bg.color = "grey95")
	
	tm_shape(Africa) +
		tm_polygons("HPI", fill.scale = tm_scale(values = "viridis")) +
		tm_symbols(size = "pop_est", fill = "purple", size.scale = tm_scale(values = tmap_seq(0, 2, "sqrt"))) +
		tm_facets_wrap("well_being_class") +
		tm_shape(metro) +
		tm_symbols(fill = "pop2020") +
		tm_basemap("OpenStreetMap")+
		tm_layout(bg.color = "grey95")
	
	tm_shape(Africa) + tm_basemap("OpenStreetMap") +
		tm_polygons("pop_est_dens") +
		tm_facets_grid("footprint_class", "HPI_class")
	
	tm_shape(Africa) +
		tm_basemap("Esri.NatGeoWorldMap") +
		tm_borders(lwd = 2) +
		tm_shape(metro) +
		tm_symbols(fill = "pop2020_class", col = "black")
})

test_that("Projected CRS warp work", {
	tm_shape(NLD_prov) +
		tm_basemap("OpenStreetMap") +
		tm_borders()
})


test_that("Reproject shape to long-lat works.", {
	tm_shape(NLD_prov, crs = 4326) +
		tm_basemap("OpenStreetMap") +
		tm_borders(lwd = 4)
	
	
	tm_shape(NLD_muni, crs = 4326) +
		tm_basemap("OpenStreetMap") +
		tm_polygons(c("pop_0_14", "pop_15_24", "pop_25_44", "pop_45_64", "pop_65plus"), fill.free = FALSE, fill_alpha = 0.5)
})
