# load Africa country data
data(World)
Africa = World[World$continent == "Africa", ]
Africa_border = sf::st_make_valid(sf::st_union(sf::st_buffer(Africa, 0.001))) # slow and ugly

# without specifications
tm_shape(Africa_border) + tm_polygons()
tm_shape(Africa_border) + tm_fill()
tm_shape(Africa_border) + tm_borders()

# specification with visual variable values
tm_shape(Africa) + 
  tm_polygons(fill = "limegreen", col = "purple", lwd = 3, lty = "solid", col_alpha = 0.3) +
tm_shape(Africa_border) +
  tm_borders("darkred", lwd = 4)

# specification with a data variable
tm_shape(Africa) +
  tm_polygons(fill = "income_grp", fill.scale = tm_scale_categorical(values = "tol.muted"))

# continuous color scale with landscape legend
tm_shape(Africa) +
  tm_polygons(fill = "inequality", 
    fill.scale = tm_scale_continuous(values = "kovesi.rainbow_bu_pk"),
    fill.legend = tm_legend(title = "", orientation = "landscape",
    						position = tm_pos_out("center", "bottom"), frame = FALSE)) + 
	tm_title("Inequality index", position = tm_pos_in("right", "TOP"), frame = FALSE) +
	tm_layout(frame = FALSE)

####################################
########## v3 ######################
####################################

tmap_style("v3")

data(World)

# Constant fill
tm_shape(World) + tm_fill("darkolivegreen3") + tm_format("World", title="A green World")

# Borders only
tm_shape(World) + tm_borders()

# Data variable containing colours values
World$isNLD <- ifelse(World$name=="Netherlands", "darkorange", "darkolivegreen3")
tm_shape(World) +
	tm_fill("isNLD") +
	tm_layout("Find the Netherlands!")

tm_shape(World, projection = "+proj=eck4") +
	tm_polygons("economy", title="Economy", id="name") +
	tm_text("iso_a3", size="AREA", scale=1.5) +
	tm_format("World")

# Numeric data variable
tm_shape(World, projection = "+proj=eck4") +
	tm_polygons("HPI", palette="RdYlGn", style="cont", n=8,
				title="Happy Planet Index", id="name") +
	tm_text("iso_a3", size="AREA", scale=1.5) +
	tm_style("grey") +
	tm_format("World")

\dontrun{
	data(NLD_prov, NLD_muni)	
	# Map coloring algorithm
	tm_shape(NLD_prov) +
		tm_fill("name", legend.show = FALSE) +
		tm_shape(NLD_muni) +
		tm_polygons("MAP_COLORS", palette="Greys", alpha = .25) +
		tm_shape(NLD_prov) +
		tm_borders(lwd=2) +
		tm_text(I("name"), col = "white", shadow=TRUE) +
		tm_format("NLD", title="Dutch provinces and\nmunicipalities", bg.color="white")
	
	# Cartogram
	if (require(cartogram)) {
		NLD_prov_pop <- cartogram(NLD_prov, "population")
		tm_shape(NLD_prov_pop) +
			tm_polygons("origin_non_west", title = "Non-western origin (%)")
	}
}

# TIP: check out these examples in view mode, enabled with tmap_mode("view")
