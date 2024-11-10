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
  tm_polygons(fill = "limegreen", col = "purple", lwd = 2, lty = "solid", col_alpha = 0.3) +
  tm_text("name", options = opt_tm_text(remove.overlap = TRUE)) +
tm_shape(Africa_border) +
  tm_borders("darkred", lwd = 3)

# specification with a data variable
tm_shape(Africa) +
  tm_polygons(fill = "income_grp", fill.scale = tm_scale_categorical(values = "-tol.muted"))


# continuous color scale with landscape legend
tm_shape(Africa) +
  tm_polygons(fill = "inequality",
    fill.scale = tm_scale_continuous(values = "-scico.roma"),
    fill.legend = tm_legend(
    	title = "", orientation = "landscape",
    	position = tm_pos_out("center", "bottom"), frame = FALSE
    	)) +
tm_shape(Africa_border) +
	tm_borders(lwd = 2) +
tm_title("Inequality index", position = tm_pos_in("right", "TOP"), frame = FALSE) +
tm_layout(frame = FALSE)

# bivariate scale
tm_shape(World) +
	tm_polygons(tm_vars(c("inequality", "well_being"), multivariate = TRUE))
