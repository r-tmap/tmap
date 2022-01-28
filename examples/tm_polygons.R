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
  tm_polygons(fill = "income_grp")

# 
tm_shape(Africa) +
  tm_polygons(fill = "inequality", 
    fill.scale = tm_scale_continuous(values = tmap_pals$pals.brewer$brewer.bupu),
    fill.legend = tm_legend(title = "", orientation = "landscape", position = tm_pos_out("center", "bottom"), frame = FALSE)) + 
tm_title("Inequality index", position = tm_pos_in("right", "top"), frame = FALSE) +
tm_layout(frame = FALSE)


#error: tmap_pals$pals.kovesi$kovesi.rainbow_bgyrm_35_85_c69