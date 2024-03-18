# Need to be updated. The following is just to illustrate bgcol(_alpha)
tm_shape(World) + tm_polygons() +
	tm_text("name", bgcol = "economy", bgcol_alpha = .5, shadow = TRUE)

tm_shape(World) +
  tm_text(text = "name", 
    size = .4, 
    bgcol = "economy")

tm_shape(World) +
  tm_text(text = "name", 
    size = .4, 
    bgcol = "economy", 
    bgcol.scale = tm_scale_categorical(values = cols4all::.P$hcl$cat$set2),
    bgcol_alpha = "pop_est",
    bgcol_alpha.scale = tm_scale_intervals(style = "kmeans"))

tm_shape(metro) +
	tm_text(text = "name", size = "pop2020", size.legend = tm_legend_hide())

## xymod
metro$initial = substr(metro$name,1,1)
metro$az = (match(metro$initial, LETTERS)-1) /25

tm_shape(metro) +
	tm_dots("red") +
	tm_text("initial", ymod = "az")

# angle
tm_shape(World) +
	tm_polygons() +
tm_shape(metro) +
	tm_text(text = "name", size = "pop2020",
			angle = -30, shadow = TRUE)

metro$upside_down = ifelse(sf::st_coordinates(metro)[,2] < 0, 180, 0)
tm_shape(metro) +
	tm_text(text = "name", size = "pop2020",
			angle = "upside_down", size.legend = tm_legend_hide())

DE = World[World$name == "Germany",]
rivers_DE = st_intersection(rivers, DE)

tm_shape(DE) +
	tm_polygons() +
tm_shape(rivers_DE) +
	tm_lines(lwd = "strokelwd", lwd.scale = tm_scale_asis()) + 
	tm_text("name", along.lines = TRUE)

metroAfrica = sf::st_intersection(metro, World[World$continent == "Africa", ])

# remove.overlap
tm_shape(metroAfrica) +
	tm_text("name", bgcol = "yellow") +
	tm_dots("red")
	
tm_shape(metroAfrica) +
	tm_text("name", bgcol = "yellow", remove.overlap = TRUE) +
	tm_dots("red")

# tm_labels uses a labeling algorithm that uses randomization (so rerunning this code may give different outcomes, unless set.seed is used)
tm_shape(metroAfrica) +
	tm_labels("name", bgcol = "yellow") +
	tm_dots("red")
