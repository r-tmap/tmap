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
	tm_labels("name")

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

##### v3 examples

current.mode <- tmap_mode("plot")

data(World, metro)

tm_shape(World) +
	tm_text("name", size="AREA")


tm_shape(World) +
	tm_text("name", size="pop_est", col="continent", palette="Dark2",
			title.size = "Population", title.col="Continent") +
	tm_legend(outside = TRUE)

tmap_mode("view")

\dontrun{
	require(tmaptools)
	metro_aus <- crop_shape(metro, bb("Australia"))
	
	tm_shape(metro_aus) +
		tm_dots() +
		tm_text("name", just = "top")
	
	# alternative
	tm_shape(metro_aus) +
		tm_markers(text = "name")
}

# restore current mode
tmap_mode(current.mode)


