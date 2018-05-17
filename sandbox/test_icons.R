library(leaflet)
data(Europe, metro)
metro$sel <- metro$pop2020 > 3e6

ic <- tmap_icons("http://icons.iconarchive.com/icons/oxygen-icons.org/oxygen/48/Apps-preferences-desktop-icons-icon.png")
ic2 <- tmap_icons(system.file("htmlwidgets/lib/leaflet/images/marker-icon.png", package="leaflet"))
ics <- tmap_icons(c("http://megaicons.net/static/img/icons_title/22/119/title/city-building-icon.png",
					"https://cdn0.iconfinder.com/data/icons/buildings-8/512/buildings_street_town_megapolis_megalopolis_city_center_urban-128.png"))
qtm(Europe, bbox="Italy") +
	tm_shape(metro) +
	tm_markers(shape="sel", text="name", shapes=ics, just=c("center", "bottom"))


Italy <- Europe[Europe$name=="Italy", ]
qtm(Italy) +
	tm_shape(metro) +
	tm_markers(shape=ic, text="name")


grb <- ggplotGrob(qplot(mpg, wt, data = mtcars))
qtm(Europe, bbox="Italy") +
	tm_shape(metro) +
	tm_markers(shape=grb, text="name")
