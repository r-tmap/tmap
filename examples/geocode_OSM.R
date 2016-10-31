geocode_OSM("India")
geocode_OSM("CBS Weg 1, Heerlen")
geocode_OSM("CBS Weg 1, Heerlen", projection = "rd")

\dontrun{
data(metro)

five_cities <- metro[sample(length(metro), 5), ]

locations <- geocode_OSM(five_cities$name_long)

five_cities <- append_data(five_cities, locations, fixed.order = TRUE)

five_cities2 <- five_cities@data
sp::coordinates(five_cities2) <- ~lon+lat

# change to interactive mode
current.mode <- tmap_mode("view")
tmap_mode("view")

tm_shape(five_cities) +
	tm_dots(col = "blue") +
tm_shape(five_cities2) +
	tm_dots(col = "red")

# restore current mode
tmap_mode(current.mode)
}
