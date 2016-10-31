\dontrun{
data(metro)

five_cities <- metro[sample(length(metro), 5), ]

addresses <- rev_geocode_OSM(five_cities, zoom = 6)

five_cities <- append_data(five_cities, addresses, fixed.order = TRUE)

# change to interactive mode
current.mode <- tmap_mode("view")
tm_shape(five_cities) +
	tm_markers(text="city")

# restore current mode
tmap_mode(current.mode)
}
