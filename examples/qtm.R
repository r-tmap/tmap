data(World, rivers, metro)

# just the map
qtm(World)

# choropleth
qtm(World, fill = "economy", format = "World", style = "col_blind", projection = "+proj=eck4")

# choropleth with more specifications
qtm(World, fill="HPI", fill.n = 9, fill.palette = "div",
    fill.title = "Happy Planet Index", fill.id = "name", 
    style = "gray", format = "World", projection = "+proj=eck4")
# this map can also be created with the main plotting method,
# which is recommended in this case.
\dontrun{
tm_shape(World, projection = "+proj=eck4") +
    tm_polygons("HPI", n = 9, palette = "div",
        title = "Happy Planet Index", id = "name") +
tm_style("gray") +
tm_format("World")
}

# bubble map
\dontrun{
qtm(World, borders = NULL) + 
qtm(metro, symbols.size = "pop2010", 
    symbols.title.size= "Metropolitan Areas", 
    symbols.id= "name",
    format = "World")
}

# dot map
\dontrun{
current.mode <- tmap_mode("view")
qtm(metro, bbox = "China")
tmap_mode(current.mode) # restore mode
}

\dontrun{
# without arguments, a plain interactive map is shown (the mode is set to view)
qtm()

# search query for OpenStreetMap nominatim
qtm("Amsterdam")
}
