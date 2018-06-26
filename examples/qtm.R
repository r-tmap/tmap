data(World, rivers, metro)

# just the map
qtm(World)

# choropleth
qtm(World, fill = "economy", format = "World", style = "col_blind")

# choropleth with more specifications
qtm(World, fill="HPI", fill.n = 9, fill.palette = "div",
    fill.title = "Happy Planet Index", fill.id = "name", 
    style = "gray", format = "World")
# this map can also be created with the main plotting method,
# which is recommended in this case.
\dontrun{
tm_shape(World) +
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

# TIP: check out these examples in view mode, enabled with tmap_mode("view")

\dontrun{
# without arguments, a plain interactive map is shown (the mode is set to view)
qtm()

# search query for OpenStreetMap nominatim
qtm("Amsterdam")
}
