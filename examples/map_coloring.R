data(World, metro)

World$color <- map_coloring(World, palette="Pastel2")
qtm(World, fill = "color")

# map_coloring used indirectly:
qtm(World, fill = "MAP_COLORS")
