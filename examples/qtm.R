data(World, rivers, metro)

# just the map
qtm(World)

# choropleth
qtm(World, fill = "economy", format="World", style="col_blind")

qtm(World, fill="HPI", fill.n=9, fill.palette="div", fill.auto.palette.mapping=FALSE, 
	fill.title="Happy Planet Index", format="World", style="gray")

# bubble map
qtm(World, borders = NULL) + qtm(metro, bubble.size = "pop2010", 
    bubble.title.size="Metropolitan Areas", bubble.scale=.5, format = "World")

# TIP: check out these examples in view mode, enabled with tmap_mode("view")
