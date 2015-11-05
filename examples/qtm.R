data(Europe, World, metro)

# just the map
qtm(Europe)

# choropleth
qtm(World, fill = "economy", text="iso_a3", text.size = "AREA", format = "World", 
	fill.title="Economy", style="col_blind")

qtm(Europe, fill="gdp_cap_est", text="iso_a3", text.size="pop_est", 
    fill.title="GDP per capita", fill.textNA="Non-European countries", 
	format="Europe", style="cobalt")

qtm(World, fill="HPI", fill.n=9, fill.palette="div", fill.auto.palette.mapping=FALSE, 
	fill.title="Happy Planex Index", format="World")

# bubble map
qtm(World, borders = NULL) + qtm(metro, bubble.size = "pop2010", 
    bubble.title.size="Metropolitan Areas", bubble.scale=.5, format = "World")
