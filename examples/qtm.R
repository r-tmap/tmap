data(Europe)
data(World)
data(metro)

# just the map
qtm(Europe)

# choropleth
qtm(World, fill = "economy", text="iso_a3", text.size = "AREA", 
	fill.palette="-Blues", theme = "World", title="Economy")

qtm(Europe, fill="gdp_cap_est", text="iso_a3", text.size="pop_est", 
    title="GDP per capita", fill.textNA="Non-European countries")

qtm(World, fill="pop_est_dens", theme="World", fill.style="kmeans", title="Population per km")

# bubble map
qtm(World, borders = NA) + qtm(metro, bubble.size = "X2010", bubble.col="purple", 
	title="Metropolitan Areas", theme = "World", bubble.scale=.5)
