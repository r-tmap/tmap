data(Europe)
data(World)
data(metro)

# just the map
qtm(Europe)

# choropleth
qtm(World, fill = "economy", text="iso_a3", text.size = "AREA", 
    fill.palette="-Blues", theme = "World", fill.title="Economy")

qtm(Europe, fill="gdp_cap_est", text="iso_a3", text.size="pop_est", 
    fill.title="GDP per capita", fill.textNA="Non-European countries")

qtm(World, fill="pop_est_dens", theme="World", fill.style="kmeans", fill.title="Population per km")

# bubble map
qtm(World, borders = NA) + qtm(metro, bubble.size = "pop2010", bubble.col="purple", 
    bubble.title.size="Metropolitan Areas", theme = "World", bubble.scale=.5)
