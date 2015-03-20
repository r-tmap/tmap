data(Europe)
data(World)
data(cities)

# just the map
qtm(Europe)

# choropleth
qtm(World, fill = "economy", text="iso_a3", text.cex = "AREA", fill.palette="-Blues", theme = "World", title="Economy")

qtm(Europe, fill="gdp_cap_est", text="iso_a3", text.cex="pop_est", 
    title="GDP per capita", fill.textNA="Non-European countries")

qtm(World, fill="pop_est_dens", theme="World", fill.style="kmeans", title="Population per km")

# bubble map
qtm(World, borders = "grey60") + qtm(cities, bubble.size = "pop_max", bubble.col="steelblue", legend.show=FALSE, title="Cities of the World", theme = "World", bubble.scale=.5)
