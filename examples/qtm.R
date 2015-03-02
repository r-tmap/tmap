# load shape objects
data(Europe)
data(World)

# just the map
qtm(Europe)

# choropleth
qtm(Europe, fill="gdp_cap_est", text="iso_a3", text.cex="pop_est", 
    title="GDP per capita", fill.textNA="Non-European countries")

qtm(World, fill="pop_est_dens", theme="World", fill.style="kmeans", title="Population per km")

# bubble map
qtm(Europe, bubble.size="pop_est", bubble.col="part", theme="Europe")

