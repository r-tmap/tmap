data(Europe)
data(World)
data(metro)

# just the map
qtm(Europe)

# choropleth
qtm(World, fill = "economy", text="iso_a3", text.size = "AREA", 
    fill.palette="-Blues", format = "World", fill.title="Economy", colors="beaver")

qtm(Europe, fill="gdp_cap_est", text="iso_a3", text.size="pop_est", 
    fill.title="GDP per capita", fill.textNA="Non-European countries")

qtm(World, fill="pop_est_dens", theme="World", fill.style="kmeans", fill.title="Population per km")

qtm(World, fill="HPI", fill.n=9, fill.palette="RdYlGn", fill.auto.palette.mapping=FALSE)

# bubble map
qtm(World, borders = NA) + qtm(metro, bubble.size = "pop2010", bubble.col="purple", 
    bubble.title.size="Metropolitan Areas", format = "World", bubble.scale=.5)
