data(Europe)

# just the map
geo(Europe)

# choropleth
geo(Europe, fill="gdp_cap_est", theme="Europe", style="kmeans", title="Estimated GDP per capita")


# bubblemap
geo(Europe, bubble.size="pop_est", bubble.col="part", theme="Europe")


# World maps
data(World)
geo(World, fill="pop_est_dens", theme="World", style="kmeans", title="Population per km")
