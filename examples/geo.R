data(Europe)

# just the map
geo(Europe)

# choropleth
geo(Europe, choro.fill="gdp_cap_est", theme="Europe", style="kmeans", title="Estimated GDP per capita")


# bubblemap
geo(Europe, bubble.size="pop_est", bubble.col="part", scale=2, theme="Europe")


# World maps
data(World)
geo(World, choro.fill="pop_est_dens", theme="World", style="kmeans", title="Population per km")
