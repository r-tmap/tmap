data(Europe)

# just a map
geo(Europe)

# choropleth
geo(Europe, choro.fill="gdp_cap_est", borders="gray25", theme="Europe", style="kmeans", title="Estimated GDP per capita")
geo(Europe, choro.fill="pop_est_dens", borders="gray25", theme="Europe", style="kmeans", title="Population per km")

# World maps
data(World)
geo(World, choro.fill="gdp_cap_est", borders="gray25", theme="World", style="kmeans", title="Estimated GDP per capita")
geo(World, choro.fill="pop_est_dens", borders="gray25", theme="World", style="kmeans", title="Population per km")


