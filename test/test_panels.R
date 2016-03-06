data(World)

summary(World@data)

tm_shape(World) + tm_polygons("pop_est_dens")

tm_shape(World) + tm_fill(c("green", "blue", "yellow", "red"))


tm_shape(World) + tm_polygons("pop_est_dens") + tm_facets("economy", use.panel.labels = F)


tm_shape(World) + tm_polygons("pop_est_dens") + tm_facets(c("economy", "income_grp"))


tm_shape(World) + tm_polygons("pop_est_dens") + tm_layout(title="F fghfgsdklfg g") + tm_facets("economy")


tm_shape(World) + tm_polygons("pop_est_dens") + tm_facets("economy")


tm_shape(World) + tm_polygons("pop_est_dens") + tm_layout(title=list(1:7, letters[1:5])) + tm_facets(c("economy", "income_grp"), use.panel.labels = F)
