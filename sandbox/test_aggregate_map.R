
data(land)
landb <- brick(land)



lb <- aggregate_map(land, fact=2)

qtm(lb, raster="trees")


lb2 <- aggregate_map(landb, fact=2)

qtm(lb2, raster="cover_cls")

lb3 <- aggregate_map(land, fact=2, agg.fun = list(cover=modal, trees=function(i, na.rm) sample(i, size = 1)))
qtm(lb3, raster="cover")
qtm(lb3, raster="trees")


lb4 <- aggregate_map(landb, fact=16, agg.fun = "last")
qtm(lb4, raster="cover")
qtm(lb4, raster="trees")


landr <- raster(land, 3)
lr <- aggregate_map(landr, fact=16, agg.fun = "modal", na.rm=T)
qtm(lr)

data(NLD_muni)

NLD_prov2 <- aggregate_map(NLD_muni, by="province")

NLD_prov2 <- aggregate_map(NLD_muni, by="province", weights = "AREA")

NLD_prov2 <- aggregate_map(NLD_muni, by="province", agg.fun = list(population="sum", origin_non_west="mean", name="modal"), weights = "population")
