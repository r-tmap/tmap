data(NLD_prov)

s1 <- NLD_prov
s2 <- set_polygon_directions(NLD_prov)

get_polygon_directions(s1)
get_polygon_directions(s2)

s3 <- set_projection(s2, "longlat")
get_polygon_directions(s3)

s2@polygons[[3]]@Polygons[[1]]
s4 <- set_polygon_directions(s3)
get_polygon_directions(s4)
