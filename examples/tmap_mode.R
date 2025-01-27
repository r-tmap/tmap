current.mode = tmap_mode()

tmap_mode("plot")

tm_shape(World) + tm_polygons("HPI")

tmap_mode("view")

tm_shape(World) + tm_polygons("HPI")

ttm()

tm_shape(World) + tm_polygons("HPI")

tmap_mode(current.mode)
