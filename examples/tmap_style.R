tmap_style()

tm_shape(World) + tm_polygons("HPI")

tmap_style("cobalt")

tm_shape(World) + tm_polygons("HPI")

# for backwards compatibility, the styles of tmap versions 1-3 are also included:

tmap_style("v3")

tm_shape(World) + tm_polygons("HPI")

tmap_style("cobalt_v3")

tm_shape(World) + tm_polygons("HPI")
