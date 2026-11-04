tmap_style()

tm_shape(World) + tm_polygons("HPI")

tmap_style("cobalt")

tm_shape(World) + tm_polygons("HPI")

# for backwards compatibility, the styles of tmap versions 1-3 are also included:

tmap_style("v3")

tm_shape(World) + tm_polygons("HPI")

tmap_style("cobalt_v3")

tm_shape(World) + tm_polygons("HPI")

# the numeric scales of tmap 4.0 to 4.4 (here: thinner data-driven line widths)
tmap_style("white")

tm_shape(World_rivers) + tm_lines(lwd = "strokelwd")

tm_shape(World_rivers) + tm_lines(lwd = "strokelwd") + tm_style("v4.4")

# styles can be combined
tm_shape(World_rivers) + tm_lines(lwd = "strokelwd") + tm_style(c("cobalt", "v4.4"))
