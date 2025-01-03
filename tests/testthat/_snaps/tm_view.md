# tm_view() errors correctly with wrong position

    Code
      tm_shape(World) + tm_crs("auto") + tm_polygons(fill = "pop_est") + tm_view(
        control.bases = c("shp1", "shp2"), control.collapse = TRUE, control.position = c(
          "top", "right"))
    Condition
      Error in `leaflet_pos()`:
      ! `position` must specify the vertical argument first. i.e. "top", "bottom".

