data(Europe, land)

land_europe <- crop_shape(land, Europe)

qtm(land_europe, raster="trees", style="natural")
