current.mode <- tmap_mode("plot")

data(NLD_muni, Europe)

qtm(NLD_muni, borders = NULL) + tm_grid()

qtm(Europe) + tm_grid(projection="longlat")

tmap_mode(current.mode)
