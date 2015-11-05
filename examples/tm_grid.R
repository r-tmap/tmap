data(NLD_muni, Europe)

qtm(NLD_muni, borders = NULL) + tm_grid()

qtm(Europe) + tm_grid(projection="longlat")
