data(World)

data(NLD_muni)

NLD_muni$area <- approx_areas(NLD_muni, total.area.km2 = 33893)

tm_shape(NLD_muni) +
	tm_bubbles(size="area") +
	tm_layout("Area in km2")
