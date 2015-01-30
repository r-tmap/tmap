data(World)

World$area_approx <- approx_areas(World) /1e6
World$area_diff <- (World$area_approx - World$area) / World$area

qtm(World, fill="area_diff", 
	title="Relative difference between	approximated and specified area size", 
	inner.margins=c(0,.1,.2,0))
