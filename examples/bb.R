## load Netherlands municipality shape
data(NLD_muni)

## get bounding box (similar to sp's function bbox)
bb(NLD_muni)

## extent it by factor 1.10
bb(NLD_muni, ext=1.10)

## convert to longlat
bb(NLD_muni, projection="longlat")

## find with Open Street Map Nominatim:
bb(q="Netherlands")

## load World shape and plot the Netherlands
data(World)
tm_shape(World, bbox=bb(q="Netherlands", projection = "eck4")) +
	tm_polygons() +
	tm_layout("Kingdom of the Netherlands (also in the Caribbean)")
