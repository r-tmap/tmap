## load shapes
data(NLD_muni)
data(World)

## get bounding box (similar to sp's function bbox)
bb(NLD_muni)

## extent it by factor 1.10
bb(NLD_muni, ext=1.10)

## convert to longlat
bb(NLD_muni, projection="longlat")

## change existing bounding box
tm_shape(NLD_muni, bbox=bb(NLD_muni, ext=1.5)) + tm_polygons()
tm_shape(NLD_muni, bbox=bb(NLD_muni, width=2, relative = TRUE)) + tm_polygons()
tm_shape(NLD_muni, bbox=bb(NLD_muni, xlim=c(.25, .75), ylim=c(.25, .75), relative = TRUE)) + 
	tm_polygons()
tm_shape(NLD_muni, bbox=bb("Limburg", projection = get_projection(NLD_muni))) + tm_polygons()

tm_shape(World, bbox=bb("Italy", projection = "eck4")) + tm_polygons()

## create new bounding box
tm_shape(NLD_muni, bbox=bb(cx=190000, cy=330000, width=50000, height=50000)) + tm_polygons()
