data(World, Europe, NLD_muni, NLD_prov, metro, land, rivers)

## not a shape
qtm(iris)

tm_shape(iris) +
	tm_fill("Species")

## Invalid variable name
tm_shape(World) + 
	tm_fill("fake")

## No tm_shape
tm_fill("pop_est")

## Wrong order
tm_fill("pop_est") + tm_shape(World)

## Wrong type of shape
tm_shape(NLD_muni) +
	tm_raster("population")

tm_shape(NLD_muni) +
	tm_lines("population")

tm_shape(rivers) +
	tm_fill("type")

tm_shape(land) +
	tm_fill("trees")

tm_shape(land) +
	tm_bubbles("trees")

tm_shape(land) +
	tm_lines("trees")


## categorical bubbles size/line lwd 
tm_shape(metro) +
	tm_bubbles("iso_a3")

tm_shape(rivers) +
	tm_lines(lwd = "type")
