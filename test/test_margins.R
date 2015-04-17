data(World)

tm_shape(World) +
	tm_fill("pop_est", convert2density = TRUE) +
	tm_borders() +
tm_layout(inner.margins = c(0,0,0,0))

tm_shape(World) +
	tm_fill("pop_est", convert2density = TRUE) +
	tm_borders() +
	tm_layout(inner.margins = c(1,1,1,1))


tm_shape(World) +
	tm_fill("pop_est", convert2density = TRUE) +
	tm_borders() +
	tm_layout(inner.margins = c(.5,.5,.5,.5))


tm_shape(World) +
	tm_fill("pop_est", convert2density = TRUE) +
	tm_borders() +
	tm_layout(inner.margins = c(0,1,0,1))

tm_shape(World) +
	tm_fill("pop_est", convert2density = TRUE) +
	tm_borders() +
	tm_layout(inner.margins = 0, outer.margins=c(.4,.4,.4,.4))
