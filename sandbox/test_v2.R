
data(World)

tm_shape(World) +
	tm_fill("HPI")

tm_shape(World) +
	tm_fill(c("HPI", "economy"))


tm_shape(World) +
	tm_fill("HPI") +
	tm_facets("continent")
