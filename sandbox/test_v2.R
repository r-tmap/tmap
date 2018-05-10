data(World)

tm_shape(World) +
	tm_fill("HPI")

tm_shape(World) +
	tm_fill(c("HPI", "economy"))


tm_shape(World) +
	tm_fill("HPI") +
	tm_facets("continent")


data(rivers)

tm_shape(rivers) +
	tm_lines(col="scalerank", lwd = "strokelwd") +
	tm_facets(by = "type", free.scales.line.lwd = TRUE)

tm_shape(rivers) +
	tm_lines(col="scalerank", lwd = "strokelwd")
