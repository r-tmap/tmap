data(World)
data(rivers)
data(metro)
data(land)
metro$growth <- (metro$X2020 - metro$X2010) / (metro$X2010 * 10) * 100

## one double legend item
tm_shape(World) +
	tm_fill("income_grp") +
	tm_bubbles(size="pop_est") +
tm_shape(rivers) +
	tm_lines(col = "type", lwd="scalerank", scale=3) +
tm_shape(metro) +
	tm_bubbles(size = "X2010", col="growth") +
tm_shape(land) +
	tm_raster("trees", alpha=.25)


tm_shape(World) +
	tm_fill(c("income_grp", "economy"), title = c("Income", "Economy")) +
	tm_bubbles(size="pop_est") +
	tm_shape(rivers) +
	tm_lines(col = "type", lwd="scalerank", scale=3) +
	tm_shape(metro) +
	tm_bubbles(size = "X2010", col="growth") +
	tm_shape(land) +
	tm_raster("trees", alpha=.25) +
	tm_layout(title="")

tm_shape(World) +
	tm_fill("income_grp")