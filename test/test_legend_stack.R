load_all("pkg")
data(World)
data(metro)

tm_shape(World) +
	tm_fill("gdp_cap_est") +
tm_shape(metro) +
	tm_bubbles(size="pop2010", col="pop2020") +
tm_layout(legend.width=1, legend.height = -.9, legend.frame=TRUE)


tm_shape(World) +
	tm_fill("gdp_cap_est") +
	tm_shape(metro) +
	tm_bubbles(size="pop2010", col="pop2020") +
	tm_layout(legend.width=-.5, legend.height = .9, legend.frame=TRUE, legend.stack="h", legend.position = c("right", "bottom"))
