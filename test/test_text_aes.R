data(Europe)

tm_shape(Europe) +
	tm_fill() +
	tm_text("iso_a3", size="pop_est")


tm_shape(Europe) + tm_text("iso_a3", size = 1, color="economy", scale=1)

tm_shape(Europe) + tm_text("iso_a3", size = c("pop_est", "gdp_cap_est"), scale=2)
tm_shape(Europe) + tm_text("iso_a3", size = c("pop_est", "gdp_cap_est"), scale=2, legend.size.show=FALSE)
