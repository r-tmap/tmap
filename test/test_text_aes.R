data(Europe)

tm_shape(Europe) +
	tm_fill() +
	tm_text("iso_a3", size="pop_est")


tm_shape(Europe) + tm_text("iso_a3", size = 1, color="economy", scale=1)

tm_shape(Europe) + tm_text("iso_a3", size = c("pop_est", "gdp_cap_est"), scale=2)

tm_shape(Europe) + tm_text("iso_a3", size = "pop_est", scale=2)



tm_shape(Europe) + tm_fill(c("economy", "income_grp"), palette=c("black", "white", "blue")) + tm_text("iso_a3", size = c("pop_est", "gdp_cap_est"), scale=2)


tm_shape(Europe) + tm_text("iso_a3", size = c("pop_est", "gdp_cap_est"), scale=2, legend.size.show=FALSE) + tm_facets(free.scales.text.size = FALSE)

tm_shape(Europe) + tm_bubbles(size = c("pop_est", "gdp_cap_est"), scale=2, legend.size.show=FALSE) + tm_facets(by="part", free.scales.bubble.size = TRUE)


tm_shape(Europe) + tm_text("iso_a3", size = 1, color="economy", scale=1, palette="Dark2")

tm_shape(Europe) + tm_text("iso_a3", size = "gdp_cap_est", color="economy", scale=2, palette="Dark2", legend.size.is.portrait = TRUE)
