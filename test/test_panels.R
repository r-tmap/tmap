data(World)

World$HPI3 <- cut(World$HPI, breaks = c(20, 35, 50, 65), labels = c("HPI low", "HPI medium", "HPI high"))
World$GDP3 <- cut(World$gdp_cap_est, breaks = c(0, 5000, 20000, Inf), labels = c("GDP low", "GDP medium", "GDP high"))

tm_shape(World) + 
	tm_fill("HPI3", palette="Dark2", colorNA="grey90", legend.show = FALSE) + 
	tm_facets(c("HPI3", "GDP3"))


qtm(World)

tm_shape(World) + 
	tm_fill("HPI3", palette="Dark2", colorNA="grey90") + 
	tm_facets(c("HPI3", "GDP3")) + tm_layout(legend.outside = TRUE)


tm_shape(World) + 
	tm_fill("HPI3", palette="Dark2", colorNA="grey90") + 
	tm_facets(c("HPI3", "GDP3")) + tm_layout(legend.outside = T, legend.outside.position = c("bottom")) + tm_style_cobalt() + tm_legend(bg.color="blue", frame=TRUE)



tm_shape(World) + 
	tm_fill("HPI3", palette="Dark2", colorNA="grey90") + 
	tm_facets(c("HPI3", "GDP3")) + tm_layout(legend.outside = T, legend.outside.position = c("bottom")) + tm_style_cobalt() + tm_legend(design.mode=T)


tm_shape(World) +
	tm_fill("pop_est") + tm_layout(outer.bg.color="yellow",  legend.frame=TRUE, legend.bg.color="red", legend.only=TRUE)


tm_shape(World) +
	tm_fill("gdp_cap_est") +
	tm_layout(legend.outside = T, legend.outside.position = "right", legend.bg.color="red", design.mode=F)



+
	tm_layout(outer.bg.color="red") 

qtm(World, style = "cobalt") + tm_layout(outer.bg.color="red")


WorldOne <- rgeos::gUnaryUnion(World)
tm_shape(World, projection="wintri") +
	tm_fill("HPI", palette="div", auto.palette.mapping = FALSE, n=7, 
			title = "Happy Planet Index") +
	tm_shape(WorldOne) + 
	tm_borders() +
	tm_grid(projection = "longlat") +
	tm_credits("Winkel Tripel projection", position = c("right", "BOTTOM")) +
	tm_style_natural(earth.boundary = c(-180,180,-87,87), inner.margins = .05, design.mode=T) +
	tm_legend(position=c("left", "bottom"), bg.color="grey95", frame=TRUE)