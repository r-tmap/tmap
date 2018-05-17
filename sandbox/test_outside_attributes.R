data(Europe)

qtm(Europe, fill="income_grp") + tm_layout(legend.outside = TRUE) + tm_scale_bar(position = c("center", "top")) + tm_compass() + tm_grid(labels.inside.frame = F) + tm_layout(design.mode=T, legend.outside.position = "bottom")


qtm(Europe, fill="income_grp") + tm_layout(legend.outside = TRUE) + tm_scale_bar() + tm_compass() + tm_grid(labels.inside.frame = F) + tm_layout(design.mode=T, legend.outside.position = "bottom", attr.outside = T)


qtm(Europe, fill="income_grp") + tm_layout(legend.outside = TRUE) + tm_scale_bar() + tm_compass() + tm_grid(labels.inside.frame = F) + tm_layout(design.mode=T, attr.outside.position = "bottom", attr.outside = T)


qtm(Europe, fill="income_grp") + tm_layout(legend.outside = TRUE) + 
	tm_scale_bar() + 
	tm_compass() + 
	#tm_credits("test124dttsd") +
tm_layout(design.mode=T, attr.outside.position = "bottom", legend.outside.position = "left", attr.outside = T, attr.just = c("right", "center"))




qtm(Europe, fill="income_grp") + 
	tm_scale_bar() + 
	tm_compass() + 
	tm_credits("test124dttsd") +
tm_layout(design.mode = T, attr.position = c("left", "bottom"))
	

qtm(Europe, fill="income_grp") + 
	tm_scale_bar() + 
	tm_compass() + 
	tm_credits("test124dttsd\ntftgfd", align = "left") +
tm_layout(design.mode = T, attr.position = c("center", "bottom"))


qtm(Europe, fill="income_grp") + 
	tm_scale_bar() + 
	tm_compass() + 
	tm_credits("test124dttsd\ntftgfd") +
tm_layout(design.mode = T, attr.position = c("right", "bottom"))


data(NLD_muni)

tm_shape(NLD_muni) +
	tm_polygons("population", convert2density=TRUE) +
	tm_format_NLD(frame = TRUE) +
tm_credits("teste1235") + 
tm_compass() +
	tm_scale_bar() + 
	tm_layout(attr.position = c("right", "bottom"),
			  #attr.just=c("center","center"),
		  attr.outside = T, 
		  attr.outside.position = "top", 
		  design.mode = T)


## test xlab and ylab

data(World)
tm_shape(World, projection="longlat") + 
	tm_grid(projection="longlat",n.x = 20, n.y = 20, labels.size = .5) +
	tm_polygons(col="#FFF8DC",alpha = .75,border.alpha = .25,border.col = "black") 