data(World, metro)

map <- tm_shape(World) +
	tm_polygons("HPI", palette="RdYlGn", auto.palette.mapping=FALSE) +
	tm_shape(metro) +
	tm_bubbles("pop2020", col="steelblue", border.col="black", alpha=.75)

# print the map
map


# print in design mode, which shows how the plot is build
map + tm_layout(design.mode =TRUE) 

# green = yellow - outer.margins
# red = blue - inner.margins

# The legend width is by default 0.4, which is relative to the map frame (blue) width. However, this is the maximum legend width. The actual legend width also depends on the content of the legend. In this case, the legend width is decreased according to the widest legend item, which are the bubble sizes.

# The legend position and justification control the position of the legend
map + tm_layout(legend.position=c("right", "top"),
				design.mode =TRUE) 

map + tm_layout(legend.position=c(.5, .5),
				legend.just = c(.5, .5),
				design.mode =TRUE) 

map + tm_layout(legend.position=c(.5, .5),
				legend.just = c("left", "top"),
				design.mode =TRUE) 

# if the legend is drawn outside the (blue) map frame (enabled with legend.outsize=TRUE), an extra viewport is created. The width of the viewport is specified by the argument legend.outside.size. If the outside position, controlled with legend.outside.position) is top or bottom, then legend.outside.size corresponds to the relative height
map + tm_layout(legend.position=c("center", "center"),
				design.mode =TRUE,
				legend.outside = TRUE,
				legend.outside.size = .3) 

# the normal position argument legend.position (together with legend.just) controls the position of the legend within the extra viewport

# print this map in normal mode:
tmap_last() + tm_layout(design.mode=FALSE)


# map attributes are controlled in a similar way
map2 <- map + tm_credits("Some text\nAnother line") + tm_compass() + tm_logo(system.file("img/tmap.png", package="tmap"))

map2

map2 + tm_layout(attr.position=c("center", "center"))

# map attributes can also be plotted outside the mapping frame (but only on top or at the bottom):
map2 + tm_layout(attr.position=c("center", "center"), 
				 attr.outside = TRUE, 
				 attr.outside.position = "bottom")

# like the outside legend, a new viewport is created, from which the height is controlled by attr.outside.size:
map2 + tm_layout(design.mode=TRUE,
				 attr.position=c("center", "center"), 
				 attr.outside = TRUE, 
				 attr.outside.position = "bottom",
				 attr.outside.size = .4)

# like the legend, the normal position argument attr.position (together with attr.just) controls the position of the attributes within the extra viewport

map2 + tm_layout(design.mode=TRUE,
				 attr.position=c(.3, .8), 
				 attr.outside = TRUE, 
				 attr.just = c("left", "top"),
				 attr.outside.position = "bottom",
				 attr.outside.size = .4)

# When both legend and attributes are placed below the map, the extra viewports are stacked:
map2 + tm_layout(design.mode=TRUE,
				 legend.position=c("center", "center"),
				 legend.outside = TRUE,
				 legend.outside.position = "bottom",
				 legend.outside.size = .3,
				 attr.position=c(.3, .8), 
				 attr.outside = TRUE, 
				 attr.just = c("left", "top"),
				 attr.outside.position = "bottom",
				 attr.outside.size = .3)

# The map attributes do not fit well, because of the small space. The size of the attribute items can be controlled in the attribute layer functions.
