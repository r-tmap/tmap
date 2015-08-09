
data(World)


tm_shape(World) +
	tm_polygons("pop_est") +
tm_layout(legend.position=c("left", "top"),elem.position = c("right", "bottom")) +
	tm_scale_bar() +
	tm_credits("ggggggdfgsdfgtesrtggfgdsfdgfdgsdfgfsdgdfgdfsgdffdgsfgsdhtfghg\nsdftggfdsgdfghhdfdgsdfgddgdferewrewrewre234", bg.color = "yellow") +
	tm_compass()


tm_shape(World) +
	tm_polygons("steelblue") +
	tm_layout(legend.position=c("left", "top"),elem.position = c("left", "top")) +
	tm_scale_bar() +
	tm_credits("test1234") +
	tm_compass(north = 40)
