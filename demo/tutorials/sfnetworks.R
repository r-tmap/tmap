library(sfnetworks)

sfn <- as_sfnetwork(roxel)

tm_shape(sfn) +
	tm_lines(col = "type", lwd = 2) +
	tm_dots(size = .3)
