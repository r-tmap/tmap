corop <- get_shape("../test/NDW_example/cr_2013.shp")
corop <- set_projection(corop, current.projection="rd")

gm <- get_shape("../shapes/gm_2012.shp")
gm <- set_projection(gm, current.projection="rd")

rwb_cr <- get_shape("../test/NDW_example/rw2013_doorgaand_cr.shp")
loops <- get_shape("../test/NDW_example/loops2013_classified.shp")

library(sp)
library(rgeos)


######## test split lines and map points to line

dist <- 100

rwb_cr2 <- split_lines(rwb_cr, dist=dist)

loops_data <- map_points_to_line(shp.points=loops, shp.lines=rwb_cr2, key.points="roadname", key.lines="ID", by.key=TRUE)

loops_data$dist <- (loops_data$id3-1) * dist

loops <- append_data(loops, loops_data, fixed.order=TRUE)

loops79 <- loops[loops$roadname=="A79",]
loops79$order <- order(order(loops79$dist))

pdf("test.pdf", width=7, height=7)
geo_shape(corop[39,]) +
	geo_fill("grey90") +
	geo_borders("white") +
	geo_shape(rwb_cr) +
	geo_lines(col="red", lwd=2) +
	geo_shape(rwb_cr2) +
	geo_lines(col="blue") +
	geo_shape(loops79) +
	geo_bubbles(size=.2) +
	geo_text("order", cex=.3)
dev.off()




######## test double_line

y <- matrix(c(0,0,-2,-1,0,0,
			  0,2,2,4,6,8), ncol=2, byrow=FALSE)

shp <- rwb_cr
width <- 1000

rwb_cr_list <- double_line(rwb_cr, width=width)

rwb_cr2 <- rwb_cr_list[[1]]
rwb_cr3 <- rwb_cr_list[[2]]

rwb_cr2b <- split_lines(rwb_cr2, dist=100, include.last=FALSE)
rwb_cr2c <- fit_polylines(rwb_cr2b, id="ID",na.rm=FALSE)

rwb_cr3b <- split_lines(rwb_cr3, dist=100, include.last=FALSE)
rwb_cr3c <- fit_polylines(rwb_cr3b, id="ID",na.rm=FALSE)


pdf("test.pdf", width=7, height=7)
geo_shape(corop) +
	geo_fill("grey90") +
	geo_borders("white") +
	geo_shape(rwb_cr) +
	geo_lines(col="red", lwd=1) +
	geo_shape(rwb_cr2c) +
	geo_lines(col="blue") 
	geo_shape(rwb_cr3c) +
	geo_lines(col="purple")
dev.off()
