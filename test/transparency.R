library(grid)

grid.newpage()

str(current.viewport())

grid.rect(gp=gpar(fill="lightgreen"))
grid.polyline(x=900*c(0.2, 0.7, 0.8, 0.5, 0.3, 0.2), y=500*c(0.1, 0.2, 0.9, 0.8, 0.2, 0.1), default.units = "native",
			 gp=gpar(col="#0000EE80", lwd=4))


grid.polygon(x=c(0.2, 0.7, 0.8, 0.5, 0.3, 0.2), y=c(0.1, 0.2, 0.9, 0.8, 0.2, 0.1), gp=gpar(fill="blue"))


# tm_borders <- function(col="grey40", lwd=1, lty="solid", alpha=1) {
# 	col <- do.call("rgb", c(as.list(col2rgb(col)), list(alpha=alpha*255, maxColorValue=255)))
# 	alpha <- NULL
# 	g <- list(tm_borders=as.list(environment()))
# 	class(g) <- "tmap"
# 	g
# }
