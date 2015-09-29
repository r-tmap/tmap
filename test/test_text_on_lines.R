data(Europe)
data(NLD_muni)
data(rivers)


tm_shape(rivers[484,]) +
	tm_lines() +
	tm_text("name")

rivers <- set_projection(rivers, projection = "eck4")
riversMids <- SpatialLinesMidPoints(rivers)

tm_shape(NLD_muni) + tm_fill() +
tm_shape(rivers) +
	tm_lines() +
tm_shape(riversMids) +
	tm_text("name")


tm_shape(NLD_muni) + tm_fill() +
	tm_shape(rivers) +
	tm_lines() +
	tm_text("name")



co <- riversMids@coords

res <- pointLabel(co, labels=riversMids$name)


n <- 500
x <- rnorm(n)*10
y <- rnorm(n)*10
plot(x, y, col = "red", pch = 20)
pointLabel(x, y, as.character(round(x,5)), offset = 0, cex = .7)


library(ggplot2)
library(directlabels)
scatter <- qplot(jitter(hwy),jitter(cty),data=mpg,colour=class,
				 main="Fuel efficiency depends on car size")
print(direct.label(scatter))
