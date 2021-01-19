\dontrun{
	
data(World)
m <- tm_shape(World) +
	tm_fill("well_being", id="name", title="Well-being")

grb = tmap_grob(m)

library(grid)

grid.newpage()
pushViewport(viewport(x = 0.1, y = 0.1, width = 0.2, height = 0.2))
grid.draw(grb)
upViewport()
pushViewport(viewport(x = 0.6, y = 0.6, width = 0.8, height = 0.8))
grid.draw(grb)
	
}
