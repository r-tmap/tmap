library(gridSVG)
library(grid)

data(NLD_prov)

tm_shape(NLD_prov) +
	tm_fill("population", convert2density = TRUE)

grid.force()


grid.ls(fullNames = TRUE)

# http://sachsmc.github.io/UseR2015-Talk/#1

grid.garnish("tm_polygons", grep = TRUE, global = TRUE, group = FALSE, onmouseover="highlight(evt)", onmouseout="unhighlight(evt)")


grid.export("../test/test.svg")
