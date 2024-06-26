# tme_graphics = function(init, polygons, symbols, lines, raster) {
# 	structure(list(init = init, polygons = polygons, symbols = symbols, lines = lines, raster = raster), class = "tme_graphics")
# }
# 
# 
# 
# tme_graphics.grid = tme_graphics(
# 	require = "grid",
# 	init = function(bbx) {
# 		if (!requireNamespace("grid")) stop("grid package required but not installed yet.")
# 		grid.newpage()
# 		grid::pushViewport(viewport(xscale = st_bbox(World)[c(1,3)], yscale = st_bbox(World)[c(2,4)]))
# 	},
# 	polygons = function(x, col, alpha, border.col, border.lwd, border.lty) {
# 		geoms = sf::st_geometry(x)
# 		gp = gpar(fill=col, col=border.col, lwd=border.lwd, lty=border.lty)
# 		sf::st_as_grob(geoms, gp = gp)
# 	},
# 	symbols = function(x, size, col, shape, alpha, border.col, border.lwd, border.lty) {
# 		geoms = sf::st_centroid(sf::st_geometry(x))
# 		co = sf::st_coordinates(geoms)
# 		gp = gpar(fill=col, col=border.col, lwd=border.lwd, lty=border.lty)
# 		
# 		pointsGrob(x=co[,1], y = co[,2],
# 				   size = size,
# 				   pch = shape,
# 				   gp = get_symbol_gpar(x=symbol.shape2,
# 				   				   fill=symbol.col2,
# 				   				   col=bordercol,
# 				   				   lwd=symbol.border.lwd), 
# 				   name=idName)
# 	}
# 	
# 	
# )
# 
# 
# tme_grid = function(init, )
# 
# 
# 
# 
# t_polygon = function(x, col, alpha, border.col, border.lwd, border.lty) {
# 	
# 	geoms = sf::st_geometry(x)
# 	
# 	gp=gpar(fill=col, col=border.col, lwd=border.lwd, lty=border.lty)
# 	
# 	st_as_grob(geoms, gp = gp)
# }
# 
