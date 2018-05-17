

test_seq_palettes <- function(k=5, pal="Blues") {
	require(grid)
	grid.newpage()
	data(NLD_muni)
	pushViewport(viewport(layout = grid.layout(1,2)))
	set.seed(1234)
	print(tm_shape(NLD_muni) + tm_fill("population", convert2density = T, palette=pal, style="kmeans", n=k) + tm_layout("Own interpolation"), vp=viewport(layout.pos.row=1, layout.pos.col = 1))
	set.seed(1234)
	print(tm_shape(NLD_muni) + tm_fill("population", convert2density = T, palette=RColorBrewer::brewer.pal(k, pal), style="kmeans", n=k, auto.palette.mapping = F) + tm_layout("Original brewer"), vp=viewport(layout.pos.row=1, layout.pos.col = 2))
	popViewport(1)
}

test_div_palettes <- function(k=5, pal="RdYlGn") {
	require(grid)
	grid.newpage()
	data(World)
	World$HPI <- ((World$HPI - min(World$HPI, na.rm = T)) / (max(World$HPI, na.rm = T) - min(World$HPI, na.rm = T))) - .5 
	pushViewport(viewport(layout = grid.layout(2,1)))
	set.seed(1234)
	print(tm_shape(World) + tm_fill("HPI", palette=pal, style="equal", n=k) + tm_layout("Own interpolation"), vp=viewport(layout.pos.row=1, layout.pos.col = 1))
	set.seed(1234)
	print(tm_shape(World) + tm_fill("HPI", palette=RColorBrewer::brewer.pal(k, pal), style="equal", n=k, auto.palette.mapping = F) + tm_layout("Original brewer"), vp=viewport(layout.pos.row=2, layout.pos.col = 1))
	popViewport(1)
}


test_seq_palettes(9)
test_seq_palettes(5, "YlOrBr")


test_div_palettes(3)

