library(tmap)

data(World)

# charts
tm_shape(World) + tm_polygons("HPI", fill.chart = tm_chart_histogram(position = c("left", "bottom")))


# issue?
lines <- st_read("example.json")

tm_shape(lines) +
	tm_lines(
		col = "Mode",
		col.scale = tm_scale(
			values = "-Set1"),
		lty = "Mode",
		lty.scale = tm_scale(
			values = c("dotted", "solid")),
		lty.legend = tm_legend_combine("col"),
		lwd = 2) +
	tm_add_legend(
		type = "lines",
		labels = c("Boat", "Road"),
		col.values = "-Set1",
		lty.values = c("dotted", "solid"),
		title = "Wrong way")

# bgcol(alpha)

tm_shape(World) +
	tm_text("name", bgcol = "HPI")

tm_shape(World) + tm_polygons() +
	tm_text("name", bgcol = "economy", bgcol_alpha = 1, shadow = TRUE)


# credits view mode
library(tmap) # tmap v3.3.4
data(World, package = "tmap")
tmap_mode("view")
my_map <- tm_shape(World) +
	tm_polygons(col = "area") +
	tm_basemap("OpenStreetMap") + tm_layout("title of my map") 

my_map

my_map |> 
	tmap_leaflet() |> 
	leaflet::addTiles(attributions = "Last update in October 2023")


# pos_out
tm_shape(World) +
	tm_fill() +
	tm_scalebar(
		position = tm_pos_out())


tm_shape(World) +
	tm_fill() +
	tm_compass(
		position = tm_pos_out())


tm_shape(World) +
	tm_fill("HPI", fill.legend=tm_legend(
		position = tm_pos_out()))




# scale bar outside

tm_shape(NLD_prov) +
	tm_fill() +
	tm_grid() +
	tm_scalebar(
		position = tm_pos_in())

tm_shape(NLD_prov) +
	tm_fill() +
	tm_grid() +
	tm_scalebar(breaks = c(0,10,20,50),
				position = tm_pos_in())

tm_shape(NLD_prov) +
	tm_fill() +
	tm_grid(labels.inside.frame = F) +
	tm_layout(meta.margins = c(0, 0, 0, .6)) +
	tm_scalebar(width = 1,
		position = tm_pos_out())

tm_shape(NLD_prov) +
	tm_fill() +
	tm_grid(labels.inside.frame = F) +
	tm_scalebar(breaks = c(0,10,20,100), width = 1,
		position = tm_pos_in())

tm_shape(NLD_prov) +
	tm_fill() +
	tm_grid(labels.inside.frame = T) +
	tm_scalebar(breaks = c(0,50,100,250),
				position = tm_pos_in())

# gridlines: tmapGridAux 675
# scalebar: tmapGridComp 487

tm_shape(NLD_prov) +
	tm_fill() +
	tm_grid(labels.inside.frame = F) +
	tm_scalebar(position = tm_pos_out(), width = 1)
tm_shape(NLD_prov) +
	tm_fill() +
	tm_grid(labels.inside.frame = F) +
	tm_scalebar(position = tm_pos_in())

tm_shape(World) +
	tm_fill() +
	tm_scalebar(
		position = tm_pos_out(
			cell.h = "center", cell.v = "bottom",
			pos.h = "right", pos.v = "top"))



library(sf)
f = system.file("shapes/world.gpkg", package = "spData")
world = read_sf(f)
tanzania = read_sf(f, query = 'SELECT * FROM world WHERE name_long = "Tanzania"')

# works
tm_shape(tanzania) +
	tm_polygons(lwd = 2) +
	tm_scalebar(position = c("left", "bottom"))

tm_shape(tanzania) +
	tm_polygons(lwd = 2) +
	tm_scalebar(c(0, 200, 400), position = c("left", "bottom"))

# 811

library(terra)

library(sf)

?coltab
r = rast(ncols=3, nrows=3, vals=1:9)
plot(r) # original raster

coltb = data.frame(value=1:9, col=rainbow(9, end=.9))

coltab(r)  =  coltb
plot(r) 

tm_shape(r) +
	tm_raster()

str(r)


cat_raster = rast(system.file("raster/nlcd.tif", package = "spDataLarge"))

tm_shape(cat_raster) +
	tm_raster(col.scale = tm_scale_categorical(levels.drop = TRUE),
			  col.legend = tm_legend("Land cover"))