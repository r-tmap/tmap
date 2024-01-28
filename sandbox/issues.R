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

tm_shape(r) +
	tm_raster(col.legend = tm_legend_hide())


str(r)


cat_raster = rast(system.file("raster/nlcd.tif", package = "spDataLarge"))

tm_shape(cat_raster) +
	tm_raster(col.scale = tm_scale_categorical(levels.drop = TRUE),
			  col.legend = tm_legend("Land cover"))


# issue 2
library(osfr)
library(rnaturalearth)
#> Support for Spatial objects (`sp`) will be deprecated in {rnaturalearth} and will be removed in a future release of the package. Please use `sf` objects with {rnaturalearth}. For example: `ne_download(returnclass = 'sf')`

dir.create("testdata")
osf_retrieve_node("xykzv") |>
	osf_ls_files(n_max = Inf) |>
	osf_download(path = "testdata",
				 conflicts = "overwrite")


lc = rast("testdata/land_cover.tif")
cameroon = ne_countries(country = "Cameroon", returnclass = "sf") |>
	st_transform(crs = st_crs(lc))

lc_cameroon = crop(lc, cameroon, mask = TRUE)

lc_palette_df = read.csv("testdata/lc_palette.csv")
coltb = lc_palette_df[c("value", "color")]
coltab(lc_cameroon) = coltb
plot(lc_cameroon)  # raster with color table

tm_shape(lc_cameroon) +
	tm_raster()

tm_shape(lc) +
	tm_raster()


# 819
L7file = system.file("tif/L7_ETMs.tif", package = "stars")
L7 = read_stars(L7file)

### working but gives warning
tm_shape(L7) + 
	tm_rgb()
	
	tm_raster(c(3, 2, 1))

tm_shape(L7) + 
	tm_raster(col = tm_mv_dim("band", c(3, 2, 1)), col.scale = tm_scale_rgb())

tm_shape(L7) + 
	tm_rgb(tm_mv_dim("band", c(3, 2, 1)))

L7split = split(L7)
tm_shape(L7split) + 
	tm_rgb(tm_mv("X3", "X2", "X1"))



# Warning message:
# In value[[3L]](cond) : could not rename the data.table

### not working gives error
tm_shape(L7) + 
	tm_rgb(3, 2, 1)

# Error: palette should be a character value
# In addition: Warning message:
# In value[[3L]](cond) : could not rename the data.table













# in #733
tm_shape(World) +
	tm_polygons(c("income_grp", "economy"), title = c("Legend Title 1", "Legend Title 2"))


tm_shape(World) +
	tm_polygons(c("income_grp", "economy"), fill.legend = tm_legend(title = c("Legend Title 1", "Legend Title 2")))

tm_shape(World) +
	tm_polygons(c("income_grp", "economy"), fill.legend = list(tm_legend(title = "Legend Title 1"), 
															   tm_legend(title = "Legend Title 2")))

tm_shape(World) +
	tm_polygons(c("income_grp", "economy"), fill.legend = list(tm_legend(title = "Legend Title 1"), 
															   tm_legend(title = "Legend Title 2")))+
tm_layout(main.title = "Main Title",
		  main.title.position = "center",
		  main.title.color = "blue",
		  title = c("Title 1", "Title 2"),
		  title.color = "red",
		  panel.labels = c("Panel Label 1", "Panel Label 2"),
		  panel.label.color = "purple",
		  legend.text.color = "brown")


tm_shape(World) +
	tm_polygons(c("income_grp", "economy"), fill.legend = list(tm_legend(title = "Legend Title 1"), 
															   tm_legend(title = "Legend Title 2")))+
	tm_title("Main", position = tm_pos_out("center", "top"))




#810
tm_shape(World, bbox = as.vector(sf::st_bbox(World))) + 
	tm_polygons()


reprex::reprex({
	sf::st_bbox(c(-180, -89, 180, 83))
	sf::st_bbox(c(xmin = -180, ymin = -89, xmax = 180, ymax = 83))
})



# 818

library(stars)
library(mapview)
m = matrix(1:20, 4)
s0 = st_as_stars(m)
s = s0
st_crs(s) <- 4326
st_crs(s0) <- 4326

st_geotransform(s0) <- c(5, 1.5, 0.2, 0, 0.2, 1.5)

library(tmap)
tmap_mode("plot")
tmap_mode("view")
tm_shape(World) + 
	tm_borders() +
tm_shape(s) +
	tm_raster(col_alpha=0.5) +
	tm_shape(s0,raster.warp = FALSE) + tm_raster()

tm_shape(s, crs = 3857)  + tm_raster()
tm_shape(s0, crs = 3857)  + tm_raster()

tm_shape(s0)  + tm_raster()

tm_shape(World) + tm_polygons()


s
(s2 = stars::st_warp(s, crs = 3857))

tm_shape(s) + tm_raster("A1", col.scale = tm_scale_discrete())
tm_shape(s2) + tm_raster("A1", col.scale = tm_scale_discrete())
tm_shape(s, crs = 3857) + tm_raster("A1", col.scale = tm_scale_discrete())

mapview(s)
mapview(s2)
s$A1[]
s2$A1[]


st_crs(s)
s$A1

s2 = transwarp(s, crs = 3857)

plot(s)
plot(s2)

s$A1[]
s2$A1[]

c("#FF0000", "#FF4D00", "#FF9900", "#FFE500", 
  "#CCFF00", "#80FF00", "#33FF00", "#00FF19", 
  "#00FF66", "#00FFB2", "#00FFFF", "#00B3FF",
  "#0066FF", "#001AFF", "#3300FF", "#7F00FF",
  "#CC00FF", "#FF00E6",  "#FF0099", "#FF004D")


######

# original matrix is m:
m
s$A1

# adding rainbow colors:
leaflet() |> addTiles() |> leafem::addStarsImage(s, colors = rainbow(20))




mtch = match(1:20, s2$A1)

leaflet() |> addTiles() |> leafem::addStarsImage(s2, colors = rainbow(20))


leaflet() |> addTiles() |> leafem::addStarsImage(s2, colors = rainbow(20)[mtch])
mapview(s2)

# tmapShape L 23
tmapReproject.dimensions




#######################

library(stars)
library(mapview)
#m = matrix(1:4, 2)
m = matrix(c(3,4,1,2), 2)

s0 = st_as_stars(m)
s = s0
st_crs(s) <- 4326
st_crs(s0) <- 4326

st_geotransform(s0) <- c(5, 1.5, 0.2, 0, 0.2, 1.5)


tm_shape(s, crs = 3857)  + tm_raster()
#tm_shape(s0, crs = 3857)  + tm_raster()

tm_shape(s)  + tm_raster()
#tm_shape(s0)  + tm_raster()

mapview::mapview(s)
plot(s)
plot(s)
