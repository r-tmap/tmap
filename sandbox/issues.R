library(tmap)

data(World)

# charts
tm_shape(World) + tm_polygons("HPI", fill.chart = tm_chart_histogram(position = c("left", "bottom")))


###########################################################
# https://github.com/r-tmap/tmap/issues/806
#
# credits view mode
###########################################################

library(tmap) # tmap v3.3.4
data(World, package = "tmap")
tmap_mode("view")
my_map <- tm_shape(World) +
	tm_polygons(col = "area") +
	tm_basemap("OpenStreetMap") + tm_layout("title of my map") 

my_map

my_map + tm_credits("Last update in October 2023")

my_map |> 
	tmap_leaflet() |> 
	leaflet::addTiles(attributions = "Last update in October 2023")

###########################################################
# scalebar 
# https://github.com/r-tmap/tmap/issues/809
# changed width back to number of lines
###########################################################

tm_shape(World) +
	 	tm_fill() +
	 	tm_scalebar(position = tm_pos_out())


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
	tm_scalebar(width = 10,
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



library(sf)
#> Linking to GEOS 3.12.1, GDAL 3.7.3, PROJ 9.2.1; sf_use_s2() is TRUE
f = system.file("shapes/world.gpkg", package = "spData")
world = read_sf(f)
tanzania = read_sf(f, query = 'SELECT * FROM world WHERE name_long = "Tanzania"')

tmap_design_mode()
tm_shape(tanzania) +
	tm_polygons(lwd = 2) +
	tm_scalebar(c(0, 200, 400), position = tm_pos_out("left", "bottom"))

tm_shape(tanzania) +
	tm_polygons(lwd = 2) +
	tm_scalebar(c(0, 200, 400), position = tm_pos_out("center", "bottom"))

tm_shape(tanzania) +
	tm_polygons(lwd = 2) +
	tm_title("c(0, 200, 400)", position = tm_pos_out("center", "bottom"))


###########################################################
# https://github.com/r-tmap/tmap/issues/811
###########################################################

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

# not working
tm_shape(r) +
	tm_rgb()


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

tm_shape(lc_cameroon) +
	tm_raster("land_cover")


tm_shape(lc_cameroon) +
	tm_raster("land_cover", col.legend = tm_legend())


tm_shape(lc) +
	tm_raster()


## issue Rapsodia86

library(terra)
rast_in <- rast("sandbox/Raster_test_tmap.tif")
rast_in[rast_in==0] <- NA
plot(rast_in)
tm_shape(rast_in) +tm_raster()

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

reprex::reprex({
library(stars)
library(mapview)
m = matrix(1:20, 4)
s0 = st_as_stars(m)
s = s0
st_crs(s) <- 4326
st_crs(s0) <- 4326
st_geotransform(s0) <- c(5, 1.5, 0.2, 0, 0.2, 1.5)



s0_4326 = st_transform(s0, crs = 4326)

stars:::is_curvilinear(s0)
stars:::is_curvilinear(s0_4326)

s0
s0_4326
})




d = st_dimensions(s0)
has_raster(s0) && isTRUE(attr(s0, "raster")$curvilinear)





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
tm_shape(s0, crs = 3857)  + tm_raster()

tm_shape(s)  + tm_raster()
#tm_shape(s0)  + tm_raster()

mapview::mapview(s)
plot(s)
plot(s)


## 820
library(sf)
#> Linking to GEOS 3.11.2, GDAL 3.7.2, PROJ 9.3.0; sf_use_s2() is TRUE
system.file("gpkg/nc.gpkg", package = "sf") |>
	read_sf() |>
	st_transform('EPSG:32119') -> nc.32119
tm_shape(nc.32119) + tm_polygons(c("SID74", "SID79"), title="SIDS") + 
	tm_layout(legend.outside=TRUE, panel.labels=c("1974-78", "1979-84")) + 
	tm_facets(free.scales=FALSE)


tm_shape(nc.32119) + tm_polygons(c("SID74", "SID79"), fill.free = FALSE, fill.legend = tm_legend(title = "SIDS")) + 
	tm_layout(legend.outside=TRUE, panel.labels=c("1974-78", "1979-84")) + 
	tm_facets()


## 

library(terra)
library(tmap)
lc = rast("sandbox/land_cover.tif") # https://osf.io/download/m92d7")

?terra

levels(lc)

unique(landr)

tm_shape(lc) + tm_raster()



class(landr)
str(landr)
terra::lapp(landr, class)

levels(landr)

terra::levels(land)


lc2 = terra::spatSample(lc, 1e5, method = "regular", as.raster = T)

tm_shape(lc2) + tm_raster("land_cover") + tm_facets("land_cover")
tm_shape(lc) + tm_raster("land_cover") + tm_facets("land_cover")

tm_shape(land) + tm_raster("trees") + tm_facets("cover_cls")
tm_shape(land) + tm_raster("cover_cls") + tm_facets("cover_cls")


# to do: 




unique(lc)

x = terra::head(lc, 10)

x = terra::unique(lc)

o = list(drop.NA.facets = TRUE, facet.max = 9)

a = bench::mark(
	a1 <- get_fact_levels_na(terra::values(lc, dataframe=TRUE)[[1]], o),
	a2 <- freq(lc),
	a3 <- unique(lc), check = FALSE
)

system.time({plot(lc)})

summaryRprof()
Rprof(tmp <- tempfile())
print(tm_shape(lc) +
	  	tm_raster())
Rprof()
summaryRprof(tmp)
unlink(tmp)

World$well_being_rounded = round(World$well_being)
tm_shape(World) + tm_polygons("HPI") + tm_facets(by = "well_being_rounded")

## maptiles leaflet inconsistancy (will be solved in maptiles)

reprex::reprex({
	library(maptiles)
	library(leaflet)
	a = names(maptiles:::maptiles_providers)
	b = names(leaflet::providers)
	
	a
	b
	setdiff(a, b)
})


ttm()
tm_basemap(c("Esri.WorldShadedRelief", "Stadia.StamenTerrain")) +
	tm_tiles("Stadia.StamenTerrainLines") +
	tm_tiles("Stadia.StamenTerrainLabels")

## bgcol

tm_shape(World) +
	tm_text("name", bgcol = "green")

tm_shape(World) +
	tm_text("name", bgcol = "economy")

tm_shape(World) +
	tm_text(text = "name", 
			size = .4, 
			bgcol = "economy", 
			bgcol.scale = tm_scale_categorical(values = cols4all::.P$hcl$cat$set2),
			bgcol_alpha = "pop_est", 
			bgcol_alpha.scale = tm_scale_intervals(style = "kmeans"))


###########################################################
#
# https://github.com/r-tmap/tmap/issues/832 (solved)
#
###########################################################
library(terra)

tm_shape(vect(cbind(0:1, 0:1), type = "l")) + 
	tm_lines() + 
	tm_shape(rast(matrix(1))) + 
	tm_raster(col_alpha = 0.2)

tm_shape(vect(cbind(c(0, 1852 * 60), c(0, 1852 * 60)), type = "l", crs = "+proj=laea")) + 
	tm_lines() + 
	tm_shape(rast(matrix(1), crs = "EPSG:4326")) +
	tm_raster(col_alpha = 1)


###########################################################
#
# https://github.com/r-tmap/tmap/issues/827
#
###########################################################


library(sf)
library(tmap)
library(spData)

packageVersion("tmap")
#> [1] '3.99.9000'

tm_shape(nz)+
	tm_fill(fill = "Island", 
			fill.scale = tm_scale_categorical(values=c("red", "#FFEBBE")),
			fill.legend = tm_legend(position = tm_pos_out("right", "top")))+
	tm_title("a)")+
	tm_layout(legend.position = tm_pos_out("left", "top"), 
			  title.position = tm_pos_out("center", "top"))

# Left/Top and Center/Top overlap
# Legend/Title are inside rather than outside even though tm_pos_out is used
tm_shape(nz)+
	tm_fill(fill = "Island", fill.scale = tm_scale_categorical(values=c("red", "#FFEBBE")))+
	tm_title("a)")#+
	#tm_layout(legend.position = tm_pos_out("left", "top"), 
	#		  title.position = tm_pos_out("center", "top"))

tm_shape(nz)+
	tm_fill(fill = "Island", fill.scale = tm_scale_categorical(values=c("red", "#FFEBBE")),
			fill.legend = tm_legend(position = tm_pos_out("left", "center")))+
	tm_title("a)", position = tm_pos_out("center", "top"))

tm_shape(nz)+
	tm_fill(fill = "Island", fill.scale = tm_scale_categorical(values=c("red", "#FFEBBE")),
			fill.legend = tm_legend(position = tm_pos_out("left", "top")))

tm_shape(World) +
	tm_polygons("HPI") +
	tm_place_legends_right()

tm_shape(World) +
	tm_polygons("HPI") +
	tm_place_legends_inside()

tm_shape(World) +
	tm_polygons("HPI", fill.legend = tm_legend(position = tm_pos_out("left", "top")))


### 822

library(motif)
landcover = rast(system.file("raster/landcover2015.tif", package = "motif"))

cats(landcover)
terra::coltab(landcover)

paleta = c("darkgreen", "orange", "yellow", "magenta", "red", "grey", "blue", "#0096A0",  "#00CF75")
tm_shape(landcover) +
	tm_raster(style = "cat",
			  palette = paleta)

tm_shape(landcover) +
	tm_raster("category")

set.seed(0)
r <- rast(nrows=10, ncols=10)
values(r) <- sample(3, ncell(r), replace=TRUE)
is.factor(r)

cls <- data.frame(id=1:3, cover=c("forest", "water", "urban"))
levels(r) <- cls
is.factor(r)





###### component size

# set device asp to ~3

#process_meta L406

tm_shape(World) +
	tm_polygons("HPI")

# why is the fW larger?:
tm_shape(World) +
	tm_polygons("HPI") +
	tm_compass(size = .1,position = tm_pos_out("right", "center"))

# should be rescaled:
tm_shape(World) +
	tm_polygons("HPI") +
	tm_compass(size = 16,position = tm_pos_out("right", "center"))


## 835
tm_shape(rivers)+
	tm_lines(col = "scalerank", lwd = 5, lineend = "butt")
