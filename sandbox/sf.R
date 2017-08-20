library(sf)
library(sp)
library(raster)
library(mapview)
library(grid)

devtools::load_all("../tmaptools")
devtools::load_all(".")


data(World, rivers, metro, land)

W <- as(World, "sf")
r <- as(rivers, "sf")
m <- as(metro, "sf")
landr <- as(land, "RasterBrick")

tm_shape(World) +
	tm_polygons("HPI")


data(Europe)
tm_shape(Europe) + tm_polygons("income_grp")

tm_shape(metro) + tm_bubbles()

tm_shape(rivers) + tm_lines("purple")

x <- st_as_grob(W$geometry[[136]])

grid.newpage()
grid.draw(x)

pushViewport(viewport(xscale = st_bbox(W)[c(1,3)], yscale = st_bbox(W)[c(2,4)]))


plot(W[5,])

attr(W, "bbox") <- st_bbox(W)
x <- grid.shape(W, i = 1, k=1, gp=gpar(fill="red"))

grid.newpage()
grid.draw(x)



grb <- sf::st_as_grob(W)



World@proj4string <- sp::CRS()


World <- as(World, "sf")
rivers <- as(rivers, "sf")
metro <- as(metro, "sf")

qtm(World)

sf::st_proj_info(World)

W2 <- st_transform(World, crs=get_proj4("robin"))

W2$geometry

st_crs(World)


st_crs(W2)



# --- preprocess_shapes
# set_projection (set only, and transform)
# approx_areas
# is_projected


# --- process_shapes
# split
# crop



