library(sf)
library(tmaptools)

data(World, rivers, metro)

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