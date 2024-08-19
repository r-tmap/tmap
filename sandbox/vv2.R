##################
# VECTOR DATA
##################

# tm_polygons without specifications
tm_shape(World) +
	tm_polygons()

# these default values are configured by the option value.const:
str(tmap_options("value.const"))

# the 'fill' of polygons is "grey85", the border 'col' is "grey40", the default line width 'lwd' is 1, etc 

# the visual values can be set as follows: 
tm_shape(World) +
	tm_polygons(fill = "pink")

# vector of values -> facets
tm_shape(World) +
	tm_polygons(fill = c("pink", "purple"), 
				col = c("black", "white"), 
				lwd = c(1, 2))

## Visual variables

# scale and legend are determined automatically based on the type of variable
tm_shape(World) +
	tm_polygons(fill = "HPI")

# other scale and legend
tm_shape(World) +
	tm_polygons(fill = "HPI",
				fill.scale = tm_scale_continuous(values = "carto.army_rose"),
				fill.legend = tm_legend(orientation = "landscape"))

# facets
tm_shape(World) +
	tm_polygons(fill = c("HPI", "footprint"))

# if there is one 'facet dimension', facets are wrapped, similar to ggplot2's facet_wrap
tm_shape(World) +
	tm_polygons(fill = "HPI") +
	tm_facets(by =  "continent")

# note: facets can be forced into a single row or column via respectively tm_facets_hstack or tm_facets_vstack

# a facet grid (similar to ggplot2's facet_grid) can be achieved via tm_facets_grid
tm_shape(World) +
	tm_polygons(fill = c("HPI", "footprint")) +
	tm_facets_grid(columns =  "continent")


##################
# Multivariate
##################

# the follow plot shows two variables
tm_shape(World) +
	tm_polygons(tm_vars(c("HPI", "well_being")))

# in this example, tm_vars is not needed explicily (tm_polygons(c("HPI", "well_being")) is identical
# however, tm_vars can be used to specify multivariate visual variables, e.g. a bivariate choropleth:

tm_shape(World) +
	tm_polygons(tm_vars(c("HPI", "well_being"), multivariate = TRUE))

# multivariate visual variables are also used in RGB maps and glyphs (see below)

##################
# RASTER DATA
##################

# 'land' is a spatial raster data object with four attributes (variables). It is a 'stars' object ("stars" package), but these examples also work for a SpatRaster object ("terra" package)
(land)

# 

# note that default of tm_raster() is not value.const (like polygons) but all variables as facets:
tm_shape(land) +
	tm_raster()

# to plot just one variable
tm_shape(land) +
	tm_raster(col = "elevation")

tm_shape(land) +
	tm_raster(col = c("elevation", "trees")) +
	tm_facets_hstack()



# stars objects
file = system.file("tif/L7_ETMs.tif", package = "stars")
L7 = stars::read_stars(file)

# 'L7' is a stars object with one attribute, and one non-spatial dimension (called "band"):
(L7)

# by default all band values are plotted:
tm_shape(L7) +
	tm_raster()

# in order to 'extract' and plot only some dimension values, use tm_vars
tm_shape(L7) +
	tm_raster(col = tm_vars(dimvalues = 1:3))

tm_shape(L7) +
	tm_rgb(col = tm_vars(dimvalues = 1:3, multivariate = TRUE))

# we have a dedicated scale for rgb values (that maps 3 numeric variables to hex colors codes)
# this scale is adopted from stars::st_rgb
tm_shape(L7) +
	tm_rgb(col = tm_vars(dimvalues = 1:3, multivariate = TRUE), 
		   col.scale = tm_scale_rgb(stretch = "histogram"))

L7split = split(L7)

tm_shape(L7split) +
	tm_raster()

tm_shape(L7split) +
	tm_raster(col = tm_vars(1:3))

tm_shape(L7split) +
	tm_rgb(col = tm_vars(1:3, multivariate = TRUE))



# complex stars
L7neg = L7
L7neg$L7_ETMs.tif = 255 - L7neg$L7_ETMs.tif

L7duo = c(L7, L7neg)
L7duo2 = merge(L7duo)

tm_shape(L7duo) +
	tm_rgb(tm_vars("band", dimvalues = 1:3, multivariate = TRUE), col.scale = tm_scale_rgb(stretch = F))

tm_shape(L7duo) +
	tm_rgb(tm_vars("band", dimvalues = 1:3, multivariate = TRUE), col.scale = tm_scale_rgb(stretch = F)) +
	tm_facets(by = "VARS__")


tm_shape(L7duo2) +
	tm_rgb(tm_vars("band", dimvalues = 1:3, multivariate = TRUE), col.scale = tm_scale_rgb(stretch = F))

tm_shape(L7duo2) +
	tm_rgb(tm_vars("band", dimvalues = 1:3, multivariate = TRUE), col.scale = tm_scale_rgb(stretch = F)) +
	tm_facets(by = "attributes")


tm_shape(L7duo) +
	tm_raster(tm_vars("band", dimvalues = 1:3, multivariate = FALSE))

tm_shape(L7duo) +
	tm_raster(tm_vars("band", dimvalues = 1:3, multivariate = FALSE)) +
	tm_facets_grid(columns = "VARS__")


tm_shape(L7duo2) +
	tm_raster(tm_vars("band", dimvalues = 1:3, multivariate = FALSE)) +
	tm_facets_grid(columns = "VARS__")


# terra objects
L7_terra = terra::rast(file)

(L7_terra)

tm_shape(L7_terra) +
	tm_raster()

tm_shape(L7_terra) +
	tm_rgb(tm_vars(dimvalues = 1:3, multivariate = TRUE))


# tmap vars
tm_shape(World) +
	tm_polygons(tm_vars(c("HPI", "footprint")))

tm_shape(World) +
	tm_polygons(tm_vars(c("HPI", "footprint"), multivariate = TRUE))

tm_shape(World) +
	tm_polygons(c("HPI", "footprint"))

tm_shape(World) +
	tm_polygons(tm_vars(c("HPI", "footprint")))

tm_shape(World) +
	tm_polygons(tm_vars())



## Glyphs

#devtools::load_all("../tmap.glyphs/")
#remotes::install_github("https://github.com/r-tmap/tmap.glyphs")
library(tmap.glyphs)


tm_shape(NLD_prov) + 
	tm_polygons() +
	tm_donuts(parts = tm_vars(c("origin_native", "origin_west", "origin_non_west"), multivariate = TRUE),
			  size = "population",
			  size.scale = tm_scale_continuous(values.scale = 1),
			  fill.scale = tm_scale_categorical(values = "brewer.dark2"))
