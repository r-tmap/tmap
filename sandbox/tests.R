source("sandbox/test_data.R")
source("../tmap_v4/tmap/sandbox/test_data.R")


# SHAPES
# one polygon shape
tm_shape(World) +
	tm_polygons("HPI")

# one polygon shape, two aes values
tm_shape(World) +
	tm_polygons(c("HPI", "income_grp"))

# split polygon shape (by)
tm_shape(World) +
	tm_polygons("HPI") +
	tm_facets(by = "continent")


# split polygon shape (by), with fixed coords
tm_shape(World) +
	tm_polygons("HPI") +
	tm_facets(by = "continent", free.coords = FALSE)

#


# SCALING
tm = tm_shape(World) +
	tm_polygons("HPI") +
	tm_symbols(size = "pop_est")

# one panel
tm + tm_layout(panel.labels = "Test")

tm + tm_facets(by = "continent")
tm + tm_facets(by = "income_grp")

tm + tm_facets_grid(rows = "income_grp", columns = "economy")
tm + tm_facets(by = c("income_grp", "economy"))






# PANELS

tm_shape(World) +
	tm_polygons("HPI") +
	tm_symbols(size = "pop_est") +
	tm_facets(by = "continent", free.coords = FALSE) +
	tm_options(scale = .5)

tm_shape(World) +
	tm_polygons("HPI") +
	tm_symbols(size = "pop_est") +
	tm_layout(panel.show = TRUE, panel.labels = "Test")
	#tm_facets(by = "continent", free.coords = FALSE, scale.factor = 2)



tm_shape(World) +
	tm_polygons("HPI") +
	tm_layout(panel.show = TRUE, panel.labels = "Test")
	

# 






# facet wrao
tm_shape(World) +
	tm_polygons("HPI") +
	tm_facets(by = "continent") #default: free.coords = TRUE


# facet wrao (same coords)
tm_shape(World) +
	tm_polygons("HPI") +
	tm_facets(by = "continent", free.coords = FALSE) #default: free.coords = TRUE

# for rasters:
# v3
tm_shape(land) + 
	tm_raster("trees", palette = "viridis") +
	tm_facets(by = "cover_cls")

tm_shape(land) + 
	tm_raster("trees", palette = "viridis") +
	tm_facets(by = "cover_cls", free.coords = FALSE)

# v4
tm_shape(land) + 
	tm_raster("trees", col.scale = tm_scale_intervals(values = "viridis")) +
	tm_facets(by = "cover_cls")

tm_shape(land) + 
	tm_raster("trees", col.scale = tm_scale_intervals(values = "viridis")) +
	tm_facets(by = "cover_cls", free.coords = FALSE)



tm_shape(land) + 
	tm_raster("trees")



tm_shape(land) + 
	tm_raster("trees", colorNA = "black") +
	tm_facets(by = "cover_cls")



# panel labels
tm_shape(World) +
	tm_polygons("HPI") +
	tm_

