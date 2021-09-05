source("sandbox/test_data.R")
source("../tmap_v4/tmap/sandbox/test_data.R")


# facet wrao
tm_shape(World) +
	tm_polygons("HPI") +
	tm_facets(by = "continent") #default: free.coords = TRUE


# facet wrao (same coords)
tm_shape(World) +
	tm_polygons("HPI") +
	tm_facets(by = "continent", free.coords = FALSE) #default: free.coords = TRUE

