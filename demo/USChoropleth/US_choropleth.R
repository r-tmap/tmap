library("readxl")
library("maptools")
library("grid")

# function to obtain Food Environment Atlas data
get_food_envir_data <- function() {
	dir <- tempdir()
	download.file("http://www.ers.usda.gov/webdocs/DataFiles/Food_Atlas_Data_Access_and_Documentation_Downloads__18030/DataDownload.xls", destfile = file.path(dir, "DataDownload.xls"), mode="wb")
	read_excel(file.path(dir, "DataDownload.xls"), sheet = "HEALTH")
}

# function to obtain US county shape
get_US_county_2010_shape <- function() {
	dir <- tempdir()
	download.file("http://www2.census.gov/geo/tiger/GENZ2010/gz_2010_us_050_00_20m.zip", destfile = file.path(dir, "gz_2010_us_050_00_20m.zip"))
	unzip(file.path(dir, "gz_2010_us_050_00_20m.zip"), exdir = dir)
	read_shape(file.path(dir, "gz_2010_us_050_00_20m.shp"))
}

# obtain Food Environment Atlas data
FEA <- get_food_envir_data()

# obtain US county shape
US <- get_US_county_2010_shape()

# quick map of the US shape
qtm(US)

# create FIPS variable
US$FIPS <- paste0(US$STATE, US$COUNTY)

# append data to shape
US <- append_data(US, FEA, key.shp = "FIPS", key.data = "FIPS")

# unmatched counties (that are in the shape, but not in the data)
unmatched_counties <- under_coverage()
str(unmatched_counties)

# view unmatched counties in interactive mode:
ttm()
qtm(US[unmatched_counties$id,], fill = "red")

# interactive choropleth
qtm(US, fill = "PCT_OBESE_ADULTS10")

# split shape 
US_cont <- US[!(US$STATE %in% c("02","15","72")),]  
US_AK <- US[US$STATE == "02", ]
US_HI <- US[US$STATE == "15",]

# create state boundaries
US_states <- unionSpatialPolygons(US_cont, IDs=US_cont$STATE)

# change back to the plotting mode
tmap_mode("plot")

# plot contiguous US
tm_shape(US_cont, projection=2163) +
	tm_polygons("PCT_OBESE_ADULTS10", border.col = "grey50", border.alpha = .5, title = "", showNA = TRUE) +
	tm_shape(US_states) +
	tm_borders(lwd=1, col = "black", alpha = .5) +
	tm_credits("Data @ Unites States Department of Agriculture\nShape @ Unites States Census Bureau", position = c("right", "bottom")) +
	tm_layout(title="2010 Adult Obesity by County, percent", 
			  title.position = c("center", "top"), 
			  legend.position = c("right", "bottom"), 
			  frame = FALSE, 
			  inner.margins = c(0.1, 0.1, 0.05, 0.05))

# Alaska inset
m_AK <- tm_shape(US_AK, projection = 3338) +
	tm_polygons("PCT_OBESE_ADULTS10", border.col = "grey50", border.alpha = .5, breaks = seq(10, 50, by = 5)) +
	tm_layout("Alaska", legend.show = FALSE, bg.color = NA, title.size = 0.8, frame = FALSE)

# Hawaii inset
m_HI <- tm_shape(US_HI, projection = 3759) +
	tm_polygons("PCT_OBESE_ADULTS10", border.col = "grey50", border.alpha = .5, breaks=seq(10, 50, by = 5)) +
	tm_layout("Hawaii", legend.show = FALSE, bg.color=NA, title.position = c("LEFT", "BOTTOM"), title.size = 0.8, frame=FALSE)

# print insets
print(m_AK, vp=viewport(x= 0.15, y= 0.15, width= 0.3, height= 0.3))
print(m_HI, vp=viewport(x= 0.4, y= 0.1, width= 0.2, height= 0.1))
