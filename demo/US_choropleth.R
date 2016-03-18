library("tmap")
library("readxl")
library("maptools")
library("grid")

# download data
download.file("http://www.ers.usda.gov/datafiles/Food_Environment_Atlas/Data_Access_and_Documentation_Downloads/Current_Version/DataDownload.xls", destfile = "DataDownload.xls", mode="wb")
df <- read_excel("DataDownload.xls", sheet = "HEALTH")

# download shape
f <- tempfile()
download.file("http://www2.census.gov/geo/tiger/GENZ2010/gz_2010_us_050_00_20m.zip", destfile = f)
unzip(f, exdir = ".")
US <- read_shape("gz_2010_us_050_00_20m.shp")

# append data to shape
US$FIPS <- paste0(US$STATE, US$COUNTY)
US <- append_data(US, df, key.shp = "FIPS", key.data = "FIPS")

# split shape 
US_cont <- US[!(US$STATE %in% c("02","15","72")),]  
US_AK <- US[US$STATE == "02", ]
US_HI <- US[US$STATE == "15",]

# create state boundaries
US_states <- unionSpatialPolygons(US_cont, IDs=US_cont$STATE)

# plot contiguous US
tm_shape(US_cont, projection=2163) +
	tm_polygons("PCT_OBESE_ADULTS10", border.col = "grey50", border.alpha = .5, title="", showNA=TRUE) +
	tm_shape(US_states) +
	tm_borders(lwd=1, col = "black", alpha = .5) +
	tm_credits("Data @ Unites States Department of Agriculture\nShape @ Unites States Census Bureau", position = c("right", "bottom")) +
	tm_layout(title="2010 Adult Obesity by County, percent", 
			  title.position = c("center", "top"), 
			  legend.position = c("right", "bottom"), 
			  frame = FALSE, 
			  inner.margins = c(.1,.1,.05,.05))

# Alaska inset
m_AK <- tm_shape(US_AK, projection = 3338) +
	tm_polygons("PCT_OBESE_ADULTS10", border.col = "grey50", border.alpha = .5, breaks = seq(10, 50, by = 5)) +
	tm_layout("Alaska", legend.show = FALSE, bg.color = NA, title.size = .8, frame = FALSE)

# Hawaii inset
m_HI <- tm_shape(US_HI, projection = 3759) +
	tm_polygons("PCT_OBESE_ADULTS10", border.col = "grey50", border.alpha = .5, breaks=seq(10, 50, by = 5)) +
	tm_layout("Hawaii", legend.show = FALSE, bg.color=NA, title.position = c("LEFT", "BOTTOM"), title.size = .8, frame=FALSE)

# print insets
print(m_AK, vp=viewport(x=.15, y=.15, width=.3, height=.3))
print(m_HI, vp=viewport(x=.4, y=.1, width=.2, height=.1))

# export to png image
save_tmap(m_cont, "./paper/figures/USchoro.png", scale=.7, width=6.125, insets_tm = list(m_AK, m_HI), insets_vp = list(viewport(x=.15, y=.15, width=.3, height=.3), viewport(x=.4, y=.1, width=.2, height=.1)))
