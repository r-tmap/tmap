library("readxl")
library("grid")

# function to obtain Food Environment Atlas data (2014)
get_food_envir_data <- function() {
	dir <- tempdir()
	download.file("https://www.ers.usda.gov/webdocs/DataFiles/48731/February2014.xls?v=41688", destfile = file.path(dir, "DataDownload.xls"), mode="wb")
	read_excel(file.path(dir, "DataDownload.xls"), sheet = "HEALTH")
}

# function to obtain US county shape
get_US_county_2010_shape <- function() {
	dir <- tempdir()
	download.file("http://www2.census.gov/geo/tiger/GENZ2010/gz_2010_us_050_00_20m.zip", destfile = file.path(dir, "gz_2010_us_050_00_20m.zip"))
	unzip(file.path(dir, "gz_2010_us_050_00_20m.zip"), exdir = dir)
	US <- read_shape(file.path(dir, "gz_2010_us_050_00_20m.shp"))
	levels(US@data$NAME) <- iconv(levels(US@data$NAME), from = "latin1", to = "utf8")
	US
}

# obtain Food Environment Atlas data
FEA <- get_food_envir_data()

# obtain US county shape
US <- get_US_county_2010_shape()

# quick map of the US shape
qtm(US)

# map with coordinate grid lines
qtm(US) + tm_grid()

# create FIPS variable
US$FIPS <- paste0(US$STATE, US$COUNTY)

# append data to shape
US <- append_data(US, FEA, key.shp = "FIPS", key.data = "FIPS", ignore.duplicates = TRUE)

# unmachted data
unmatched_data <- over_coverage()
str(unmatched_data)
View(FEA[unmatched_data$id, ])

# interactive choropleth
ttm()
qtm(US, fill = "PCT_OBESE_ADULTS10")

# process shapes
US_cont <- US %>% 
	subset(!STATE %in% c("02","15","72")) %>% 
	simplify_shape(0.2) 

US_AK <- US %>% 
	subset(STATE == "02") %>% 
	simplify_shape(0.2) 

US_HI <- US %>% 
	subset(STATE == "15") %>% 
	simplify_shape(0.2) 

# create state boundaries
US_states <- US_cont %>% 
	aggregate_map(by = "STATE")

# contiguous US
m_cont <- tm_shape(US_cont, projection=2163) +
	tm_polygons("PCT_OBESE_ADULTS10", border.col = "gray50", border.alpha = .5, title = "", showNA = TRUE) +
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
	tm_polygons("PCT_OBESE_ADULTS10", border.col = "gray50", border.alpha = .5, breaks = seq(10, 50, by = 5)) +
	tm_layout("Alaska", legend.show = FALSE, bg.color = NA, title.size = 0.8, frame = FALSE)

# Hawaii inset
m_HI <- tm_shape(US_HI, projection = 3759) +
	tm_polygons("PCT_OBESE_ADULTS10", border.col = "gray50", border.alpha = .5, breaks=seq(10, 50, by = 5)) +
	tm_layout("Hawaii", legend.show = FALSE, bg.color=NA, title.position = c("LEFT", "BOTTOM"), title.size = 0.8, frame=FALSE)

# specify viewports for Alaska and Hawaii
vp_AK <- viewport(x = 0.15, y = 0.15, width = 0.3, height = 0.3)
vp_HI <- viewport(x = 0.4, y = 0.1, width = 0.2, height = 0.1)

# change back to the plotting mode
tmap_mode("plot")

# print map
m_cont
print(m_AK, vp = vp_AK)
print(m_HI, vp = vp_HI)

# save map
save_tmap(m_cont, "US_adult_obesity.png", scale = 0.7, width = 6.125, 
		  insets_tm = list(m_AK, m_HI), 
		  insets_vp = list(vp_AK, vp_HI))
