#########################################################
### airports
#########################################################
# 
# 
# download.file("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/cultural/ne_10m_airports.zip", "../shapes/ne_10m_airports.zip")
# unzip("../shapes/ne_10m_airports.zip", exdir="../shapes")
# airports10 <- read_shape("../shapes/ne_10m_airports.shp")
# 
# names(airports10)
# 
# #airports10$size <- 10 - as.numeric(as.character(airports10$scalerank))
# 
# airports <- airports10
# airports@data <- airports@data[, c("iata_code", "name", "scalerank", "natlscale")]
# 
# airports$name <- as.character(airports$name)
# 
# x <- grep("I_WAS_NOT_ASCII", iconv(airports$name, "latin1", "ASCII", sub="I_WAS_NOT_ASCII"))
# 
# airports$name[x] <- c("Rosario - Islas Malvinas Int", "Martin Miguel De Guemes Int", "Cayenne-Rochambeau", "Foz do Iguacu")
# 
# 
# 
# save(airports, file="./data/airports.rda", compress="xz")

#########################################################
### Cities (from naturalearth)
#########################################################
# # natural earth
# download.file("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_populated_places_simple.zip", "../shapes/ne_50m_populated_places_simple.zip")
# unzip("../shapes/ne_50m_populated_places_simple.zip", exdir="../shapes")
# cities50 <- read_shape("../shapes/ne_50m_populated_places_simple.shp")
# 
# cities <- cities50
# cities@data <- cities@data[, (c("nameascii", "adm0name", "adm0_a3",  "featurecla", "pop_max", "pop_min"))]
# 
# cities$capital <- cities$featurecla == "Admin-0 capital"
# 
# cities@data <- cities@data[, c("nameascii", "adm0name", "adm0_a3",  "capital", "pop_max", "pop_min")]
# names(cities) <- c("name", "country", "iso_a3", "capital", "pop_max", "pop_min")
# 
# cities <- cities[cities$pop_max>=1e6 | cities$capital, ]
# 
# cities$name <- factor(as.character(cities$name), levels=intersect(levels(cities$name), as.character(cities$name)))
# 
# x <- grep("I_WAS_NOT_ASCII", iconv(levels(cities$name), "latin1", "ASCII", sub="I_WAS_NOT_ASCII"))
# levels(cities$name)[x] <- "Montreal"
# 
# 
# x <- grep("I_WAS_NOT_ASCII", iconv(levels(cities$name), "latin1", "ASCII", sub="I_WAS_NOT_ASCII"))
# 
# 
# cities$country <- factor(as.character(cities$country), levels=intersect(levels(cities$country), as.character(cities$country)))
# x <- grep("I_WAS_NOT_ASCII", iconv(levels(cities$country), "latin1", "ASCII", sub="I_WAS_NOT_ASCII"))
# 
# 
# 
# cities$iso_a3 <- factor(as.character(cities$iso_a3), levels=intersect(levels(cities$iso_a3), as.character(cities$iso_a3)))
# 
# cities <- set_projection(cities, current.projection = "longlat", overwrite.current.projection=TRUE)
# 
# str(ct@data)
# str(cities@data)
# 
# setdiff(as.character(ct$City), as.character(cities$name))
# setdiff(as.character(cities$name), as.character(ct$City))
# 
# save(cities, file="./data/cities.rda", compress="xz")







#########################################################
### metro
# source: http://esa.un.org/unpd/wup/CD-ROM/
#source: United Nations, Department of Economic and Social Affairs, Population Division (2014). World Urbanization Prospects: The 2014 Revision, CD-ROM Edition.
#########################################################


require(XLConnect)
require(XML)
convertStrings <- function(x) {
	# iconv(ct$name, to='ASCII//TRANSLIT')
	unwanted_array = list(    'Š'='S', 'š'='s', 'Ž'='Z', 'ž'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E', 'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U', 'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ă'='a', 'ç'='c', 'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o', 'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ü'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y')
	for(i in seq_along(unwanted_array))
		x <- gsub(names(unwanted_array)[i],unwanted_array[i],x)
	
	enc2native(x)
}
checkStrings <- function(x) {
	grep("I_WAS_NOT_ASCII", iconv(x, "latin1", "ASCII", sub="I_WAS_NOT_ASCII"))
}

u <- "http://www.nationsonline.org/oneworld/country_code_list.htm"
tables = readHTMLTable(u, stringsAsFactors=FALSE)

ccodes <- tables[["codelist"]]
ccodes$iso_a3 <- ccodes[[4]]
ccodes$ccode <- as.numeric(ccodes[[5]])

ct <- readWorksheetFromFile("../shapes/WUP2014-F12-Cities_Over_300K.xls",
							sheet="DATA", region="A17:Y1709", header=TRUE)
names(ct)[substr(names(ct),1,1)=="X"] <- paste0("pop", substr(names(ct),2,5)[substr(names(ct),1,1)=="X"])


ct$name <- convertStrings(ct$Urban.Agglomeration)
ct$name[checkStrings(ct$name)]

ct$name_long <- ct$name

ct$Country.Code[ct$Country.Code==729] <- 736 # all Sudan cities are in Sudan (not South Sudan)

ct$iso_a3 <- ccodes$iso_a3[match(ct$Country.Code, ccodes$ccode)]

!any(is.na(ct$iso_a3))

ct$name <- gsub("Basilan City (including City of Isabela)", "Basilan City", ct$name,fixed=TRUE)
ct$name <- gsub("Gaza (incl. Ash Shati Camp)", "Gaza", ct$name,fixed=TRUE)

ct$name <- gsub(".*\\((.*)\\).*", "\\1", ct$name)
ct$name <- gsub(",.*$", "", ct$name)

mlt <- strsplit(ct$name, "-")
mlt_len <- sapply(mlt, length)

mlt[mlt_len>1]

split_min <- c("BEL", "CAN", "JPN", "PHL", "PRI", "GBR", "USA")

ct$name[mlt_len>1 & ct$iso_a3 %in% split_min] <- sapply(mlt[mlt_len>1 & ct$iso_a3 %in% split_min], "[", 1)

ct[, c("pop1950", "pop1960", "pop1970", "pop1980", "pop1990", "pop2000", "pop2010", "pop2020", "pop2030")] <- ct[, c("pop1950", "pop1960", "pop1970", "pop1980", "pop1990", "pop2000", "pop2010", "pop2020", "pop2030")] * 1000

ct <- ct[,c("name", "name_long", "iso_a3", "Latitude", "Longitude", "pop1950", "pop1960", "pop1970", "pop1980", "pop1990", "pop2000", "pop2010", "pop2020", "pop2030")]

ct <- ct[ct$pop2010 >= 1e6, ]

## manual editing
ct$name[ct$name=="West Midlands"] <- "Birmingham"
ct$name[ct$name=="Bombay"] <- "Mumbai"
ct$name[ct$name=="Sana'a'"] <- "Sana'a"
ct$name[ct$name=="Marseille-Aix-en-Provence"] <- "Marseille"
ct$name[ct$name=="Thiruvananthapuram"] <- "Trivandrum"



library(sp)
metro <- SpatialPointsDataFrame(coords = ct[, c("Longitude", "Latitude")], data = ct[,setdiff(names(ct),c("Longitude", "Latitude"))], proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

save(metro, file="./data/metro.rda", compress="xz")

metro$Y1 <- (metro$pop2010 - metro$pop2000) / (metro$pop2000 * 10) * 100
metro$Y2 <- (metro$pop2010 - metro$pop1950) / (metro$pop1950 * 60) * 100

data(Europe)
data(World)
qtm(World) + 
qtm(metro, bubble.size = "pop2010", bubble.col="Y", bubble.style="fixed", bubble.breaks=c(-Inf, -1, -.1, .1, 1, 2, 4, 8, Inf))

tm_shape(World) +
	tm_fill("grey60") +
tm_shape(metro) +
	tm_bubbles("pop2010", col = "Y1", border.col = "black", border.alpha = .5, style="fixed", breaks=c(-Inf, seq(0, 6, by=2), Inf) ,palette="-RdYlBu", contrast=1, scale=1.5) + 
tm_layout_World("Population", legend.titles=c(bubble.col="Growth rate (%)"))



tm_shape(metro) +
tm_bubbles(c("pop1950", "pop1960", "pop1970", "pop1980", "pop1990", "pop2000", "pop2010", "pop2020", "pop2030")) +
tm_facets(free.scales = FALSE) +
	tm_layout(legend.show = FALSE)




