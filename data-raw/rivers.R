library(rnaturalearth)
library(sf)
library(dplyr)

rivers <- rnaturalearth::ne_download(scale = 50, type = "rivers_lake_centerlines_scale_rank", category = "physical", returnclass = "sf")
rivers$strokeweig <- rivers$strokeweig * 10

rivers <- rivers %>% select(name, type=featurecla, scalerank, strokelwd = strokeweig)

#rivers$name <- factor(rivers$name)
rivers$type <- factor(rivers$type)
rivers$scalerank <- as.integer(rivers$scalerank)
rivers$strokelwd <- as.numeric(rivers$strokelwd)

Encoding(rivers$name) <- "latin1"
rivers$name <- iconv(
	rivers$name, 
	"latin1", 
	"ASCII",
	""
)
x <- grep("I_WAS_NOT_ASCII", iconv(levels(rivers$name), "latin1", "ASCII", sub="I_WAS_NOT_ASCII"))

rivers = sf::st_set_crs(rivers, 4326)

rivers = rivers %>% 
	filter(!is.na(type)) 
	

save(rivers, file="data/rivers.rda", compress="xz")
