


download.file("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/physical/ne_50m_rivers_lake_centerlines_scale_rank.zip", "../shapes/ne_50m_rivers_lake_centerlines_scale_rank.zip")
unzip("../shapes/ne_50m_rivers_lake_centerlines_scale_rank.zip", exdir="../shapes")
rivers <- read_shape("../shapes/ne_50m_rivers_lake_centerlines_scale_rank.shp")

rivers$strokeweig <- rivers$strokeweig * 10


rivers@data <- rivers@data[, c("name", "featurecla", "scalerank", "strokeweig")]

names(rivers) <- c("name", "type", "scalerank", "strokelwd")




Encoding(levels(rivers$name)) <- "latin1"
levels(rivers$name) <- iconv(
	levels(rivers$name), 
	"latin1", 
	"ASCII",
	""
)

x <- grep("I_WAS_NOT_ASCII", iconv(levels(rivers$name), "latin1", "ASCII", sub="I_WAS_NOT_ASCII"))


rivers <- set_projection(rivers, current.projection = "longlat", overwrite.current.projection=TRUE)

save(rivers, file="./data/rivers.rda", compress="xz")
