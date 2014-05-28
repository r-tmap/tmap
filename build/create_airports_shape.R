download.file("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/cultural/ne_10m_airports.zip", "../shapes/ne_10m_airports.zip")
unzip("../shapes/ne_10m_airports.zip", exdir="../shapes")
airports10 <- get_shape("../shapes/ne_10m_airports.shp")

names(airports10)

#airports10$size <- 10 - as.numeric(as.character(airports10$scalerank))

airports <- airports10
airports@data <- airports@data[, c("iata_code", "name", "scalerank", "natlscale")]

airports$name <- as.character(airports$name)

x <- grep("I_WAS_NOT_ASCII", iconv(airports$name, "latin1", "ASCII", sub="I_WAS_NOT_ASCII"))

airports$name[x] <- c("Rosario – Islas Malvinas Int", "Martin Miguel De Guemes Int", "Cayenne-Rochambeau", "Foz do Iguacu")



save(airports, file="./data/airports.rda", compress="xz")

