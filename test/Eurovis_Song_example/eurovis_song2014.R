
song <- read.table("../test/Eurovis_Song_example/results2014.txt", sep="\t")

library(XML)
theurl <- "http://www.eurovision.tv/page/history/by-year/contest?event=1893#Scoreboard"
tables <- readHTMLTable(theurl, header=TRUE, stringsAsFactors=FALSE)



song <- tables[[2]]

countries <- song$Participant
countries[countries=="The Netherlands"] <- "Netherlands"

totals <- as.integer(song$Points)
names(totals) <- countries

nrow(song)
song <- matrix(unlist(lapply(song[,2:38], as.integer)), nrow=26)



dimnames(song) <- list(countries,
					   c("Albania", "Armenia", "Austria", "Azerbaijan", "Belarus", "Belgium", "Denmark", "Estonia",
					     "Macedonia", "Finland", "France", "Georgia", "Germany", "Greece", "Hungary", "Iceland",
					     "Ireland", "Israel", "Italy", "Latvia", "Lithuania", "Malta", "Moldova", "Montenegro", "Norway",
					     "Poland", "Portugal", "Romania", "Russia", "San Marino", "Slovenia", "Spain", "Sweden",
					     "Switzerland", "Netherlands", "Ukraine", "United Kingdom"))
					     

## check totals
totals-rowSums(song, na.rm=TRUE)
colSums(song, na.rm=TRUE)

save(song, totals, file="../test/Eurovis_Song_example/results2014.rdata")
load("../test/Eurovis_Song_example/results2014.rdata")

data(Europe)
setdiff(rownames(song) , Europe$name)
setdiff(colnames(song) , Europe$name)


Europe <- append_data(Europe, data=t(song), key.shp="name", fixed.order=FALSE)

Europe$type <- as.factor(ifelse(Europe$iso_a3=="NLD", "Netherlands", 
					  ifelse(Europe$name %in% rownames(song), "Participant in final",
					   ifelse(Europe$name %in% colnames(song), "Participant in semi final", NA))))

geo_shape(Europe, ylim=c(-.1, 1)) +
	geo_borders() + 
	geo_bubblemap(size="Netherlands", col="red", scale=1) +
	geo_text("Netherlands", scale=1, bg.color=NA) + 
	geo_choropleth("type", palette=c("orange", "gold1", "gold2")) +
	geo_theme_Europe(legend.profile="text", title="Points given to the Netherlands", 
			  legend.choro.title="Countries",
			  legend.bubble.size.title="Points")

geo_shape(Europe) +
	geo_borders() + 
	geo_fill(ifelse(Europe$iso_a3=="AUT", "orange", Europe$color)) +
	geo_text("iso_a3", cex="AREA3", scale=3) + 
	geo_bubblemap(size="Austria", col="red", scale=1) +
	geo_theme_Europe()
