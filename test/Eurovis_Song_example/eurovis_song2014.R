
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

Europe$type1 <- as.factor(ifelse(Europe$iso_a3=="AUT", "Austria", 
								 ifelse(Europe$name %in% rownames(song), "Participant in final",
								 	   ifelse(Europe$name %in% colnames(song), "Participant in semi final", NA))))

Europe$type2 <- as.factor(ifelse(Europe$iso_a3=="NLD", "Netherlands", 
					  ifelse(Europe$name %in% rownames(song), "Participant in final",
					   ifelse(Europe$name %in% colnames(song), "Participant in semi final", NA))))


xmod <- rep(0, length(Europe))
xmod[Europe$iso_a3=="NOR"] <- -.02

ymod <- rep(0, length(Europe))
ymod[Europe$iso_a3=="NOR"] <- -.06

Europe$iso_a3_eurovis <- Europe$iso_a3
Europe$iso_a3_eurovis[!(Europe$name %in% colnames(song))] <- NA

g <- geo_shape(Europe, ylim=c(-.1, 1)) +
	geo_borders() + 
	geo_text("iso_a3_eurovis", cex="AREA10", 
			 xmod=xmod,
			 ymod=ymod-.025,
			 bg.alpha=100,
			 scale=.5) +
	geo_bubblemap(size=c("Austria", "Netherlands"), col="orange", 
				  scale=1.5, xmod=xmod, ymod=ymod) +
	geo_choropleth(c("type1", "type2"), palette=c("orange", "lightblue", "lightgreen")) +
	geo_shape(Europe) +
	geo_text(c("Austria","Netherlands"), cex=c("Austria", "Netherlands"),
			 scale=.5, bg.color=NA, xmod=xmod, ymod=ymod, cex.lowerbound=.5) +
	geo_theme_Europe(legend.profile="text", title=c("Points for Austria", "Points for the Netherlands"), 
			  legend.choro.title="Countries",
			  legend.bubble.size.title="Points",
			  legend.text.cex=.5,
			  legend.title.cex=.8,
			  inner.margins=c(0,.5,0,0),
			  legend.NA.text=NA) +
	geo_grid(nrow=2)


pdf("../test/Eurovis_Song_example/plot1.pdf", width=5, height=7)
g
dev.off()

