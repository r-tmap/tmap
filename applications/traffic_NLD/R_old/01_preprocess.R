### load package
devtools::load_all()
library(sp)
library(rgeos)
source("../applications/traffic_NLD/00_misc_functions.R")

###########################################################################
##### load and preprocess loops data
###########################################################################
loopsdata <- read.csv("../applications/traffic_NLD/input/Rijkswegen_loops.csv", stringsAsFactors=FALSE)

names(loopsdata) <- c("site", "lat", "long", "roadname", "roadnumber")


## Rijksweg 15 = A/N15 + A/N18
loopsdata$roadname[loopsdata$roadname=="A18"] <- "A15"
loopsdata$roadnumber[loopsdata$roadnumber==18] <- 15



loops.roadnumbers <- unique(loopsdata$roadnumber)
loops.roadnames <- loopsdata$roadname[match(loops.roadnumbers, loopsdata$roadnumber)]

loopsdata$roadname <- factor(loopsdata$roadname, levels=loops.roadnames)
loopsdata$roadname[is.na(loopsdata$roadname)] <- loops.roadnames[match(loopsdata$roadnumber[is.na(loopsdata$roadname)], loops.roadnumbers)]

loops <- SpatialPointsDataFrame(coords=loopsdata[,c("long", "lat")], data=loopsdata)
loops <- set_projection(loops, current.projection="longlat", projection="rd")




###########################################################################
##### load and preprocess corop shape
###########################################################################
corop <- read_shape("../applications/traffic_NLD/input/cr_2013.shp")
corop <- set_projection(corop, current.projection="rd")

nuts <- read.csv2("../applications/traffic_NLD/input/COROP-NUTS.csv", stringsAsFactor=FALSE)
nuts$NUTS1 <- as.integer(substr(nuts$NUTS3, 3, 3))
nuts$NUTS2 <- as.integer(substr(nuts$NUTS3, 3, 4))

corop <- append_data(corop, data=nuts, key.shp="CR2013", key.data="COROP")



###########################################################################
##### load and preprocess road shape
###########################################################################

rw <- read_shape("../applications/traffic_NLD/input/rijksweg2013.shp")
rw <- set_projection(rw, current.projection="rd")

rw.roadnumbers <- unique(as.numeric(as.character(rw$WEGNUMMER)))

setdiff(rw.roadnumbers, loops.roadnumbers)
setdiff(loops.roadnumbers, rw.roadnumbers)

rw$roadnumber <- as.numeric(as.character(rw$WEGNUMMER))
rw$roadname <- factor(loops.roadnames[match(rw$roadnumber, loops.roadnumbers)], levels=loops.roadnames)

#table(rw$roadnumber, rw$roadname, useNA="always")

rw <- rw[!is.na(rw$roadname),]

# ## exploration
# tapply(read_lengths(rw), list(rw$BAANSUBSRT), sum) 
# 
# 
# pdf("test.pdf", width=7,height=7)
# tm_shape(corop) +
# 	tm_fill() +
# 	tm_shape(rw) +
# 	tm_lines(lwd=.02, col="black", max.categories=20)+
# tm_shape(rw[rw$BAANSUBSRT=="HR",]) +
# 	tm_lines(lwd=.02, col="blue", max.categories=20) +
# tm_shape(rw[rw$BAANSUBSRT=="OPR",]) +
# 	tm_lines(lwd=.05, col="red", max.categories=20) +
# tm_shape(rw[rw$BAANSUBSRT=="PST",]) +
# 	tm_lines(lwd=.05, col="green", max.categories=20)
# dev.off()
# 
# pdf("test.pdf", width=7,height=7)
# tm_shape(corop) +
# 	tm_fill() +
# 	tm_shape(rw) +
# 	tm_lines(lwd=.02, col="RPE_CODE", max.categories=20)
# dev.off()
# 

# "AFR" "BST" "BU"  "FP"  "HR"  "MRB" "NRB" "OPR" "PAR" "PKB" "PKP" "PST" "TN"  "VBD" "VBI" "VBK" "VBR" "VBS" "VBW"
# AFR = afrit
# HR ? hoofd route
# PKB ? af/oprit?
# PST ? invoegstrook?
# OPR = oprit



## create plots
rw$Segment <- factor(ifelse(rw$BAANSUBSRT=="HR", "Main route", ifelse(rw$BAANSUBSRT=="AFR", "Exit ramp", ifelse(rw$BAANSUBSRT=="OPR", "Entrance ramp", "Other"))),
					 levels=c("Main route", "Exit ramp", "Entrance ramp", "Other"))


loops$sensor <- factor("Road sensor")


### example: Knooppunt Lunetten (A12/A27)
png("../test/NDW_example/plots/rijkswegen_met_loops.png", width=1600,height=800, res=300)
tm_shape(corop, xlim=c(136000, 139000), ylim=c(451000, 452500), relative=FALSE) +
	tm_fill() +
	tm_shape(rw) +
	tm_lines(lwd=1.5, col="Segment", max.categories=20) +
	tm_shape(loops) +
	tm_bubbles(.025, "sensor", palette="black") +
	tm_layout("", legend.position=c("left", "bottom"), legend.titles=c(line.col="Road segment"),
			  legend.title.cex=.8, legend.text.cex=.6) #+
	#tm_grid()
dev.off()







rw <- rw[rw$BAANSUBSRT %in% c("HR", "PST"),]

rwL <- rw[rw$RPE_CODE=="L", ]
rwR <- rw[rw$RPE_CODE=="R", ]


# save all
save(loops, file="../applications/traffic_NLD/throughput/loops.rda")
save(corop, file="../applications/traffic_NLD/throughput/corop.rda")
save(rw, rwL, rwR, file="../applications/traffic_NLD/throughput/rijkswegen.rda")

