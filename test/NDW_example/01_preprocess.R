### load package
devtools::load_all()
library(sp)
library(rgeos)
source("../test/NDW_example/00_misc_functions.R")

###########################################################################
##### load and preprocess loops data
###########################################################################
loopsdata <- read.csv("../test/NDW_example/input/Rijkswegen_loops.csv", stringsAsFactors=FALSE)

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
corop <- get_shape("../test/NDW_example/input/cr_2013.shp")
corop <- set_projection(corop, current.projection="rd")

nuts <- read.csv2("../test/NDW_example/COROP-NUTS.csv", stringsAsFactor=FALSE)
nuts$NUTS1 <- as.integer(substr(nuts$NUTS3, 3, 3))
nuts$NUTS2 <- as.integer(substr(nuts$NUTS3, 3, 4))

corop <- append_data(corop, data=nuts, key.shp="CR2013", key.data="COROP")



###########################################################################
##### load and preprocess road shape
###########################################################################

rw <- get_shape("../test/NDW_example/input/rijksweg2013.shp")
rw <- set_projection(rw, current.projection="rd")

rw.roadnumbers <- unique(as.numeric(as.character(rw$WEGNUMMER)))

setdiff(rw.roadnumbers, loops.roadnumbers)
setdiff(loops.roadnumbers, rw.roadnumbers)

rw$roadnumber <- as.numeric(as.character(rw$WEGNUMMER))
rw$roadname <- factor(loops.roadnames[match(rw$roadnumber, loops.roadnumbers)], levels=loops.roadnames)

#table(rw$roadnumber, rw$roadname, useNA="always")

rw <- rw[!is.na(rw$roadname),]

# ## exploration
# tapply(get_lengths(rw), list(rw$BAANSUBSRT), sum) 
# 
# 
# pdf("test.pdf", width=7,height=7)
# geo_shape(corop) +
# 	geo_fill() +
# 	geo_shape(rw) +
# 	geo_lines(lwd=.02, col="black", max.categories=20)+
# geo_shape(rw[rw$BAANSUBSRT=="HR",]) +
# 	geo_lines(lwd=.02, col="blue", max.categories=20) +
# geo_shape(rw[rw$BAANSUBSRT=="OPR",]) +
# 	geo_lines(lwd=.05, col="red", max.categories=20) +
# geo_shape(rw[rw$BAANSUBSRT=="PST",]) +
# 	geo_lines(lwd=.05, col="green", max.categories=20)
# dev.off()
# 
# pdf("test.pdf", width=7,height=7)
# geo_shape(corop) +
# 	geo_fill() +
# 	geo_shape(rw) +
# 	geo_lines(lwd=.02, col="RPE_CODE", max.categories=20)
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
geo_shape(corop, xlim=c(136000, 139000), ylim=c(451000, 452500), relative=FALSE) +
	geo_fill() +
	geo_shape(rw) +
	geo_lines(lwd=1.5, col="Segment", max.categories=20) +
	geo_shape(loops) +
	geo_bubbles(.025, "sensor", palette="black") +
	geo_theme("", legend.position=c("left", "bottom"), legend.titles=c(line.col="Road segment"),
			  legend.title.cex=.8, legend.text.cex=.6) #+
	#geo_grid()
dev.off()







rw <- rw[rw$BAANSUBSRT %in% c("HR", "PST"),]

rwL <- rw[rw$RPE_CODE=="L", ]
rwR <- rw[rw$RPE_CODE=="R", ]


# save all
save(loops, file="../test/NDW_example/throughput/loops.rda")
save(corop, file="../test/NDW_example/throughput/corop.rda")
save(rw, rwL, rwR, file="../test/NDW_example/throughput/rijkswegen.rda")

