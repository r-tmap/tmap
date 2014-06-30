rwb <- get_shape("../test/NDW_example/rw2013_doorgaand.shp")
loops <- get_shape("../test/NDW_example/loops2013.shp")

## Rijksweg 15 = A/N15 + A/N18
loops$roadname[loops$roadname=="A18"] <- "A15"
loops$roadnumber[loops$roadnumber==18] <- 15

loops$roadname <- factor(as.character(loops$roadname), levels=levels(rwb$ID))

library(rgeos)
########## assign loops to roads
d <- gDistance(rwb, loops, byid=TRUE)
d <- d[,1:(ncol(d)-1)]

looprwnames <- as.character(loops$roadname)

setdiff(looprwnames, dimnames(d)[[2]])

nl <- length(loops)

drw <- numeric(nl)
for (i in 1:length(loops)) {
	drw[i] <- d[i, looprwnames[i]]
}


nl
sum(drw<25)

loops$afstand <- factor(ifelse(drw<=10, "Binnen 10m",
						ifelse(drw<=25, "Binnen 25m",
						ifelse(drw<=50, "Binnen 50m", "Meer dan 50m"))), levels=c("Binnen 10m", "Binnen 25m", "Binnen 50m", "Meer dan 50m"))


save_shape(loops, "../test/NDW_example/loops2013_classified")
