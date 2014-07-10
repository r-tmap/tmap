### load package
devtools::load_all()
library(sp)
library(rgeos)

### load preprocessed data
load("../test/NDW_example/throughput/loops.rda")
load("../test/NDW_example/throughput/rijkswegen.rda")
source("../test/NDW_example/00_misc_functions.R")

## simplify roads

#### testing mode A18
# rwA18 <- rw[rw$roadname=="A18", ]
# loopsA18 <- loops[loops$roadname=="A18", ]
# 
# shps <- list(rwA18, loopsA18)
# id="roadname"
# min.dist=200
# max.opt.dist=250
# sep.dist=5000
# verbose=TRUE
# 
# rwbA18 <- fit_polylines(rwA18, loopsA18, id="roadname")
# rwbA18b <- fit_polylines(rwA18, min.dist=0, loopsA18, id="roadname")
# 
# pdf("test.pdf", width=7, height=7)
# geo_shape(corop) +
# 	geo_fill("gray65")+
# 	geo_borders("white") +
# 	geo_shape(rwbA18) +
# 	geo_lines() +
# 	geo_shape(rwbA18b) +
# 	geo_lines(lwd=.4, col="blue") +
# 	geo_theme(scale=.2)
# dev.off()

##### testing mode N35
# rwN35 <- rw[rw$roadname=="N35", ]
# loopsN35 <- loops[loops$roadname=="N35", ]
# 
# shps <- list(rwN35, loopsN35)
# id="roadname"
# min.dist=10
# max.opt.dist=250
# sep.dist=5000
# verbose=TRUE
# 
# rwbN35 <- fit_polylines(rwN35, loopsN35, sep.dist=20000, id="roadname")
# pdf("test.pdf", width=7, height=7)
# geo_shape(corop) +
# 	geo_fill("gray65")+
# 	geo_borders("white") +
# geo_shape(rwbN35) +
# 	geo_lines() +
# geo_shape(rwN35) +
# 	geo_lines(lwd=.05, col="purple") +
# geo_shape(loopsN35) +
# 	geo_bubbles(size=.02) +
# 	geo_theme(scale=.2)
# dev.off()



system.time({
	#drw <- fit_polylines(rw, loops, id="roadname")
	drw <- fit_polylines(rw, id="roadname")
})

system.time({
	drwL <- fit_polylines(rwL, id="roadname")
})

system.time({
	drwR <- fit_polylines(rwR, id="roadname")
})



save(drw, drwL, drwR, file="../test/NDW_example/throughput/doorlopende_rijkswegen.rda")

### diagnostics
pdf("../test/NDW_example/output/doorlopende_rijkswegen.pdf", width=7, height=7)
geo_shape(corop) +
	geo_fill("gray65")+
#	geo_borders("white") +
geo_shape(drw) +
	geo_lines() +
geo_shape(rw) +
	geo_lines(lwd=.05, col="purple") +
geo_shape(loops) +
	geo_bubbles(size=.001, col="black") +
	geo_theme(scale=.2)
dev.off()

pdf("../test/NDW_example/output/doorlopende_rijkswegenL.pdf", width=7, height=7)
geo_shape(corop) +
	geo_fill("gray65")+
	#	geo_borders("white") +
	geo_shape(drwL) +
	geo_lines() +
	geo_shape(rwL) +
	geo_lines(lwd=.05, col="purple") +
	geo_shape(loops) +
	geo_bubbles(size=.001, col="black") +
	geo_theme(scale=.2)
dev.off()

pdf("../test/NDW_example/output/doorlopende_rijkswegenR.pdf", width=7, height=7)
geo_shape(corop) +
	geo_fill("gray65")+
	#	geo_borders("white") +
	geo_shape(drwR) +
	geo_lines() +
	geo_shape(rwR) +
	geo_lines(lwd=.05, col="purple") +
	geo_shape(loops) +
	geo_bubbles(size=.001, col="black") +
	geo_theme(scale=.2)
dev.off()


### output table length per doorlopende rijksweg
drw_len <- data.frame(roadname=levels(drw$ID), length=NA,lengthL=NA,lengthR=NA, stringsAsFactors=FALSE)



drw_len$length[as.integer(drw$ID)] <- get_lengths(drw)
drw_len$lengthL[as.integer(drwL$ID)] <- get_lengths(drwL)
drw_len$lengthR[as.integer(drwR$ID)] <- get_lengths(drwR)
drw_len <- rbind(drw_len, data.frame(roadname="total", length=sum(drw_len$length, na.rm=TRUE),
									 lengthL=sum(drw_len$lengthL, na.rm=TRUE), 
									 lengthR=sum(drw_len$lengthR, na.rm=TRUE)))

write.table(drw_len, file="../test/NDW_example/output/lengtes_drw.csv", row.names=FALSE, sep=",")
