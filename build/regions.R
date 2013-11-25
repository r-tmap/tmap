regions <- read.csv2("regions.csv", header=FALSE)
names(regions) <- c("region", "code")
save(regions, file="../pkg/R/sysdata.rda")