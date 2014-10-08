### load package
devtools::load_all()
library(sp)
library(rgeos)

### load preprocessed data
load("../applications/traffic_NLD//throughput/loops.rda")
load("../applications/traffic_NLD//throughput/rijkswegen.rda")
load("../applications/traffic_NLD//throughput/corop.rda")
source("../applications/traffic_NLD//00_misc_functions.R")

load("../applications/traffic_NLD/throughput/doorlopende_rijkswegen.rda")
load("../applications/traffic_NLD/output/road_list_info.rdata")


### Google Earth animation
plot_google(drwL, listL, rn = "A79")
plot_google(drwR, listR, rn = "A79")

listL[[1]]
