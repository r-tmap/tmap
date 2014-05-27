library(sp)
library(maptools)
library(raster)
library(rgeos)
library(rgdal)
devtools::load_all(".")

NLD_prov <- get_shape("../shapes/pv_2012.shp")
NLD_muni <- get_shape("../shapes/gm_2012.shp")

NLD_prov <- set_projection(NLD_prov, current.projection="rd")
NLD_muni <- set_projection(NLD_muni, current.projection="rd")



## downloaded from
#http://statline.cbs.nl/StatWeb/publication/?VW=T&DM=SLNL&PA=03759NED&D1=0-2&D2=129-132&D3=101-104,109,113-114,116-119,121,124,126,128-129,134-135,138-140,143-144,148-149,152,154,156,159,162,167-171,178,180-184,186-189,191,193-194,196-198,202,204-208,210,213,216,219-220,222-225,227-228,230,232-234,236,238,240-242,244-245,247,252,255,257-258,260-261,263-264,267-268,270,272,278-280,282-283,285,289-291,293-297,300,303,306-307,309,311,313,316,320-321,323-324,326-327,329,331-333,336,339,344-346,349-354,356,360-365,368,374-377,379,381,385,387,390-393,395,397,399,401-402,404-406,409-410,416-418,421-422,425-426,431,435,437,439-440,442,449-451,454-458,460-463,466,468-471,473,475-478,485,487-488,491,493-494,496,498-500,508-510,512-513,520-521,523,526,531-535,537-538,544-546,553-554,556,558,561-562,564-565,568,571,575-576,584-585,587-588,590-592,595,597-598,602,604-607,610,612-615,620-623,627-629,632-633,636-641,643,645,649,651-652,657-659,661-664,669-670,673-674,676-677,681-682,686,688-689,695,698-703,706,708,710-711,716-717,719-720,722-724,727,731-734,738,740-741,743,746-747,749,751-752,758-759,761-763,765,767-773,775-776,778,780-782,785,787-793,795-796,798,801-802,805,807-808,810,813-814,818-822,829,831,833-834,836,839,844-845,847-850,853-854,858,863-864,868-869,871,873,875-876,880-881,883-884,886-887,889,891,897-900,902,904-906,909-910&D4=24&HD=140318-1003&HDR=T,G1&STB=G2,G3

# delete first two and last row
data <- read.csv2("../build/Bevolking__leeftijd,_180314100401.csv", header=TRUE, stringsAsFactors=FALSE)
data <- as.matrix(data)
data <- data[-2, -2]
colnames(data) <- c("muni", "pop", "pop_0_20", "pop_20_65", "pop_65p", "men", "men_0_20", "men_20_65", "men_65p", "women", "women_0_20", "women_20_65", "women_65p")
data <- data[-1,]
data <- as.data.frame(data, stringsAsFactors=FALSE)

data[,-1] <- lapply(data[,-1], as.integer)

setdiff(data[,1], NLD_muni$GM_NAAM)
setdiff(NLD_muni$GM_NAAM, data[,1])

nm_dataid <- which(!(data[,1] %in% NLD_muni$GM_NAAM))
nm_shpid <- which(!(NLD_muni$GM_NAAM %in% data[,1]))

nm_data <- data[nm_dataid, 1]
nm_shp <- as.character(NLD_muni$GM_NAAM[nm_shpid])

## compare first four digits
nm_data2 <- nm_shp[match(substr(nm_data, 1, 4), substr(nm_shp, 1, 4))]
nm_data2[is.na(nm_data2)] <- nm_shp[!(substr(nm_shp, 1, 4) %in% substr(nm_data, 1, 4))]

data[nm_dataid, 1] <- nm_data2

setdiff(data[,1], NLD_muni$GM_NAAM)
setdiff(NLD_muni$GM_NAAM, data[,1])

NLD_muni@data <- NLD_muni@data[, c("GM_2012", "GM_NAAM")]
names(NLD_muni) <- c("code", "name")

vars <- c(2, 6, 10)


NLD_ageGroups <- data[, -vars]

NLD_muni <- append_data(NLD_muni, data[,c(1,vars)], key.data="muni", key.shp="name")
NLD_muni$muni <- NULL

NLD_prov@data <- NLD_prov@data[, c("PV_2012", "PV_NAAM")]
names(NLD_prov) <- c("code", "name")

NLD_prov <- convert_shape_data(NLD_muni, NLD_prov)


## check data
gIsValid(NLD_muni)
gIsValid(NLD_prov)

save(NLD_ageGroups, file="./data/NLD_ageGroups.rda", compress="xz")
save(NLD_muni, file="./data/NLD_muni.rda", compress="xz")
save(NLD_prov, file="./data/NLD_prov.rda", compress="xz")



