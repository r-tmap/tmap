NLD_nbhd <- read_shape("../../shapes/buurt_2014.shp")
data(NLD_muni)
NLD_nbhd <- rgeos::gBuffer(NLD_nbhd, byid=TRUE, width=0)

# remove all water neighborhoods
NLD_nbhd <- NLD_nbhd[NLD_nbhd$OPP_LAND>0, ]

# process data
NLD_nbhd@data <- within(NLD_nbhd@data, {
	# convert land area ("OPP_LAND") from hectare (ha) to km2
	OPP_LAND <- OPP_LAND / 100
	
	# set negative percentages of western and non-western immigrants to 0
	P_WEST_AL[P_WEST_AL<0] <- 0
	P_N_W_AL[P_N_W_AL<0] <- 0
	
	# calculate population density values
	dens <- (AANT_INW / OPP_LAND)
	dens[is.nan(dens)] <- 0
	
	# divide population in three groups: western and non-western immigrants, and native Dutch
	west <- dens * P_WEST_AL / 100
	non_west <- dens * P_N_W_AL / 100
	dutch <- dens - non_west - west
})

qtm(NLD_nbhd, fill="dens")

NLD_pop_dens <- smooth_map(NLD_nbhd, var="dens", breaks=c(0,500,1000, 2500, 5000, Inf), cover = NLD_muni)

save(NLD_pop_dens, file="./data/NLD_pop_dens.rda", compress="xz")

