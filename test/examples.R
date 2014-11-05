## load data that is contained in tmap package
data(NLD_prov)
data(NLD_muni)

## quick plot without statistical info
qtm(NLD_muni)

## what variables are contained in the shape object?
names(NLD_muni)

## plot the population counts
qtm(NLD_muni, fill = "population")

## similar to this, more flexible, notation:
tm_shape(NLD_muni) + 
	tm_fill("population") +
	tm_borders()

## two things to improve:
## 1) the population is scaled is linear. Often a kmeans clustering reveals more information
## 2) more importantly, the population counts have to be transformed into population density numbers, for otherwise a large area will look more populated than a small area with the same population number.  

tm_shape(NLD_muni) + 
	tm_fill("population", convert2density = TRUE,
			style = "kmeans") +
	tm_borders()

## enhancement: draw province borders
tm_shape(NLD_muni) + 
	tm_fill("population", convert2density = TRUE,
			style = "kmeans") +
	tm_borders("grey75") +
tm_shape(NLD_prov) +
	tm_borders(lwd = 2)

## add text
tm_shape(NLD_muni) + 
	tm_fill("population", convert2density = TRUE,
			style = "kmeans") +
	tm_borders("grey75") +
	tm_shape(NLD_prov) +
	tm_borders(lwd = 2) +
	tm_text("name", cex = .75, bg.color = "white")



rel_numbers <- NLD_muni@data[, c("pop_0_14", "pop_15_24",
								 "pop_25_44",  "pop_45_64",  "pop_65plus")]

abs_numbers <- rel_numbers / 100 * NLD_muni$population

NL_perc <- (sapply(abs_numbers, sum) / sum(NLD_muni$population)) * 100

rel_diff <- rel_numbers - rep(NL_perc, each=length(NLD_muni))
names(rel_diff) <- paste(names(rel_diff), "rd", sep="_")

NLD_muni <- append_data(NLD_muni, data = rel_diff)



## facets:
tm_shape(NLD_muni) + 
	tm_fill(c("pop_0_14_rd", "pop_15_24_rd",
			  "pop_25_44_rd",  "pop_45_64_rd",  "pop_65plus_rd"),
			palette="-RdYlBu",
			style = "kmeans") +
	tm_borders("grey75") +
	tm_shape(NLD_prov) +
	tm_borders(lwd = 2) +
	tm_text("name", cex = .75, bg.color = "white") +
	tm_facets(free.scales = FALSE) +
	tm_layout(scale=4)

