data(NLD_prov)
data(NLD_muni)

## Compare to the original province population data
qtm(NLD_muni, fill="population", convert2density = TRUE)
qtm(NLD_prov, fill="population", convert2density = TRUE)

## For illustration, the population of provinces is derived from the municipality data
NLD_prov <- convert_shape_data(NLD_muni, NLD_prov, variables.from = "population")
qtm(NLD_prov, fill="population.data", convert2density = TRUE)

## Now, the population of province level is spread equally over the municipalities
NLD_muni <- convert_shape_data(NLD_prov, NLD_muni, variables.from = "population")
qtm(NLD_muni, fill="population.data", convert2density = TRUE)


