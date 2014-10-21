data(NLD_muni)


NLD_muni$pop <- cut(NLD_muni$population, breaks = c(0, 10000, 40000, 100000, Inf))




NLD_muni2 <- do.call("sbind", split(NLD_muni, NLD_muni$pop))

cities2 <- do.call("sbind", split(cities, cities$country))

rivers2 <- do.call("sbind", split(rivers, rivers$scalerank))

