## load shape files
options(shp_dir=system.file("shapes", package="geoNL"))
		
shp.gm <- getShape("gm", 2012)


## load CBS-Statline data: NL population at 2012-01-01 at manucipality level
data(NLpop)

## add derived variables
NLpop <- transform(NLpop, perc.male=total.male/total*100, perc.female=total.female/total*100)

## append data to shape file
shp.gm <- appendData(shp.gm, data=NLpop, key.data="gm.name",key.shp="GM_NAAM")

## add population densities
shp.gm$pop.dens <- densities(shp.gm, "total")

## plot population density map on gm level
choropleth(shp.gm, "pop.dens", style="kmeans")

## draw province borders
shp.pv <- getShape("pv", 2012)
choropleth(shp.gm, "pop.dens", style="kmeans", shp2=shp.pv, lwd2=1)
