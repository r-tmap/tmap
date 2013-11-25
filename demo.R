## load shape files
options(shp_dir="./inst/shapes/")

shp.gm <- getShape("gm", 2012)
shp.pv <- getShape("pv", 2012)


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
choropleth(shp.gm, "pop.dens", style="kmeans", shp2=shp.pv, lwd2=1)

## draw absolute population totals
bubbleMap(shp.gm, x="total")

## map gender ratio to color
shp.gm$gender <- ((shp.gm$perc.male/100)-.5) / sqrt(shp.gm$total) * 10000

bubbleMap(shp.gm, x="total", col="gender", palette="RdBu", n=5, style="kmeans", legend.labels=c("more women", "slightly more women", "equal", "slightly more men", "more men"),
		  show.legend.sizes=TRUE, shp2=shp.pv, lwd2=2)

igeoNL()

animateMaps(choropleth(shp.gm, x=names(shp.gm)[11:13],smallMultiples=FALSE))
