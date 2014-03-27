\name{igeoNL}
\alias{igeoNL}
\title{Interactive cartography}
\usage{
igeoNL()
}
\description{
Start the user interface
}
\examples{
\dontrun{
		# load shape files
		options(shp_dir=system.file("shapes", package="geoNL"))

		## load shape
		shp.gm <- getShape("gm", 2012)
		shp.pv <- getShape("pv", 2012)

		## load data
		data(NLpop)

		## add derived variables
		NLpop <- transform(NLpop, perc.male=total.male/total*100, perc.female=total.female/total*100)

		## append data to shape file
		shp.gm <- appendData(shp.gm, data=NLpop, key.data="gm.name",key.shp="GM_NAAM")

		## map gender ratio to color
		shp.gm$gender <- ((shp.gm$perc.male/100)-.5) / sqrt(shp.gm$total) * 10000

		igeoNL()

}
}

