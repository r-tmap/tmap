#' Get a PROJ.4 character string
#' 
#' Get full PROJ.4 string from either an existing PROJ.4 string or a shortcut.
#' 
#' @param x a \code{PROJ.4} character string or a shortcut, which is one of:
#' \describe{
#'    	\item{\code{"longlat"}}{Not really a projection, but a plot of the longitude-latitude coordinates (WGS84 datum).} 
#'    	\item{\code{"wintri"}}{Winkel Tripel (1921). Popular projection that is useful in world maps. It is the standard of world maps made by the National Geographic Society. Type: compromise} 
#'    	\item{\code{"robin"}}{Robinson (1963). Another popular projection for world maps. Type: compromise}
#'    	\item{\code{"eck4"}}{Eckert IV (1906). Projection useful for world maps. Area sizes are preserved, which makes it particularly useful for truthful choropleths. Type: equal-area}
#'    	\item{\code{"hd"}}{Hobo-Dyer (2002). Another projection useful for world maps in which area sizes are preserved. Type: equal-area}
#'    	\item{\code{"gall"}}{Gall (Peters) (1855). Another projection useful for world maps in which area sizes are preserved. Type: equal-area}
#'    	\item{\code{"merc"}}{Web Mercator. Projection in which shapes are locally preserved, a variant of the original Mercator (1569), used by Google Maps, Bing Maps, and OpenStreetMap. Areas close to the poles are inflated. Type: conformal}
#'    	\item{\code{"utmXX(s)"}}{Universal Transverse Mercator. Set of 60 projections where each projection is a traverse mercator optimized for a 6 degree longitude range. These ranges are called UTM zones. Zone \code{01} covers -180 to -174 degrees (West) and zone \code{60} 174 to 180 east. Replace XX in the character string with the zone number. For southern hemisphere, add \code{"s"}. So, for instance, the Netherlands is \code{"utm31"} and New Zealand is \code{"utm59s"}}
#'    	\item{\code{"mill"}}{Miller (1942). Projetion based on Mercator, in which poles are displayed. Type: compromise}
#'    	\item{\code{"eqc0"}}{Equirectangular (120). Projection in which distances along meridians are conserved. The equator is the standard parallel. Also known as Plate Carr\'ee. Type: equidistant}
#'    	\item{\code{"eqc30"}}{Equirectangular (120). Projection in which distances along meridians are conserved. The latitude of 30 is the standard parallel. Type: equidistant}
#'    	\item{\code{"eqc45"}}{Equirectangular (120). Projection in which distances along meridians are conserved. The latitude of 45 is the standard parallel. Also known as Gall isographic. Type: equidistant}
#'    	\item{\code{"rd"}}{Rijksdriehoekstelsel. Triangulation coordinate system used in the Netherlands.}
#'    	\item{EPSG code}{A valid code from the EPSG database}}
#'	@return validated PROJ.4 character string
#'	@importFrom rgdal CRSargs make_EPSG checkCRSArgs
#'	@import sp
#'	@seealso \url{http://en.wikipedia.org/wiki/List_of_map_projections} for a overview of projections. \url{http://trac.osgeo.org/proj/} for the \code{PROJ.4} project home page. An extensive list of \code{PROJ.4} codes can be created with rgdal's \code{\link[rgdal:make_EPSG]{make_EPSG}}.
#'	@export
get_proj4 <- function(x) {
	if (is.null(x)) return(NULL)
	if (is.na(x)) return(NA)
	if (is.numeric(x)) {
		y <- paste("+init=epsg:", x, sep="")
		if (!checkCRSArgs(y)[[1]]) stop("unknown EPSG code")
	} else {
		y <- switch(x,
					longlat="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs",
					latlong="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs",
					WGS84="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs",
					NAD83="+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs",
					NAD83="+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs",
					NAD27="+proj=longlat +ellps=clrk66 +datum=NAD27 +no_defs",
					wintri="+proj=wintri +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
					robin="+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
					eck4="+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
					hd="+proj=cea +lat_ts=37.5 +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
					gall="+proj=cea +lon_0=0 +lat_ts=45 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
					merc="+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs",
					mill="+proj=mill +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +R_A +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
					eqc0="+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
					eqc30="+proj=eqc +lat_ts=30 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
					eqc45="+proj=eqc +lat_ts=45 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
					rd="+init=epsg:28992",
					x)
		if (suppressWarnings(!is.na(as.numeric(y)))) {
			y <- paste("+init=epsg:", y, sep="")
			if (!checkCRSArgs(y)[[1]]) stop("unknown EPSG code")
		} else if (substr(y, 1, 3)=="utm") {
			y <- paste("+proj=utm +zone=", substr(y, 4, 5), ifelse(substr(y, 6, 6)=="s", " +south", ""), " +ellps=WGS84 +datum=WGS84 +units=m +no_defs", sep="")
		}
	}	
	CRSargs(CRS(y))
}
