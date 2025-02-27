% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tm_shape.R
\name{tm_shape}
\alias{tm_shape}
\title{Shape (spatial object) specification}
\usage{
tm_shape(
  shp = NULL,
  bbox = NULL,
  crs = NULL,
  is.main = NA,
  name = NULL,
  unit = NULL,
  filter = NULL,
  ...
)
}
\arguments{
\item{shp}{Spatial object}

\item{bbox}{Bounding box of the map (only used if \code{shp} is the main shape (see \code{is.main})}

\item{crs}{Map projection (CRS). Can be set to an \code{crs} object (see \code{\link[sf:st_crs]{sf::st_crs()}}), a proj4string, an EPSG number, the value \code{"auto"} (automatic crs recommendation), or one the the following generic projections: \code{c("laea", "aeqd", "utm", "pconic", "eqdc", "stere")}. See details.}

\item{is.main}{Is \code{shp} the main shape, which determines the crs and
bounding box of the map? By default, \code{TRUE} if it is the first \code{tm_shape} call}

\item{name}{of the spatial object}

\item{unit}{Unit of the coordinates}

\item{filter}{Filter features}

\item{...}{passed on to \code{\link[tmaptools:bb]{bb}} (e.g. \code{ext} can be used to enlarge or shrink a bounding box)}
}
\description{
Specify a shape, which is a spatial object from one of these spatial object
class packages: \code{\link[sf:sf]{sf}}, \code{\link[stars:st_as_stars]{stars}}, or \code{terra}.
}
\details{
The map projection (\code{crs}) determines in which coordinate system the spatial object is processed and plotted.
See \href{https://r-tmap.github.io/tmap/articles/foundations_crs}{vignette about CRS}. The \code{crs} can be specified in two places: 1) \code{tm_shape()} and \code{tm_crs()}.
In both cases, the map is plotted into the specified \code{crs}. The difference is that in the first option, the \code{crs} is also taken into account in spatial transformation functions, such as the calculation of centroids and cartograms. In the second option, the \code{crs} is only used in the plotting phase.

The automatic crs recommendation (which is still work-in-progress) is the following:\tabular{ll}{
   \strong{Property} \tab \strong{Recommendation} \cr
   \code{global} (for world maps) \tab A pseudocylindrical projection tmap option \code{crs_global}, by default \verb{"eqearth} (Equal Eearth). See \url{https://r-tmap.github.io/tmap/articles/41_advanced_crs.html} for more options \cr
   \code{area} (equal area) \tab Lambert Azimuthal Equal Area (\code{laea}) \cr
   \code{distance}	(equidistant) \tab Azimuthal Equidistant (\code{aeqd}) \cr
   \code{shape}	(conformal) \tab Stereographic (\code{stere}) \cr
}


For further info about the available "generic" projects see:
for utm: \url{https://proj.org/en/9.4/operations/projections/utm.html}
for laea: \url{https://proj.org/en/9.4/operations/projections/laea.html}
for aeqd: \url{https://proj.org/en/9.4/operations/projections/aeqd.html}
for pconic: \url{https://proj.org/en/9.4/operations/projections/pconic.html}
for eqdc: \url{https://proj.org/en/9.4/operations/projections/eqdc.html}
}
\note{
as of tmap 4.0, simplify has been removed. Please use \code{\link[tmaptools:simplify_shape]{tmaptools::simplify_shape()}} instead
}
\examples{
tm_shape(World, crs = "auto") +
	tm_polygons()

tm_shape(World, crs = 3035, bb = "Europe") +
	tm_polygons()

tm_shape(World, crs = "+proj=robin", filter = World$continent=="Africa") +
	tm_polygons()
}
\seealso{
\href{https://r-tmap.github.io/tmap/articles/foundations_crs}{vignette about CRS}
}
