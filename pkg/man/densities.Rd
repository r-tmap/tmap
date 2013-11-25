\name{densities}
\alias{densities}
\title{Calculate densities}
\usage{
  densities(shp, var, total.area.km2 = 33893)
}
\arguments{
  \item{shp}{a shape object}

  \item{var}{name(s) of a qualtity variable name contained
  in the \code{map} data}

  \item{total.area.km2}{total area size of NL in km2.
  According to Wikipedia, this is 33893.}
}
\value{
  vector or data.frame (depending on whether
  \code{length(var)==1} with density values. This can be
  appended directly to the shape file.
}
\description{
  This function transposes quantitiy variables to densitiy
  variables, which are needed for choroplets.
}
\keyword{densities}

