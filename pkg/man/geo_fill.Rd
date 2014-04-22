\name{geo_fill}
\alias{geo_fill}
\title{Fill polygons}
\usage{
geo_fill(col = "gray75")
}
\arguments{
  \item{col}{a single color value, or a vector of colors
  (specifying a color per polygon).}
}
\value{
\code{\link{geo-object}}
}
\description{
This layer defines the fill colors of the polygons. The
colors in this layer are directly specified. Use
\code{\link{geo_choropleth}} to map colors to a data
variable.
}
\seealso{
\code{\link{geo_choropleth}}
}

