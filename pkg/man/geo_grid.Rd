\name{geo_grid}
\alias{geo_grid}
\title{Small multiples grid}
\usage{
geo_grid(ncol = NULL, nrow = NULL, free.scales = FALSE)
}
\arguments{
  \item{ncol}{number of columns of the small multiples
  grid}

  \item{nrow}{number of rows of the small multiples grid}

  \item{free.scales}{should the scales of the plotted data
  variables be free, i.e. independent of each other?
  Possible data variables are color from
  \code{\link{geo_choropleth}} and color and size from
  \code{\link{geo_bubblemap}}.}
}
\value{
\code{\link{geo-object}}
}
\description{
This layer specifies how the different plots are placed in
a grid. The number of rows and columns can be specified
here, as well as whether the scales are free (i.e.
independent of each other).
}

