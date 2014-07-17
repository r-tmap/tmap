\name{read_shape}
\alias{read_shape}
\title{Read shape file}
\usage{
read_shape(file)
}
\arguments{
  \item{file}{a shape file name (including directory).}
}
\value{
shape object
}
\description{
This function reads an ESRI shape file.
}
\details{
This function is a convenient wrapper of readOGR of the
package sp. It also fixes a common bug when reading shape
files that are in the Dutch rijksdriehoekstelsel.
}

