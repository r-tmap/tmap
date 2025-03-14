% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tm_layout.R
\name{tm_plot}
\alias{tm_plot}
\title{Plot mode options}
\usage{
tm_plot(use_gradient, limit_latitude_3857)
}
\arguments{
\item{use_gradient}{Use gradient fill using \link[grid:patterns]{linearGradient()}}

\item{limit_latitude_3857}{Vector of two limit latitude values for maps printed in Web Mercator projection (EPSG 3857). If \code{c(-90, 90)} the poles will be inflated too much. The Web Mercator is defines as \code{c(-85.06, 85.06)}, but the default setting in tmap is \code{c(-84, 84)}.}
}
\description{
Plot mode options. This option is specific to the plot mode.
}
