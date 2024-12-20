% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tm_components.R
\name{tm_logo}
\alias{tm_logo}
\title{Map component: logo}
\usage{
tm_logo(
  file,
  height,
  margins,
  between_margin,
  stack,
  position,
  frame,
  frame.lwd,
  frame.r,
  group.frame,
  resize_as_group,
  z
)
}
\arguments{
\item{file}{either a filename or url of a png image. If multiple files/urls are provided with a character vector, the logos are placed near each other.
To specify logos for small multiples use a list of character values/vectors. In order to stack logos vertically, multiple \code{tm_logo} elements can be stacked.}

\item{height}{height of the logo in number of text line heights. The width is scaled based the height and the aspect ratio of the logo.
If multiple logos are specified by a vector or list, the heights can be specified accordingly.}

\item{margins}{margins}

\item{between_margin}{Margin between}

\item{stack}{stack}

\item{position}{Vector of two values, specifying the x and y coordinates. The first is either \code{"left"} or \code{"right"}, the second either \code{"top"} or \code{"bottom"}.}

\item{frame}{frame}

\item{frame.lwd}{frame line width}

\item{frame.r}{Radius of the rounded frame corners. 0 means no rounding.}

\item{group.frame}{group.frame}

\item{resize_as_group}{resize_as_group}

\item{z}{z}
}
\description{
Map component that adds a logo.
}
\examples{
data(World)

tm_shape(World) +
	tm_polygons("HPI", fill.scale = tm_scale_intervals(values = "RdYlGn")) +
	tm_logo(c("https://www.r-project.org/logo/Rlogo.png", 
			  system.file("img/tmap.png", package="tmap"))) +
	tm_logo("http://blog.kulikulifoods.com/wp-content/uploads/2014/10/logo.png", 
			height=5, position = c("left", "top")) +
	tm_format("World")
}
