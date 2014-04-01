\name{geo_theme}
\alias{geo_theme}
\alias{geo_theme_World}
\title{Theme elements of cartographic maps}
\usage{
geo_theme(title = NULL, title.cex = 1.5, bg.color = NA,
  draw.frame = FALSE, crop = draw.frame, show.legend.text = NULL,
  type.legend.plot = NULL, legend.position = c("left", "top"),
  legend.plot.size = NA, legend.in.frame = TRUE, legend.cex = 0.8,
  legend.digits = 2, title.position = c("left", "top"), margins = NA,
  frame.lwd = 1, legend.only = FALSE)

geo_theme_World(bg.color = "lightblue1", draw.frame = TRUE, crop = TRUE,
  legend.in.frame = TRUE, legend.position = c("left", "bottom"),
  legend.plot.size = c(0.2, 0.2), legend.cex = 0.6, ...)
}
\arguments{
  \item{title}{Title of the map(s)}

  \item{title.cex}{Relative size of the title}

  \item{bg.color}{Background color}

  \item{draw.frame}{Boolean that determines whether a frama
  is drawn.}

  \item{crop}{boolean that determines whether the shape
  objects are cropped at the bounding box (see
  \code{\link[sp:bbox]{bbox}})}

  \item{show.legend.text}{Boolean that determines if the
  legend text is shwon.}

  \item{type.legend.plot}{Type of legend plot. One of
  "hist", "bar", "bubble", "none".}

  \item{legend.position}{Position of the legend. Vector of
  two values, specifing the x and y coordinates. Either
  this vector contains "left", "center" or "right" for the
  first value and "top", "center", or "right" for the
  second value, or this vector contains two numeric values
  between 0 and 1 that specifies the x and y value of the
  left bottom corner of the legend.}

  \item{legend.plot.size}{Relative size of the legend.
  Vector of two numeric values between 0 and 1 that specify
  the height and width of the legend.}

  \item{legend.in.frame}{boolean that determines whether
  the legend in drawn inside the frame}

  \item{legend.cex}{Relative font size for the legend}

  \item{legend.digits}{Number of digits for the legend
  labels}

  \item{title.position}{Position of the title. Vector of
  two values, specifing the x and y coordinates. Either
  this vector contains "left", "center" or "right" for the
  first value and "top", "center", or "right" for the
  second value, or this vector contains two numeric values
  between 0 and 1 that specifies the x and y value of the
  left bottom corner of the legend.}

  \item{margins}{Relative margins. Vector of four values
  specifying the left, top, right and bottom margin. Values
  are between 0 and 1.}

  \item{frame.lwd}{Width of the frame}

  \item{legend.only}{Boolean that determines whether only
  the legend is shown}

  \item{...}{other arguments from \code{geo_theme}}
}
\description{
This layer specifies thematic layout options for the maps.
}

