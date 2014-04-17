\name{geo_theme}
\alias{geo_theme}
\alias{geo_theme_Europe}
\alias{geo_theme_NLD}
\alias{geo_theme_World}
\title{Theme elements of cartographic maps}
\usage{
geo_theme(title = NA, title.cex = 1, bg.color = "grey85",
  draw.frame = FALSE, crop = draw.frame, title.position = c("left",
  "top"), margins = rep(0, 4), frame.lwd = 1, frame.margins = rep(0.02,
  4), legend.profile = "full", legend.only = FALSE,
  legend.choro.title = NA, legend.bubble.size.title = NA,
  legend.bubble.col.title = NA, legend.position = c("left", "top"),
  legend.width = 0.2, legend.height = 0.7, legend.choro.height = 0.2,
  legend.choro.hist.height = 0.2, legend.bubble.size.height = 0.15,
  legend.bubble.col.height = 0.2, legend.config = c("hist", "choro",
  "bubble.size", "bubble.col"), legend.in.frame = TRUE,
  legend.title.cex = 1, legend.text.cex = 0.6, legend.hist.cex = 0.6,
  legend.digits = 2L, legend.bg.color = NA)

geo_theme_World(title = NA, title.cex = 1, draw.frame = TRUE,
  crop = TRUE, title.position = c("left", "bottom"), margins = rep(0.02,
  4), frame.margins = c(0, 0.02, 0.02, 0.02), legend.position = c("left",
  "bottom"), legend.width = 0.2, legend.height = 0.5,
  legend.bg.color = "grey85", ...)

geo_theme_Europe(title = NA, draw.frame = TRUE,
  legend.position = c("left", "top"), legend.width = 0.25,
  margins = rep(0.02, 4), frame.margins = c(0, 0.2, 0, 0), ...)

geo_theme_NLD(title = NA, draw.frame = FALSE, legend.in.frame = FALSE,
  legend.position = c("left", "top"), legend.width = 0.3, ...)
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

  \item{title.position}{Position of the title. Vector of
  two values, specifing the x and y coordinates. Either
  this vector contains "left", "center" or "right" for the
  first value and "top", "center", or "right" for the
  second value, or this vector contains two numeric values
  between 0 and 1 that specifies the x and y value of the
  left bottom corner of the legend.}

  \item{margins}{Relative margins. Vector of four values
  specifying the bottom, left, top, and right margin.
  Values are between 0 and 1.}

  \item{frame.lwd}{Width of the frame}

  \item{frame.margins}{Frame margins}

  \item{legend.profile}{Character that specifies which
  legend elements are drawn (if applicable): \describe{
  \item{\code{"full"}:}{All of them. (Which are: choropleth
  text, choropleth histogram, bubble size text, and bubble
  color text.)} \item{\code{"text"}:}{Only the choropleth
  text, bubble size text, and bubble color text.}
  \item{\code{"hist"}:}{Only the choropleth histogram.}
  \item{\code{"none"}:}{None of them.}} Alternatively,
  \code{legend.config} can be used to specify the elements
  directly.}

  \item{legend.only}{logical. Only draw the legend (without
  map)? Particularly useful for small multiples with a
  common legend.}

  \item{legend.choro.title}{title of the choropleth legend}

  \item{legend.bubble.size.title}{title of the bubblemap
  legend associated with the size of the bubbles}

  \item{legend.bubble.col.title}{title of the bubblemap
  legend associated with the color of the bubbles}

  \item{legend.position}{Position of the legend. Vector of
  two values, specifing the x and y coordinates. Either
  this vector contains "left", "center" or "right" for the
  first value and "top", "center", or "right" for the
  second value, or this vector contains two numeric values
  between 0 and 1 that specifies the x and y value of the
  left bottom corner of the legend.}

  \item{legend.width}{width of the legend}

  \item{legend.height}{total maximum height of the legend.
  Heights of single legend elements are specified by
  \code{legend.choro.height},
  \code{legend.choro.hist.height},
  \code{legend.bubble.size.height}, and
  \code{legend.bubble.col.height}. If their total exceeds
  \code{legend.height}, then there are downscaled
  linearly.}

  \item{legend.choro.height}{see \code{legend.height}}

  \item{legend.choro.hist.height}{see \code{legend.height}}

  \item{legend.bubble.size.height}{see
  \code{legend.height}}

  \item{legend.bubble.col.height}{see \code{legend.height}}

  \item{legend.config}{character vector that specifies
  which legend elements are drawn and at what position. The
  legend elements are called \code{"choro"}, \code{"hist"},
  \code{"bubble.size"}, and \code{"bubble.col"}. The
  \code{legend.config} vector should only contain these
  elements (it can also be a subset). The order corresponds
  to the order in which the legend elements are stacked
  from top to bottom.}

  \item{legend.in.frame}{logical. Should the legend be
  drawn inside the frame?}

  \item{legend.title.cex}{Relative font size for the legend
  title}

  \item{legend.text.cex}{Relative font size for the legend
  text elements}

  \item{legend.hist.cex}{Relative font size for the
  choropleth histogram}

  \item{legend.digits}{Number of digits for the legend
  labels}

  \item{legend.bg.color}{Background color of the legend.}

  \item{...}{other arguments from \code{geo_theme}}
}
\description{
This layer specifies thematic layout options for the maps.
}

