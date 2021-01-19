% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tm_layers.R
\name{tm_iso}
\alias{tm_iso}
\title{Draw iso (contour) lines with labels}
\usage{
tm_iso(
  col = NA,
  text = "level",
  size = 0.5,
  remove.overlap = TRUE,
  along.lines = TRUE,
  overwrite.lines = TRUE,
  bg.color = tmap_options()$bg.color,
  group = NA,
  ...
)
}
\arguments{
\item{col}{line color. See \code{\link{tm_lines}}.}

\item{text}{text to display.}

\item{size}{text size (see \code{\link{tm_text}})}

\item{remove.overlap}{see \code{\link{tm_text}}}

\item{along.lines}{see \code{\link{tm_text}}}

\item{overwrite.lines}{see \code{\link{tm_text}}}

\item{bg.color}{background color of the labels. Note: in tmap <= 3.2, the iso lines were cut to make space for labels. In tmap >= 3.3, this is changed: the iso lines remain unchanged, but the labels are printed with a background color by default.}

\item{group}{name of the group to which this layer belongs in view mode. Each group can be selected or deselected in the layer control item. Set \code{group = NULL} to hide the layer in the layer control item. By default, it will be set to the name of the shape (specified in \code{\link{tm_shape}}).}

\item{...}{arguments passed on to \code{\link{tm_lines}} or \code{\link{tm_text}}}
}
\description{
This function is a wrapper of \code{\link{tm_lines}} and \code{\link{tm_text}} aimed to draw isopleths.
}