% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tm_add_legend.R
\name{tm_add_legend}
\alias{tm_add_legend}
\title{Map component: manual legend}
\usage{
tm_add_legend(
  ...,
  labels = "",
  type = "symbols",
  title = "",
  orientation = NULL,
  position = NULL,
  group_id = NA_character_,
  group = NA,
  group.control = "check",
  z = NA_integer_
)
}
\arguments{
\item{...}{visual variables and arguments passed on to \code{tm_legend()}.
By default, the argument \code{type} is set to \code{"symbols"}, which means that the
supported visual variables are: \code{"fill"}, \code{"col"}, \code{"shape"}, \code{"size"},
\code{"fill_alpha"}, \code{"col_alpha"}, \code{"lty"}, \code{"lwd"}, \code{"linejoin"}, and \code{"lineend"}.
The number of legend items will be equal to the maximum number of specific values (and specified labels.)}

\item{labels}{labels by default \code{""} (so omitted)}

\item{type}{the layer type from which the visual variables (see \code{...}) are taken.
Options: \code{"symbols"} (default), \code{"lines"}, \code{"polygons"}, and \code{"text"}.}

\item{title}{The title of the legend.}

\item{orientation}{The orientation of the legend.}

\item{position}{The position of the legend. A tm_pos object, or a shortcut of two values: horizontal (left, center, right) and vertical (top, center, bottom). See tm_pos for details}

\item{group_id}{Component group id name. All components (e.g. legends, titles, etc) with the same \code{group_id} will be grouped. The specifications of how they are placed (e.g. stacking, margins etc.) are determined in \code{\link[=tm_components]{tm_components()}} where its argument \code{id} should correspond to \code{group_id}.}

\item{group}{Name of the group to which this layer belongs. This is only
relevant in view mode, where layer groups can be switched (see \code{group.control})}

\item{group.control}{In view mode, the group control determines how
layer groups can be switched on and off. Options: \code{"radio"} for radio
buttons (meaning only one group can be shown), \code{"check"} for check boxes
(so multiple groups can be shown), and \code{"none"} for no control
(the group cannot be (de)selected).}

\item{z}{z index, e.g. the place of the component relative to the other componets}
}
\description{
Map component that adds a manual legend.
}
\examples{
\dontrun{
tm_shape(NLD_muni) +
	tm_borders() +
	tm_basemap("OpenStreetMap") +
	tm_add_legend(labels = c("Motorway", "Primary road", "Secondary road", "Railway"),
				  col = c("#E892A1", "#FCD6A4", "#F8FABF", "#707070"),
				  lty = c("solid", "solid", "solid", "dotted"),
				  lwd = 3,
				  type = "lines",
				  bg.color = "grey92",
				  bg.alpha = 1)
}
}
