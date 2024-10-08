% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tm_layers_sf.R
\name{tm_sf}
\alias{tm_sf}
\title{Map layer: simple features}
\usage{
tm_sf(
  fill = tm_const(),
  fill.scale = tm_scale(),
  fill.legend = tm_legend(),
  fill.free = NA,
  col = tm_const(),
  col.scale = tm_scale(),
  col.legend = tm_legend(),
  col.free = NA,
  size = tm_const(),
  size.scale = tm_scale(),
  size.legend = tm_legend(),
  size.free = NA,
  shape = tm_const(),
  shape.scale = tm_scale(),
  shape.legend = tm_legend(),
  shape.free = NA,
  lwd = tm_const(),
  lwd.scale = tm_scale(),
  lwd.legend = tm_legend(),
  lwd.free = NA,
  lty = tm_const(),
  lty.scale = tm_scale(),
  lty.legend = tm_legend(),
  lty.free = NA,
  fill_alpha = tm_const(),
  fill_alpha.scale = tm_scale(),
  fill_alpha.legend = tm_legend(),
  fill_alpha.free = NA,
  col_alpha = tm_const(),
  col_alpha.scale = tm_scale(),
  col_alpha.legend = tm_legend(),
  col_alpha.free = NA,
  linejoin = "round",
  lineend = "round",
  plot.order.list = list(polygons = tm_plot_order("AREA"), lines =
    tm_plot_order("LENGTH"), points = tm_plot_order("size")),
  options = opt_tm_sf(),
  zindex = NA,
  group = NA,
  group.control = "check",
  ...
)
}
\arguments{
\item{fill, fill.scale, fill.legend, fill.free}{Visual variable that determines
the fill color. See details.}

\item{col, col.scale, col.legend, col.free}{Visual variable that determines
the col color. See details.}

\item{size, size.scale, size.legend, size.free}{Visual variable that determines
the size. See details.}

\item{shape, shape.scale, shape.legend, shape.free}{Visual variable that determines
the shape. See details.}

\item{lwd, lwd.scale, lwd.legend, lwd.free}{Visual variable that determines
the line width. See details.}

\item{lty, lty.scale, lty.legend, lty.free}{Visual variable that determines
the line type. See details.}

\item{fill_alpha, fill_alpha.scale, fill_alpha.legend, fill_alpha.free}{Visual
variable that determines the fill color alpha transparency See details.}

\item{col_alpha, col_alpha.scale, col_alpha.legend, col_alpha.free}{Visual variable
that determines the border color alpha transparency. See details.}

\item{linejoin, lineend}{line join and line end. See \link[grid:gpar]{gpar()} for details.}

\item{plot.order.list}{Specification in which order the spatial features are drawn.
This consists of a list of three elementary geometry types: for polygons, lines and, points.
For each of these types, which are drawn in that order, a \code{\link[=tm_plot_order]{tm_plot_order()}} is required.}

\item{options}{options passed on to the corresponding \verb{opt_<layer_function>} function}

\item{zindex}{Map layers are drawn on top of each other. The \code{zindex} numbers
(one for each map layer) determines the stacking order.
By default the map layers are drawn in the order they are called.}

\item{group}{Name of the group to which this layer belongs. This is only
relevant in view mode, where layer groups can be switched (see \code{group.control})}

\item{group.control}{In view mode, the group control determines how layer groups
can be switched on and off. Options: \code{"radio"} for radio buttons
(meaning only one group can be shown), \code{"check"} for check boxes
(so multiple groups can be shown), and \code{"none"} for no control
(the group cannot be (de)selected).}

\item{...}{passed on to \code{\link[=tm_polygons]{tm_polygons()}}, \code{\link[=tm_lines]{tm_lines()}}, and \code{\link[=tm_dots]{tm_dots()}}}
}
\description{
Map layer that draws simple features as they are. Supported visual variables
are: \code{fill} (the fill color), \code{col} (the border color), \code{size} the point size,
\code{shape} the symbol shape, \code{lwd} (line width), \code{lty} (line type), \code{fill_alpha} (fill color alpha transparency)
and \code{col_alpha} (border color alpha transparency).

The visual variable arguments (e.g. \code{col}) can be specified with either a data
variable name (of the object specified in \code{\link[=tm_shape]{tm_shape()}}), or with a visual value
(for \code{col}, a color is expected). Multiple values can be specified: in that case
facets are created. These facets can be combined with other faceting data variables,
specified with \code{\link[=tm_facets]{tm_facets()}}.
}
\details{
The \code{.scale} arguments determine the used scale to map the data values to visual
variable values. These can be specified with one of the available \code{tm_scale_()} functions.
The default scale that is used is specified by the tmap option \code{scales.var}.

The \code{.legend} arguments determine the used legend, specified with \code{\link[=tm_legend]{tm_legend()}}.
The default legend and its settings are determined by the tmap options \code{legend.}.

The \code{.free} arguments determine whether scales are applied freely across facets,
or shared. A logical value is required. They can also be specified with a
vector of three logical values; these determine whether scales are applied freely
per facet dimension. This is only useful when facets are applied (see \code{\link[=tm_facets]{tm_facets()}}).
There are maximally three facet dimensions: rows, columns, and pages. This only applies
for a facet grid (\code{\link[=tm_facets_grid]{tm_facets_grid()}}). For instance, \code{col.free = c(TRUE, FALSE, FALSE)} means
that for the visual variable \code{col}, each row of facets will have its own scale,
and therefore its own legend. For facet wraps and stacks (\code{\link[=tm_facets_wrap]{tm_facets_wrap()}} and
\code{\link[=tm_facets_stack]{tm_facets_stack()}}) there is only one facet dimension, so the \code{.free} argument requires
only one logical value.
}
\examples{
data(World)

World$geometry[World$continent == "Africa"] <- 
  sf::st_centroid(World$geometry[World$continent == "Africa"])
World$geometry[World$continent == "South America"] <- 
  sf::st_cast(World$geometry[World$continent == "South America"], 
    "MULTILINESTRING", group_or_split = FALSE)

tm_shape(World, crs = "+proj=robin") + 
	tm_sf()
}
