#' Generic tmap layer function
#' 
#' @param ... the aesthetic arguments and how they are used. For each aesthetic, three arguments are needed: 1) the aesthetic itself, 2) the definition, and 3) the setup. The aesthetic itself (1) should have a self-chosen name should reflect what it does (e.g.  \code{col} and \code{lwd} for \code{tm_lines}). The value is either the direct value for the aesthetic (e.g. \code{"blue"} for \code{col}) or a data variable whose values are mapped to these direct values. The definition (2) describes what is visualized. The name should correspond to the name of (1) plus the postfix \code{.def}. For instance, \code{col.def} and \code{lwd.def} define the aesthetics line color and width for \code{tm_lines}. The setup (3) defines how data values are mapped to the direct values of each aesthetic. The name should correspond with the name of (1) plus the postfix \code{.setup}.
#' @param id name of the data variable that specifies the indices of the geometries. Only used for \code{"view"} mode (see \code{\link{tmap_mode}}).
#' @param interactive logical that determines whether this layer is interactive in view mode (e.g. hover text, popup, and click event in shiny apps)
#' @param popup.vars names of data variables that are shown in the popups in \code{"view"} mode.
#' @param popup.format list of formatting options for the popup values.
#' @param zindex zindex of the pane in view mode. By default, it is set to the layer number plus 400. By default, the tmap layers will therefore be placed in the custom panes \code{"tmap401"}, \code{"tmap402"}, etc., except for the base tile layers, which are placed in the standard \code{"tile"}. This parameter determines both the name of the pane and the z-index, which determines the pane order from bottom to top. For instance, if \code{zindex} is set to 500, the pane will be named \code{"tmap500"}.
#' @param group name of the group to which this layer belongs in view mode. Each group can be selected or deselected in the layer control item. Set \code{group = NULL} to hide the layer in the layer control item. By default, it will be set to the name of the shape (specified in \code{\link{tm_shape}}).
tm_layer = function(...,
					id = NA,
					interactive = TRUE,
					popup.vars = NA,
					popup.format = list(),
					zindex = NA,
					group = NA) {
	structure(c(as.list(environment()), list(...)), class = "tm_layer")
}



t_aes_def = function(class,
					 part,
					 property) {
	structure(list(class = class, part = part, property = property), class = "t_aes_def")
}



t_setup = function(...,
				   title = NA,
				   legend.show = TRUE,
				   legend.format = list(),
				   legend.is.portrait = TRUE,
				   legend.reverse = FALSE) {
	structure(c(list(title = title, legend.show = legend.show, legend.format = legend.format, legend.is.portrait = legend.is.portrait, legend.reverse = legend.reverse), list(...)), class = "t_setup")
}


t_setup_col = function(n = 5,
					   style = ifelse(is.null(breaks), "pretty", "fixed"),
					   style.args = list(),
					   as.count = NA,
					   breaks = NULL,
					   interval.closure = "left",
					   palette = NULL,
					   labels = NULL,
					   drop.levels = FALSE,
					   midpoint = NULL,
					   stretch.palette = TRUE,
					   contrast = NA,
					   colorNA = NA,
					   textNA = "Missing",
					   showNA = NA,
					   colorNULL = NA,
					   ...) {
	do.call("t_setup", as.list(environment()))
}

# to be implemented
t_setup_alpha = function(...) {
	do.call("t_setup", as.list(environment()))
}

# to be implemented
t_setup_size = function(scale=1,
						lwd.legend = NULL,
						lwd.legend.labels = NULL,
						lwd.legeld.col = NA,
	
	
	
	...) {
	do.call("t_setup", as.list(environment()))
}

# to be implemented
t_setup_lty = function(...) {
	do.call("t_setup", as.list(environment()))
}



tm_polygons = function(col = NA,
					   alpha = NA,
					   border.col = NA,
					   border.alpha = NA,
					   border.lwd = NA,
					   border.lty = NA,
					   
					   col.setup = t_setup_col(), 
					   alpha.setup = t_setup_alpha(),
					   border.col.setup = t_setup_col(),
					   border.alpha.setup = t_setup_alpha(),
					   border.lwd.setup = t_setup_lwd(),
					   border.lty.setup = t_setup_lty(),
					   ...) {
	
	
	do.call("tm_layer", c(as.list(environment()), list(
		col.def = t_aes_def("polygon", "area", "color"),
		alpha.def = t_aes_def("polygon", "area", "alpha"),
		border.col.def = t_aes_def("polygon", "line", "color"),
		border.alpha.def = t_aes_def("polygon", "line", "alpha"),
		border.lwd.def = t_aes_def("polygon", "line", "lwd"),
		border.lty.def = t_aes_def("polygon", "line", "lty")
	), list(...)))
}

tm_symbols = function(size = NA,
					  col = NA,
					  shape = NA,
					  alpha = NA,
					  
					  border.col = NA,
					  border.alpha = NA,
					  border.lwd = NA,

					  size.setup = t_setup_size(), 
					  
					  alpha.setup = t_setup_alpha(),
					  border.col.setup = t_setup_col(),
					  border.alpha.setup = t_setup_alpha(),
					  border.lwd.setup = t_setup_lwd(),
					  border.lty.setup = t_setup_lty(),
					  )

tm_lines = function(col = NA,
					alpha = NA,
					lwd = NA,
					lty = NA,
					
					col.setup = t_setup_col(),
					alpha.setup = t_setup_alpha(),
					lwd.setup = t_setup_size(),
					lty.setup = t_setup_lty(),
					...) {
	
	
	do.call("tm_layer", c(as.list(environment()), list(
		col.def = t_aes_def("line", "line", "color"),
		alpha.def = t_aes_def("line", "line", "alpha"),
		lwd.def = t_aes_def("line", "line", "lwd"),
		lty.def = t_aes_def("line", "line", "lty")
	), list(...)))
}

