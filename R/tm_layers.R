#' Generic tmap layer function
#' 
#' @param ... the aesthetic arguments and how they are used. For each aesthetic, three arguments are needed: 1) the aesthetic itself, 2) the definition, and 3) the setup. The aesthetic itself (1) should have a self-chosen name should reflect what it does (e.g.  \code{col} and \code{lwd} for \code{tm_lines}). The value is either the direct value for the aesthetic (e.g. \code{"blue"} for \code{col}) or a data variable whose values are mapped to these direct values. The definition (2) describes what is visualized. The name should correspond to the name of (1) plus the postfix \code{.def}. For instance, \code{col.def} and \code{lwd.def} define the aesthetics line color and width for \code{tm_lines}. The setup (3) defines how data values are mapped to the direct values of each aesthetic. The name should correspond with the name of (1) plus the postfix \code{.setup}.
#' @param id id
#' @param zindex zindex
#' @param group group
tm_layer = function(...,
					geom,
					id = NA,
					zindex = NA,
					group = NA) {
	structure(c(as.list(environment()), list(...)), class = "tm_layer")
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
					   style = "pretty",
					   ...) {
	do.call("t_setup", as.list(environment()))
}
t_setup_alpha = function(...) {
	do.call("t_setup", as.list(environment()))
}
t_setup_size = function(scale=1,
						lwd.legend = NULL,
						lwd.legend.labels = NULL,
						lwd.legeld.col = NA,
						...) {
	do.call("t_setup", as.list(environment()))
}
t_setup_lty = function(...) {
	do.call("t_setup", as.list(environment()))
}




