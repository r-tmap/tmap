#' Map component: title
#' 
#' Map component that adds a title
#' 
#' @param text text of the title
#' @param size font size of the title
#' @param color font color of the title
#' @param padding padding
#' @param fontface font face, bold, italic
#' @param fontfamily font family
#' @param stack stack
#' @param just just
#' @param frame frame
#' @param frame.lwd frame line width
#' @param frame.r frame.r
#' @param bg.color Background color
#' @param bg.alpha Background transparency
#' @param position position
#' @param width,height width and height of the title box.
#' @param group.frame group.frame
#' @param resize.as.group resize.as.group
#' @param z z
#' @export
tm_title = function(text, size, color, padding, fontface, fontfamily, stack, just, frame, frame.lwd, frame.r, bg.color, bg.alpha, position, width, height, group.frame, resize.as.group, z) {
	args = lapply(as.list(match.call()[-1]), eval, envir = parent.frame())
	if (!("z" %in% names(args))) args$z = as.integer(NA)
	tm_element_list(do.call(tm_element, c(args, list(subclass = c("tm_title", "tm_component")))))
}

#' @export
#' @param ... passed on to `tm_title()`
#' @rdname tm_title
tm_title_in = function(text, ..., position = tm_pos_in("left", "top")) {
	args = lapply(as.list(match.call()[-1]), eval, envir = parent.frame())
	if (!("z" %in% names(args))) args$z = as.integer(NA)
	args$position = position
	tm_element_list(do.call(tm_element, c(args, list(subclass = c("tm_title", "tm_component"
	)))))
}

#' @export
#' @rdname tm_title
#' @name tm_title_in
tm_title_out = function(text, ..., position = tm_pos_out("center", "top")) {
	args = lapply(as.list(match.call()[-1]), eval, envir = parent.frame())
	if (!("z" %in% names(args))) args$z = as.integer(NA)
	args$position = position
	tm_element_list(do.call(tm_element, c(args, list(subclass = c("tm_title", "tm_component"
	)))))
}

# tm_compass = function( x = 1) {
# 	tm_element_list(tm_element(as.list(environment()), 
# 							   subclass = c("tm_layer", "tm_compass")))
# }


#' Map component: (credits) text
#' 
#' Map component that adds a text, typically used as credits
#' 
#' @param text text of the title
#' @param size font size of the title
#' @param color color
#' @param padding padding
#' @param fontface font face 
#' @param fontfamily font family
#' @param stack stack
#' @param just just
#' @param frame frame
#' @param frame.lwd frame.lwd
#' @param frame.r frame.r
#' @param bg.color bg.color
#' @param bg.alpha bg.alpha
#' @param position position
#' @param width width
#' @param height height
#' @param group.frame group.frame
#' @param resize.as.group resize.as.group
#' @param z z
#' @export
tm_credits = function(text, size, color, padding, fontface, fontfamily, stack, just, frame, frame.lwd, frame.r, bg.color, bg.alpha, position, width, height, group.frame, resize.as.group, z) {
	args = lapply(as.list(match.call()[-1]), eval, envir = parent.frame())
	if (!("z" %in% names(args))) args$z = as.integer(NA)
	tm_element_list(do.call(tm_element, c(args, list(subclass = c("tm_credits", "tm_component")))))
}


#' Map component: compass
#' 
#' Map component that adds a compass
#' 
#' @param north north
#' @param type type
#' @param text.size text.size
#' @param size size
#' @param show.labels show.labels
#' @param cardinal.directions cardinal.directions
#' @param text.color text.color
#' @param color.dark color.dark
#' @param color.light color.light
#' @param lwd lwd
#' @param position position
#' @param bg.color bg.color
#' @param bg.alpha bg.alpha
#' @param stack stack
#' @param just just
#' @param frame frame
#' @param frame.lwd frame.lwd
#' @param frame.r frame.r
#' @param margins margins
#' @param z z
#' @export
tm_compass <- function(north, 
					   type, 
					   text.size,
					   size,
					   show.labels, 
					   cardinal.directions, 
					   text.color,
					   color.dark, 
					   color.light,
					   lwd,
					   position,
					   bg.color,
					   bg.alpha,
					   stack, 
					   just, 
					   frame,
					   frame.lwd,
					   frame.r,
					   margins,
					   z) {
	args = lapply(as.list(match.call()[-1]), eval, envir = parent.frame())
	if (!("z" %in% names(args))) args$z = as.integer(NA)
	tm_element_list(do.call(tm_element, c(args, list(subclass = c("tm_compass", "tm_component")))))
}

#' Map component: scale bar
#' 
#' Map component that adds a scale bar. As of version 4.0, `tm_scalebar()` is
#' used instead of `tm_scale_bar()` (now deprecated), because of the potential
#' confusion with the `tm_scale_*()` scaling functions (like [tm_scale_continuous()]).
#' 
#' @param breaks breaks
#' @param width width
#' @param text.size text.size
#' @param text.color text.color
#' @param color.dark color.dark
#' @param color.light color.light
#' @param lwd lwd
#' @param position position
#' @param bg.color bg.color
#' @param bg.alpha bg.alpha
#' @param size size
#' @param stack stack
#' @param frame frame
#' @param frame.lwd frame.lwd
#' @param frame.r frame.r
#' @param margins margins
#' @param z z
#' @export
#' @rdname tm_scalebar
#' @name tm_scalebar
tm_scalebar = function(breaks,
						width, 
						text.size,
						text.color,
						color.dark, 
						color.light,
						lwd,
						position,
						bg.color,
						bg.alpha,
						size = "deprecated",
						stack, 
						frame,
						frame.lwd,
						frame.r,
						margins,
						z) {
	
	if (!identical(size, "deprecated")) {
		message("The 'size' argument of 'tm_scalebar()' is deprecated as of tmap 4.0. Please use 'text.size' instead.", call. = FALSE)
	}

	args = lapply(as.list(match.call()[-1]), eval, envir = parent.frame())
	if (!("z" %in% names(args))) args$z = as.integer(NA)
	tm_element_list(do.call(tm_element, c(args, list(subclass = c("tm_scalebar", "tm_component")))))
}

#' Map component: scale bar
#' 
#' This function was renamed to [tm_scalebar()] in tmap v4.0
#' @inheritDotParams tm_scalebar
#' @export
tm_scale_bar = function(...) {
	message("As of version 4.0, 'tm_scale_bar()' is deprecated. Please use 'tm_scalebar()' instead.", call. = FALSE)
	tm_scalebar(...)
	# can also be
	# stop("As of version 4.0, tm_scale_bar has been renamed to tm_scalebar and is therefore deprecated. tm_scalebar also has new argument names.", call. = FALSE)
}

#' Map component: mouse coordinates
#' 
#' Map component that adds mouse coordinates
#' 
#' @param stack stack
#' @param position position
#' @param z z
#' @export
tm_mouse_coordinates <- function(stack, 
								 position,
								 z) {
	args = lapply(as.list(match.call()[-1]), eval, envir = parent.frame())
	if (!("z" %in% names(args))) args$z = as.integer(NA)
	tm_element_list(do.call(tm_element, c(args, list(subclass = c("tm_mouse_coordinates", "tm_component")))))
}

#' Map component: minimap
#' 
#' Map component that adds a minimap in view mode
#' 
#' @param server name of the provider or an URL (see \code{\link{tm_tiles}}). By default, it shows the same map as the basemap, and moreover, it will automatically change when the user switches basemaps. Note the latter does not happen when \code{server} is specified.
#' @param toggle should the minimap have a button to minimise it? By default \code{TRUE}.
#' @param position position of the scale bar Vector of two values, specifying the x and y coordinates. The first is either "left" or "right", the second either "top" or "bottom".
#' @param stack stack
#' @param position position
#' @param z z
#' @param ... arguments passed on to \code{\link[leaflet:addMiniMap]{addMiniMap}}.
#' @seealso \code{\link[leaflet:addMiniMap]{addMiniMap}}
#' @export
tm_minimap <- function(server, 
					   toggle, 
					   stack,
					   position,
					   z,
					   ...) {
	args = lapply(as.list(match.call()[-1]), eval, envir = parent.frame())
	if (!("z" %in% names(args))) args$z = as.integer(NA)
	tm_element_list(do.call(tm_element, c(args, list(subclass = c("tm_minimap", "tm_component")))))
}


#' Map component: logo
#' 
#' Map component that adds a scale bar. As of version 4.0, `tm_scalebar()` is
#' used instead of `tm_scale_bar()` (now deprecated), because of the potential
#' confusion with the `tm_scale_*()` scaling functions (like [tm_scale_continuous()]).
#' 
#' @param file either a filename or url of a png image. If multiple files/urls are provided with a character vector, the logos are placed near each other. To specify logos for small multiples use a list of character values/vectors. In order to stack logos vertically, multiple \code{tm_logo} elements can be stacked.
#' @param height height of the logo in number of text line heights. The width is scaled based the height and the aspect ratio of the logo. If multiple logos are specified by a vector or list, the heights can be specified accordingly.
#' @param margins margins
#' @param between.margin between.margin
#' @param stack stack
#' @param position position
#' @param frame frame
#' @param frame.lwd frame.lwd
#' @param frame.r frame.r
#' @param group.frame group.frame
#' @param resize.as.group resize.as.group
#' @param z z
#' @example ./examples/tm_logo.R 
#' @export
tm_logo = function(file,
				   height,
				   margins,
				   between.margin,
				   stack,
				   position,
				   frame,
				   frame.lwd,
				   frame.r,
				   group.frame,
				   resize.as.group,
				   z) {
	args = lapply(as.list(match.call()[-1]), eval, envir = parent.frame())
	if (!("z" %in% names(args))) args$z = as.integer(NA)
	tm_element_list(do.call(tm_element, c(args, list(subclass = c("tm_logo", "tm_component")))))
}
