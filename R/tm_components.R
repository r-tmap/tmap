#' Map component: title
#' 
#' Map component that adds a title
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
tm_title = function(text, size, color, padding, fontface, fontfamily, stack, just, frame, frame.lwd, frame.r, bg.color, bg.alpha, position, width, height, group.frame, resize.as.group, z) {
	args = lapply(as.list(match.call()[-1]), eval, envir = parent.frame())
	if (!("z" %in% names(args))) args$z = as.integer(NA)
	tm_element_list(do.call(tm_element, c(args, list(subclass = c("tm_title", "tm_component")))))
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
#' Map component that adds a scale bar
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
#' @param just just
#' @param frame frame
#' @param frame.lwd frame.lwd
#' @param frame.r frame.r
#' @param margins margins
#' @param z z
#' @export
tm_scale_bar = function(breaks,
						width, 
						text.size,
						text.color,
						color.dark, 
						color.light,
						lwd,
						position,
						bg.color,
						bg.alpha,
						size,
						stack, 
						just, 
						frame,
						frame.lwd,
						frame.r,
						margins,
						z) {
	args = lapply(as.list(match.call()[-1]), eval, envir = parent.frame())
	if (!("z" %in% names(args))) args$z = as.integer(NA)
	tm_element_list(do.call(tm_element, c(args, list(subclass = c("tm_scale_bar", "tm_component")))))
}

#' Map component: mouse coordinates
#' 
#' Map component that adds mouse coordinates
#' 
#' @param stack stack
#' @param position
#' @param z z
#' @export
tm_mouse_coordinates <- function(stack, 
								 position,
								 z) {
	args = lapply(as.list(match.call()[-1]), eval, envir = parent.frame())
	if (!("z" %in% names(args))) args$z = as.integer(NA)
	tm_element_list(do.call(tm_element, c(args, list(subclass = c("tm_mouse_coordinates", "tm_component")))))
}

 