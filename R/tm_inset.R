#' Map component: inset maps and other objects
#'
#' Map component that adds an inset object, e.g. a mini map
#'
#' @param x object to draw. Can be: bounding box, tmap object, ggplot2 object, grob object, image file name.
#' @param height height of the component in number of text line heights.
#' @param width width of the component in number of text line heights.
#' @param margins margins
#' @param between_margin Margin between
#' @param box_frame Should a box frame be drawn in the main map that shows where the inset is? `TRUE` by default
#' @param box_frame.color,box_frame.alpha,box_frame.lwd,box_frame.lty Properties of the box frame
#' @param box_bg Should the frame box have a background? `FALSE` by default
#' @param box_bg.color,box_bg.alpha Properties of the box background
#' @param main_frame Should a frame be drawn around the inset map? Note that this is different from the general map component frame (the argument `frame`)
#' @param main_frame.r,main_frame.color,main_frame.alpha,main_frame.lwd Properties of the main frame
#' @inheritParams tm_title
#' @example ./examples/tm_inset.R
#' @export
tm_inset = function(x = NULL,
					height,
					width,
					margins,
					between_margin,
					position,
					group_id,
					frame, frame.color, frame.alpha, frame.lwd, frame.r, bg, bg.color, bg.alpha,
					box_frame, box_frame.color, box_frame.alpha, box_frame.lwd, box_frame.lty, box_bg, box_bg.color, box_bg.alpha,
					main_frame, main_frame.r, main_frame.color, main_frame.alpha, main_frame.lwd,
					z) {
	args = lapply(as.list(rlang::call_match()[-1]), eval, envir = parent.frame())
	args$called = names(args)
	args$group_id = args$group_id %||% NA_character_
	args$group_type = c("tm_inset")
	args$z = args$z %||% NA_integer_

	cls = if (is.null(x) || (inherits(x, "bbox"))) {
		"map"
	} else if (inherits(x, "tmap")) {
		"tmap"
	} else if (inherits(x, "ggplot")) {
		"gg"
	} else if (inherits(x, c("grob", "gList"))) {
		"grob"
	} else if (is.character(x)) {
		"image"
	} else {
		stop("Unsupported object")
	}

	cls2 = paste0("tm_inset_", cls)

	tm_element_list(do.call(tm_element, c(args, list(subclass = c(cls2, "tm_inset", "tm_component")))))
}
