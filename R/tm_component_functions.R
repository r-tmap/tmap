#' Map component: title
#'
#' Map component that adds a title
#'
#' @param text text
#' @param size font size
#' @param color font color
#' @param padding padding
#' @param fontface font face, bold, italic
#' @param fontfamily font family
#' @param alpha alpha transparency of the text
#' @param stack stack with other map components, either `"vertical"` or `"horizontal"`.
#' @param just just
#' @param frame frame should a frame be drawn?
#' @param frame.color frame color
#' @param frame.alpha frame alpha transparancy
#' @param frame.lwd frame line width
#' @param frame.r Radius of the rounded frame corners. 0 means no rounding.
#' @param bg Show background?
#' @param bg.color Background color
#' @param bg.alpha Background transparency
#' @param position The position specification of the component: an object created with `tm_pos_in()` or `tm_pos_out()`. Or, as a shortcut, a vector of two values, specifying the x and y coordinates. The first is `"left"`, `"center"` or `"right"` (or upper case, meaning tighter to the map frame), the second `"top"`, `"center"` or `"bottom"`. Numeric values are also supported, where 0, 0 means left bottom and 1, 1 right top. See also \href{https://r-tmap.github.io/tmap/articles/adv_positions}{vignette about positioning}. In case multiple components should be combined (stacked), use `group_id` and specify `component` in [tm_components()].
#' @param group_id Component group id name. All components (e.g. legends, titles, etc) with the same `group_id` will be grouped. The specifications of how they are placed (e.g. stacking, margins etc.) are determined in [tm_components()] where its argument `id` should correspond to `group_id`.
#' @param width,height width and height of the component.
#' @param z z index, e.g. the place of the component relative to the other componets
#' @seealso \href{https://r-tmap.github.io/tmap/articles/basics_components}{Vignette about components}
#' @export
tm_title = function(text, size, color, padding, fontface, fontfamily, alpha, stack, just, frame, frame.color, frame.alpha, frame.lwd, frame.r, bg, bg.color, bg.alpha, position, group_id, width, height, z) {
	args = lapply(as.list(rlang::call_match()[-1]), eval, envir = parent.frame())
	args$called = names(args)
	args$group_id = args$group_id %||% NA_character_
	args$group_type = c("tm_<other>", "tm_title")
	args$z = args$z %||% NA_integer_
	tm_element_list(do.call(tm_element, c(args, list(subclass = c("tm_title", "tm_component")))))
}

#' @export
#' @param ... passed on to `tm_title()`
#' @rdname tm_title
tm_title_in = function(text, ..., position = tm_pos_in("left", "top")) {
	args = lapply(as.list(rlang::call_match()[-1]), eval, envir = parent.frame())
	args$called = names(args)
	args$group_id = args$group_id %||% NA_character_
	args$group_type = c("tm_<other>", "tm_title_in")
	args$z = args$z %||% NA_integer_
	args$position = position
	tm_element_list(do.call(tm_element, c(args, list(subclass = c("tm_title", "tm_component"
	)))))
}

#' @export
#' @rdname tm_title
tm_title_out = function(text, ..., position = tm_pos_out("center", "top")) {
	args = lapply(as.list(rlang::call_match()[-1]), eval, envir = parent.frame())
	args$called = names(args)
	args$group_id = args$group_id %||% NA_character_
	args$group_type = c("tm_<other>", "tm_title_out")
	args$z = args$z %||% NA_integer_
	args$position = position
	tm_element_list(do.call(tm_element, c(args, list(subclass = c("tm_title", "tm_component"
	)))))
}

#' Map component: (credits) text
#'
#' Map component that adds a text, typically used as credits. This function is the same as [tm_title()] but with different default values.
#'
#' @inheritParams tm_title
#' @param ... to catch deprecated arguments
#' @seealso \href{https://r-tmap.github.io/tmap/articles/basics_components}{Vignette about components}
#' @export
tm_credits = function(text, size, color, padding, fontface, fontfamily, alpha, stack, just, frame, frame.lwd, frame.r, bg, bg.color, bg.alpha, position, group_id, width, height, z, ...) {
	args = lapply(as.list(rlang::call_match(dots_expand = TRUE)[-1]), eval, envir = parent.frame())
	args$called = names(args)
	args$group_id = args$group_id %||% NA_character_
	args$group_type = c("tm_<other>", "tm_credits")
	args$z = args$z %||% NA_integer_
	if ("align" %in% names(args)) {
		args$position = tm_pos_in(pos.h = "right", pos.v = "bottom", align.h = args$align, align.v = "top", just.h = "left", just.v = "bottom")
		args$align = NULL
		v3_instead_message("align", "position = tm_pos_in(align.h)", "tm_credits")
	}
	tm_element_list(do.call(tm_element, c(args, list(subclass = c("tm_credits", "tm_component")))))
}


#' Map component: compass
#'
#' Map component that adds a compass
#'
#' @param north north
#' @param type compass type, one of: \code{"arrow"}, \code{"4star"}, \code{"8star"}, \code{"radar"}, \code{"rose"}. The default is controlled by \code{\link{tm_layout}} (which uses \code{"arrow"} for the default style)
#' @param text.size text.size
#' @param size size
#' @param show.labels show.labels
#' @param cardinal.directions cardinal.directions
#' @param text.color text.color
#' @param color.dark color.dark
#' @param color.light color.light
#' @param lwd lwd
#' @inheritParams tm_title
#' @param stack stack with other map components, either `"vertical"` or `"horizontal"`.
#' @param just just
#' @param margins margins
#' @param ... to catch deprecated arguments (alpha)
#' @seealso \href{https://r-tmap.github.io/tmap/articles/basics_components}{Vignette about components}
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
					   group_id,
					   bg,
					   bg.color,
					   bg.alpha,
					   stack,
					   just,
					   frame,
					   frame.color,
					   frame.alpha,
					   frame.lwd,
					   frame.r,
					   margins,
					   z,
					   ...) {
	args = lapply(as.list(rlang::call_match()[-1]), eval, envir = parent.frame())
	args$called = names(args)
	args = warning_group_args(args)

	args$group_id = args$group_id %||% NA_character_
	args$group_type = c("tm_<other>", "tm_compass")
	args$z = args$z %||% NA_integer_
	tm_element_list(do.call(tm_element, c(args, list(subclass = c("tm_compass", "tm_component")))))
}

#' Map component: scale bar
#'
#' Map component that adds a scale bar.
#'
#' @param breaks breaks. E.g. `c(0, 10, 50)` places scale bar breaks at 0, 10, and 50 units. These units are specified in [tm_shape()].
#' @param width width of the scale bar. Units are number of text line heights, which is similar to the number of characters. In case `beaks` are specified, the `width` is only handy to finetune the approximated width, e.g. in case clipping of the labels occurs, or there is too much whitespace.
#' @param allow_clipping should clipping of the last label by allowed? If `TRUE` (default), the last break label including unit is printed even when it doesn't fit the frame. If `FALSE` it will not be printed. Instead the unit suffix is added to the second last label.
#' @param text.size text size
#' @param text.color text.color
#' @param color.dark color.dark
#' @param color.light color.light
#' @param lwd linewidth
#' @inheritParams tm_title
#' @param size Deprecated (use `text.size` instead)
#' @param stack stack with other map components, either `"vertical"` or `"horizontal"`.
#' @param margins margins
#' @seealso \href{https://r-tmap.github.io/tmap/articles/basics_components}{Vignette about components}
#' @export
tm_scalebar = function(breaks,
						width,
						allow_clipping,
						text.size,
						text.color,
						color.dark,
						color.light,
						lwd,
						position,
						group_id,
						bg,
						bg.color,
						bg.alpha,
						size = "deprecated",
						stack,
						frame,
						frame.color,
						frame.alpha,
						frame.lwd,
						frame.r,
						margins,
						z) {

	if (!identical(size, "deprecated")) {
		cli::cli_inform(c(
			"!" = "The 'size' argument of {.fn tm_scalebar} is deprecated as of tmap 4.0.",
			"i" = "Please use 'text.size' instead."
		))
	}

	args = lapply(as.list(rlang::call_match()[-1]), eval, envir = parent.frame())
	args$called = names(args)
	args$group_id = args$group_id %||% NA_character_
	args$group_type = c("tm_<other>", "tm_scalebar")
	args$z = args$z %||% NA_integer_
	tm_element_list(do.call(tm_element, c(args, list(subclass = c("tm_scalebar", "tm_component")))))
}

#' Map component: scale bar
#'
#' This function was renamed to [tm_scalebar()] in tmap v4.0.
#' @inheritDotParams tm_scalebar
#' @export
#' @keywords internal
tm_scale_bar = function(...) {
	cli::cli_inform(c(
		"!" = "{.fn tm_scale_bar} is deprecated. Please use {.fn tm_scalebar} instead."
	))
	tm_scalebar(...)
}

#' Map component: mouse coordinates
#'
#' Map component that adds mouse coordinates
#'
#' @seealso \href{https://r-tmap.github.io/tmap/articles/basics_components}{Vignette about components}
#' @inheritParams tm_title
#' @export
tm_mouse_coordinates <- function(stack,
								 position,
								 group_id,
								 z) {
	args = lapply(as.list(rlang::call_match()[-1]), eval, envir = parent.frame())
	args$called = names(args)
	args$group_id = args$group_id %||% NA_character_
	args$group_type = c("tm_<other>", "tm_mouse_coordinates")
	args$z = args$z %||% NA_integer_
	tm_element_list(do.call(tm_element, c(args, list(subclass = c("tm_mouse_coordinates", "tm_component")))))
}

#' Map component: minimap
#'
#' Map component that adds a [minimap][leaflet::addMiniMap()] in view mode.
#'
#' @param server name of the provider or an URL (see \code{\link{tm_tiles}}).
#'   By default, it shows the same map as the basemap, and moreover, it will automatically change when the user switches basemaps.
#'   Note the latter does not happen when `server` is specified.
#' @param toggle should the minimap have a button to minimise it? By default \code{TRUE}.
#' @inheritParams tm_title
#' @inheritParams tm_inset
#' @inheritDotParams leaflet::addMiniMap
#' @seealso \href{https://r-tmap.github.io/tmap/articles/basics_components}{Vignette about components}
#' @export
tm_minimap <- function(server,
					   toggle,
					   height,
					   width,
					   margins,
					   between_margin,
					   position,
					   group_id,
					   frame, frame.color, frame.alpha, frame.lwd, frame.r, bg, bg.color, bg.alpha,
					   z,
					   ...) {
	args = lapply(as.list(rlang::call_match()[-1]), eval, envir = parent.frame())
	args$called = names(args)
	args$group_id = args$group_id %||% NA_character_
	args$group_type = c("tm_<other>", "tm_minimap")
	args$z = args$z %||% NA_integer_
	tm_element_list(do.call(tm_element, c(args, list(subclass = c("tm_minimap", "tm_component")))))
}


#' Map component: logo
#'
#' Map component that adds a logo.
#'
#' @param file either a filename or url of a png image. If multiple files/urls are provided with a character vector, the logos are placed near each other.
#'   To specify logos for small multiples use a list of character values/vectors. In order to stack logos vertically, multiple \code{tm_logo} elements can be stacked.
#' @param height height of the logo in number of text line heights. The width is scaled based the height and the aspect ratio of the logo.
#'   If multiple logos are specified by a vector or list, the heights can be specified accordingly.
#' @param margins margins
#' @param between_margin Margin between
#' @inheritParams tm_title
#' @example ./examples/tm_logo.R
#' @seealso \href{https://r-tmap.github.io/tmap/articles/basics_components}{Vignette about components}
#' @export
tm_logo = function(file,
				   height,
				   margins,
				   between_margin,
				   stack,
				   position,
				   group_id,
				   frame,
				   frame.color,
				   frame.alpha,
				   frame.lwd,
				   frame.r,
				   z) {
	args = lapply(as.list(rlang::call_match()[-1]), eval, envir = parent.frame())
	args$called = names(args)
	args$group_id = args$group_id %||% NA_character_
	args$group_type = c("tm_<other>", "tm_logo")
	args$z = args$z %||% NA_integer_
	tm_element_list(do.call(tm_element, c(args, list(subclass = c("tm_logo", "tm_component")))))
}
