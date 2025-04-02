#' Specify an animation (experimental)
#'
#' @description `tm_animate`
#'
#' @param by group by variable used to create the animation frames. Note: it is called `pages` in the core function [tm_facets()].
#' @param nframes number of animation frames. This only app
#' @param fps frames per second. Default: 30 for `tm_facets_animate` and 2 for `tm_facets_animate_slow`.
#' @param play how should the animation be played? One of `"loop"`, `"pingpong"`, and `"once"`
#' @param dpr device pixel ratio. The ratio between the physical pixel density of a device and its logical pixel density.
#' @param ... passed on to [tm_facets()]
#' @export
#' @seealso [tm_facets()] which is the core function, and [tmap_animation()] used to save the animation
#' @rdname tm_animate
tm_animate = function(by = "VARS__",
							 nframes = 60L,
							 fps = 20L,
							 play = c("loop", "pingpong", "once"),
							 dpr = 2,
							 ...) {
	args = list(...)
	args_called = names(rlang::call_match()[-1])

	play = match.arg(play)

	tm = do.call("tm_facets", c(list(pages = by, type = NA, animate = TRUE, nframes = nframes, fps = fps, play = play, dpr = dpr), args[setdiff(names(args), "type")]))
	tm[[1]]$calls = args_called
	tm
}

#' @export
#' @rdname tm_animate
tm_animate_slow = function(by = "VARS__",
								  nframes = 60L,
								  fps = 2L,
								  play = c("loop", "pingpong", "once"),
								dpr = 2,
								  ...) {
	args = list(...)
	args_called = names(rlang::call_match()[-1])

	play = match.arg(play)

	tm = do.call("tm_facets", c(list(pages = by, type = NA, animate = TRUE, nframes = nframes, fps = fps, play = play, dpr = dpr), args[setdiff(names(args), "type")]))
	tm[[1]]$calls = args_called
	tm
}
