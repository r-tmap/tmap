#' Specify an animation
#'
#' Specify an animation from a tmap plot. This is similar to creating facets with [tm_facets()]. The animation subsequently can be exported to a gif or video file (e.g. mp4) with [tmap_animation()]. If the tmap plot with [tm_animate()] is printed, the animation will be previewed. The default [tm_animate()] will show the individual frames slowly (frame per seconds (fps) set to 2) whereas [tm_animate_fast()] will show them like a movie (with a fps set to 24).
#'
#' @description `tm_animate`
#'
#' @param frames group by variable used to create the animation frames. This is similar to the `by` argument of [tm_facets_wrap()]. Instead of showing facets next to each other, they are shown as animation frames. However, under the hood `frames` will be used to specify `pages` of [tm_facets()]. This makes it possible to create an animation of regular facets.
#' @param nframes number of animation frames. So far, this only applied experimentally in transition map variables. See the extension package tmap.cartogram.
#' @param fps frames per second. Default: 30 for `tm_facets_animate` and 2 for `tm_facets_animate_slow`.
#' @param play how should the animation be played? One of `"loop"` (default), `"pingpong"`, and `"once"`, where `"loop"` means that the animation will loop indefinitely, `"pingpong"` means that it will play forward and then backward, and `"once"` means that it will play only once.
#' @param dpr device pixel ratio. The ratio between the physical pixel density of a device and its logical pixel density.
#' @param ... passed on to [tm_facets()]. Note that for animated facets, `by` can be specified to create animated facet wraps, and `rows` and `cols` to created animated facet grids.
#' @export
#' @seealso [tm_facets()] which is the core function, and [tmap_animation()] used to save the animation
#' @rdname tm_animate
tm_animate_fast = function(frames = "VARS__",
							 nframes = 60L,
							 fps = 24L,
							 play = c("loop", "pingpong", "once"),
							 dpr = 2,
							 ...) {
	args = list(...)
	args_called = names(rlang::call_match()[-1])

	play = match.arg(play)

	tm = do.call("tm_facets", c(list(pages = frames, type = NA, animate = TRUE, nframes = nframes, fps = fps, play = play, dpr = dpr), args[setdiff(names(args), "type")]))
	tm[[1]]$calls = args_called
	class(tm[[1]]) = c("tm_animate", class(tm[[1]]))
	tm
}

#' @export
#' @rdname tm_animate
tm_animate = function(frames = "VARS__",
								  nframes = 60L,
								  fps = 2L,
								  play = c("loop", "pingpong", "once"),
								dpr = 2,
								  ...) {
	args = list(...)
	args_called = names(rlang::call_match()[-1])

	play = match.arg(play)

	tm = do.call("tm_facets", c(list(pages = frames, type = NA, animate = TRUE, nframes = nframes, fps = fps, play = play, dpr = dpr), args[setdiff(names(args), "type")]))
	tm[[1]]$calls = unique(c(args_called, "frames"))
	class(tm[[1]]) = c("tm_animate", class(tm[[1]]))
	tm
}
