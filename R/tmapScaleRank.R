#' @export
#' @rdname tmap_internal
tmapScaleRank = function(x1, scale, legend, chart, o, aes, layer, layer_args, sortRev, bypass_ord, submit_legend = TRUE) {

	# update misc argument from tmap option scale.misc.args
	scale = update_scale_args("rank", scale, aes, o)

	cls = data_class(x1, midpoint_enabled = !is.null(scale$midpoint))
	maincls = class(scale)[1]

	if (attr(cls, "unique") && is.null(scale$limits) && is.null(scale$ticks)) stop("Unique value, so cannot determine continuous scale range. Please specify limits and/or ticks.", call. = FALSE)

	#if (cls[1] == "na") stop("data contain only NAs, so ", maincls, " cannot be applied", call. = FALSE)

	if (cls[1] != "num") {
		if (!is.factor(x1)) x1 = as.factor(x1)
		x1 = as.integer(x1)
		warning(maincls, " is supposed to be applied to numerical data", call. = FALSE)
	}


	x1 = rank(without_units(x1), na.last = "keep")

	if (aes %in% c("lty", "shape", "pattern")) stop("tm_scale_rank cannot be used for layer ", layer, ", aesthetic ", aes, call. = FALSE)

	scale = structure(c(scale, list(limits = NULL, outliers.trunc = NULL, trans = NULL, midpoint = NULL,
						  trans.args = list())), class = c("tm_scale_continuous", "tm_scale", "list"))

	if (is.null(scale$ticks)) {
		scale$ticks = generate_rank_labels(max_rank = max(x1,na.rm=T), num_ticks = scale$n)
	}


	tmapScaleContinuous(x1, scale, legend, chart, o, aes, layer, layer_args, sortRev, bypass_ord, submit_legend)

}

generate_rank_labels = function(max_rank, num_ticks = 5) {
	ticks = c(1, max_rank)

	if (num_ticks > 2) {
		probs = seq(0, 1, length.out = num_ticks)
		probs = probs[-c(1, length(probs))]

		intermediate_ticks = quantile(1:max_rank, probs = probs)
		ticks = c(ticks, intermediate_ticks)
	}

	# Ensure the order
	ticks = sort(unique(round(ticks)))

	return(ticks)
}
