#' @export
#' @rdname tmap_internal
tmapScaleBivariate = function(x1, x2, scale, legend, chart, o, aes, layer, layer_args, sortRev, bypass_ord, submit_legend = TRUE) {

	if (!(aes %in% c("fill", "color", "shape"))) stop("tm_scale_bivariate cannot be used for layer ", layer, ", aesthetic ", aes, call. = FALSE)

	scale = get_scale_defaults(scale, o, aes, layer, cls = "biv")


	dummyLegend = legend
	dummyLegend$show = FALSE

	dummyChart = list(show = FALSE, summary = "pass")

	res = mapply(function(x, s) {
		f = s$FUN
		s$FUN = NULL
		# update label.format
		s$label.format = process_label_format(scale$label.format, o$label.format)
		r = do.call(f, list(x1 = x, scale = s, legend = dummyLegend, chart = dummyChart, o = o, aes = aes, layer = layer, layer_args = layer_args, sortRev = sortRev, bypass_ord = bypass_ord, submit_legend = FALSE))

		leg = r$legend
		vv = leg$vvalues
		lab = leg$labels
		na = leg$na.show
		if (na) {
			vv = head(vv, -1)
			lab = head(lab, -1)
		}
		vid = match(r$vals, vv)
		n = length(lab)
		list(vv = vv, lab = lab, na = na, vid = vid, n = n, ids = r$ids, scale = leg$scale, chart = r$chart)

	}, list(x1, x2), scale[1:2], SIMPLIFY = FALSE)

	if (res[[1]]$scale == "continuous" || res[[2]]$scale == "continuous") stop("tm_scale_bivariate not implemented for continuous scales", call. = FALSE)

	n1 = res[[1]]$n # rows m
	n2 = res[[2]]$n # cols n

	vals = res[[1]]$vid + (res[[2]]$vid - 1L) * n1

	with(scale, {
		check_values(layer, aes, values)

		fun_getBVV = paste0("tmapValuesBVV_", aes)
		VV = do.call(fun_getBVV, list(x = values, value.na = value.na, m = n1, n = n2, scale = values.scale * o$scale, rep = values.repeat, o = o))

		vvalues = VV$vvalues
		value.na = VV$value.na


		#pal = c4a(cols4all::.P$c4a$bivs$pu_gn_bivs, n = n2, m = n1)[n1:1,]
		#colorNA = c4a_na(cols4all::.P$c4a$bivs$pu_gn_bivs)

		#pal = as.vector(pal)


		pal = as.vector(vvalues)#[n1:1,])

		attr(pal, "biv") = TRUE
		attr(pal, "m") = n1
		attr(pal, "n") = n2

		vals = pal[vals]

		vals[is.na(vals)] = value.na

		ids = res[[1]]$ids + res[[2]]$ids

		na = res[[1]]$na || res[[2]]$na


		legend$labels = c(res[[1]]$lab, res[[2]]$lab)
		legend$nitems = n1 + na
		legend$dvalues = vals
		legend$vvalues = pal
		legend$vneutral = pal[4]
		legend$na.show = na
		legend$layer_args = layer_args



		chartFun = paste0("tmapChart", toTitleCase(chart$summary))

		chart = do.call(chartFun, list(chart,
									   chart1 = res[[1]]$chart,
									   chart2 = res[[2]]$chart))


		# for consistency with the other tmapScaleX
		# not needed because submit_legend is only needed to supress legends of the two variables by which a bivariate legend is created.
		if (submit_legend) {
			if (bypass_ord) {
				format_aes_results(vals, legend = legend, chart = chart)
			} else {
				format_aes_results(vals, ids, legend, chart)
			}
		} else {
			list(vals = vals, ids = ids, legend = legend, bypass_ord = bypass_ord)
		}



	})

}
