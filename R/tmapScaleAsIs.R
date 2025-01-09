#' @export
#' @rdname tmap_internal
tmapScaleAsIs = function(x1, scale, legend, chart, o, aes, layer, layer_args, sortRev, bypass_ord, submit_legend = TRUE) {
	legend = list(title = NA,
				  nitems = 0,
				  labels = NA,
				  dvalues = NA,
				  vvalues = NA,
				  vneutral = NA,
				  na.show = NA,
				  scale = "AsIs",
				  show = FALSE,
				  active = FALSE)

	x1h = head(x1, 100)
	check_values(layer, aes, x1h)


	scale$values.scale = if (is.na(scale$values.scale)) getAesOption("values.scale", o, aes, layer) else scale$values.scale

	isna = is.na(x1)
	if (any(isna) && is.na(scale$value.na)) {
		x1[isna] = getAesOption("value.na", o, aes, layer)
	}

	sfun = paste0("tmapValuesScale_", aes)
	cfun = paste0("tmapValuesColorize_", aes)

	sc = o$scale * scale$values.scale

	x2 = do.call(sfun, list(x = x1, scale = sc))
	values = do.call(cfun, list(x = x2, pc = o$pc))

	vneutral = if (is.na(scale$value.neutral)) {
		getAesOption("value.neutral", o, aes, layer)
	} else {
		scale$value.neutral
	}


	legend$vneutral = do.call(sfun, list(x = do.call(cfun, list(x = vneutral, pc = o$pc)), scale = o$scale))

	if (submit_legend) {
		if (bypass_ord) {
			format_aes_results(values, legend = legend, chart = chart)
		} else {
			format_aes_results(values, ord = 1L, legend = legend, chart = chart)
		}
	} else {
		list(vals = values, ids = 1L, legend = legend, chart = chart, bypass_ord = bypass_ord)
	}
}
