check_values = function(layer, aes, values) {
	fun_check = paste0("tmapValuesCheck_", aes)

	are_valid = do.call(fun_check, args = list(x = values, is_var = FALSE))
	if (!are_valid) {
		info = attr(are_valid, "info")
		cli::cli_abort(c(
			"x" = "Incorrect values for layer {layer} aesthetic {aes}",
			"!" = "values should conform visual variable {.val {aes}}.",
			"i" = info
		),
		call = call(paste0("tm_", layer)))
	}
}

getValuesNames = function(values) {
	if (inherits(values, "tmap_icons")) {
		if ("iconUrl" %in% names(values)) {
			rep("", length(values$iconUrl))
		} else {
			if (!is.null(names(values))) {
				names(values)
			} else {
				rep("", length(values))
			}
		}
	} else {
		if (!is.null(names(values))) {
			names(values)
		} else {
			rep("", length(values))
		}
	}
}

#' Internal tmap function get scale values
#'
#' Internal tmap function get scale values
#'
#' @param scale scale
#' @param o o
#' @param aes aes
#' @param layer layer
#' @param cls cls
#' @param ct ct
#' @export
#' @keywords internal
get_scale_defaults = function(scale, o, aes, layer, cls, ct = NULL) {
	within(scale, {
		values = if (is.na(values[1])) {
			if (is.null(ct)) {
				getAesOption("values.var", o, aes, layer, cls = cls)
			} else {
				ct
			}
		} else values

		value.na = if (is.na(value.na) || isTRUE(value.na)) {
			m = if (aes %in% c("col", "fill")) getPalMeta(as.character(values[1])) else NULL
			ona = getAesOption("value.na", o, aes, layer, cls = cls)

			# take option value.na instead of cols4all palette na-color in these two cases:
			if (is.null(m) || substr(ona, 8, 9) == "00") {
				ona
			} else{
				getPalNA(as.character(values[1]))
			}
		} else {
			value.na
		}

		value.null = if (is.na(value.null)) getAesOption("value.null", o, aes, layer, cls = cls) else value.null
		value.neutral = if (is.na(value.neutral)) getAesOption("value.neutral", o, aes, layer, cls = cls) else value.neutral
		values.range = if (is.na(values.range[1])) getAesOption("values.range", o, aes, layer, cls = cls) else values.range
		values.scale = if (is.na(values.scale)) getAesOption("values.scale", o, aes, layer, cls = cls) else values.scale

		value.blank = getAesOption("value.blank", o, aes, layer, cls = cls)
		if (is.na(value.na) || identical(value.na, value.blank)) label.na = ""

		# label.na TRUE: always show NA's, but use option
		# label.na FALSE or "": never show NA's
		# label.na NA: show NA is there are any
		# label.na "qwerty" always snow NA's
		label.show = !isFALSE(label.na) && (isTRUE(label.na) || (!is.na(label.na) && label.na != ""))
		if (is.na(label.na)) label.show = NA # will be TRUE if there are NAs
		if (is.logical(label.na)) label.na = getAesOption("label.na", o, aes, layer, cls = cls)
	})
}

update_na.show = function(label.show, na.show, anyNA) {
	if (is.na(label.show)) {
		if (is.na(na.show)) anyNA else na.show
	} else {
		label.show
	}
}



tmapScale_returnNA = function(n, legend, chart, value.na, label.na, label.show, na.show, sortRev, bypass_ord) {

	ids = if (is.null(sortRev)) {
		NULL
	} else  {
		rep(0L, n)
	}

	if (isFALSE(label.show)) {
		legend = within(legend, {
			title = NA
			nitems = 0
			labels = NA
			dvalues = NA
			vvalues = NA
			vneutral = value.na
			na.show = na.show
			show = FALSE
		})
	} else {
		legend = within(legend, {
			title = legend$title
			nitems = 1
			labels = label.na
			dvalues = NA
			vvalues = value.na
			vneutral = value.na
			na.show = TRUE
		})
	}

	vals = rep(value.na, n)

	if (bypass_ord) {
		format_aes_results(vals, legend = legend, chart = chart)
	} else {
		format_aes_results(vals, ids, legend, chart = chart)
	}

}
