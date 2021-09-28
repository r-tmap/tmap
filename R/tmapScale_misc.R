get_scale_defaults = function(scale, opt, aes, layer, cls) {
	within(scale, {
		values = if (is.na(values[1])) getAesOption("values.var", opt, aes, layer, cls = cls) else values
		value.na = if (is.na(value.na) || identical(value.na, TRUE)) getAesOption("value.na", opt, aes, layer, cls = cls) else value.na
		value.null = if (is.na(value.null)) getAesOption("value.null", opt, aes, layer, cls = cls) else value.null
		value.neutral = if (is.na(value.neutral)) getAesOption("value.neutral", opt, aes, layer, cls = cls) else value.neutral
		values.contrast = if (is.na(values.contrast[1])) getAesOption("values.contrast", opt, aes, layer, cls = cls) else values.contrast
		
		value.blank = getAesOption("value.blank", opt, aes, layer, cls = cls)
		if (is.na(value.na) || value.na == value.blank) label.na = ""
		
		# label.na TRUE: always show NA's, but use option
		# label.na FALSE or "": never show NA's
		# label.na NA: show NA is there are any
		# label.na "qwerty" always snow NA's
		na.show = !identical(label.na, FALSE) && (identical(label.na, TRUE) || (!is.na(label.na) && label.na != ""))
		if (is.na(label.na)) na.show = NA # will be TRUE if there are NAs
		if (is.logical(label.na)) label.na = getAesOption("label.na", opt, aes, layer, cls = cls)
	})
}


tmapScale_returnNA = function(n, legend, value.na, label.na, na.show) {
	if (identical(na.show, FALSE)) {
		legend = list(title = NA, 
					  nitems = 0,
					  labels = NA, 
					  dvalues = NA, 
					  vvalues = NA,
					  vneutral = value.na,
					  na.show = NA,
					  setup = list(show = FALSE))
	} else {
		legend = list(title = legend$title, 
					  nitems = 1,
					  labels = label.na, 
					  dvalues = NA, 
					  vvalues = value.na,
					  vneutral = value.na,
					  na.show = TRUE,
					  setup = legend)
	}
	return(format_aes_results(rep(value.na, n), rep(0L, n), legend))}
