get_scale_defaults = function(scale, o, aes, layer, cls) {
	within(scale, {
		values = if (is.na(values[1])) getAesOption("values.var", o, aes, layer, cls = cls) else values
		
		value.na = if (is.na(value.na) || identical(value.na, TRUE)) {
			m = getPalMeta(values[1])
			if (is.null(m)) {
				getAesOption("value.na", o, aes, layer, cls = cls)		
			} else{
				cols4all::c4a_na(values)
			}
		} else {
			value.na
		}

		value.null = if (is.na(value.null)) getAesOption("value.null", o, aes, layer, cls = cls) else value.null
		value.neutral = if (is.na(value.neutral)) getAesOption("value.neutral", o, aes, layer, cls = cls) else value.neutral
		values.range = if (is.na(values.range[1])) getAesOption("values.range", o, aes, layer, cls = cls) else values.range
		
		value.blank = getAesOption("value.blank", o, aes, layer, cls = cls)
		if (is.na(value.na) || identical(value.na, value.blank)) label.na = ""
		
		# label.na TRUE: always show NA's, but use option
		# label.na FALSE or "": never show NA's
		# label.na NA: show NA is there are any
		# label.na "qwerty" always snow NA's
		na.show = !identical(label.na, FALSE) && (identical(label.na, TRUE) || (!is.na(label.na) && label.na != ""))
		if (is.na(label.na)) na.show = NA # will be TRUE if there are NAs
		if (is.logical(label.na)) label.na = getAesOption("label.na", o, aes, layer, cls = cls)
	})
}


tmapScale_returnNA = function(n, legend, value.na, label.na, na.show, sortRev) {
	
	ids = if (is.null(sortRev)) {
		NULL
	} else  {
		rep(0L, n)
	}
	
	if (identical(na.show, FALSE)) {
		legend = within(legend,{
			title = NA
			nitems = 0
			labels = NA 
			dvalues = NA 
			vvalues = NA
			vneutral = value.na
			na.show = NA
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
		format_aes_results(vals, legend)
	} else {
		format_aes_results(vals, ids, legend)			
	}
	
}
