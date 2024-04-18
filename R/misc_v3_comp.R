v3_only = function(fun) {
	setdiff(funs_v3[[fun]], funs_v4[[fun]])
}

v3_start_message = function() {
	if (!.TMAP$v3) {
		message("tmap v3 code detected:")
		.TMAPv3 = TRUE
	}
	invisible(NULL)
}

v3_reset_flag = function() {
	.TMAPv3 = FALSE
	invisible(NULL)
}


v3_instead = function(args_called, old, new, fun, extra_called = character()) {
	args = args_called$args
	called = args_called$called
	
	if (old %in% called) {
		args[[new]] = args[[old]]
		args[[old]] = NULL
		
		# may be needed to trigger something (e.g. "shape" to use symbols instead of dots)
		if (length(extra_called)) {
			called = unique(c(called, extra_called))
		}
		message(paste0(fun, ": (v3->v4), use '", new, "' instead of '", old, "'"))
	}
	list(args = args, called = called)
}

v3_instead_value = function(args_called, old, new, fun, value_old, value_new) {
	args = args_called$args
	called = args_called$called
	
	if (old %in% called) {
		if (identical(args[[old]], value_old)) {
			args[[old]] = NULL
			args[[new]] = value_new
			if (is.null(value_old)) value_old = "NULL" 
			message(paste0(fun, ": (v3->v4), use '", new, " = ", value_new, "' instead of '", old, " = ", value_old, "'"))
			list(args = args, called = called)
		} else {
			v3_instead(args_called, old, new, fun)
		}
	} else {
		list(args = args, called = called)
	}
}
