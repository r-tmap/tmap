text_width_npc = function(txt, space = TRUE, to_height = FALSE) {
	brks = attr(txt, "brks")
	if (is.null(brks)) {
		if (space) txt = paste(txt, " ", sep = "")

		if (to_height) {
			convertHeight(convertWidth(stringWidth(txt), "inch"), "npc", TRUE)
		} else {
			convertWidth(stringWidth(txt), "npc", TRUE)
		}

	} else {
		txt_splits = split_legend_labels(txt, brks)
		spx = if (space) convertWidth(stringWidth(" "), "npc", TRUE) * 1.5 else 0
		res = lapply(txt_splits, function(tx) convertWidth(stringWidth(tx), "npc", TRUE) + spx)

		max1 = max(sapply(res, "[", 1))
		max2 = max(sapply(res, "[", 2))
		max3 = max(sapply(res, "[", 3))
		#r3 = sapply(res, "[", 3)
		widths = max1 + max2 + max3 #r3

		attr(widths, "cw") =  do.call(rbind, res)
		widths
	}
}

text_width_inch = function(txt, space = TRUE) {
	brks = attr(txt, "brks")
	if (is.null(brks)) {
		if (space) txt = paste(txt, " ", sep = "")

		convertWidth(stringWidth(txt), "inch", TRUE)

	} else {
		txt_splits = split_legend_labels(txt, brks)
		spx = if (space) convertWidth(stringWidth(" "), "inch", TRUE) * 1.5 else 0
		res = lapply(txt_splits, function(tx) convertWidth(stringWidth(tx), "inch", TRUE) + spx)

		max1 = max(sapply(res, "[", 1))
		max2 = max(sapply(res, "[", 2))
		max3 = max(sapply(res, "[", 3))
		#r3 = sapply(res, "[", 3)
		widths = max1 + max2 + max3 #r3

		attr(widths, "cw") =  do.call(rbind, res)
		widths
	}
}

text_height_npc = function(txt, to_width = FALSE) {
	if (to_width) {
		convertWidth(convertHeight(stringHeight(txt), "inch"), "npc", TRUE)
	} else {
		convertHeight(stringHeight(txt), "npc", TRUE)
	}
}

text_height_inch = function(txt, to_width = FALSE) {
	if (to_width) {
		grid::convertWidth(grid::convertHeight(stringHeight(txt), "inch"), "inch", TRUE)
	} else {
		grid::convertHeight(stringHeight(txt), "inch", TRUE)
	}
}

split_legend_labels = function(txt, brks) {
	lapply(1L:length(txt), function(i) {
		c(substr(txt[i], 1, brks[i,1]-2),
		  substr(txt[i], brks[i,1], brks[i,2]-2),
		  substr(txt[i], brks[i,2], nchar(txt[i])))
	})
}

is.ena = function(x) {
	if (is.expression(x)) {
		rep_len(FALSE, length(x))
	} else is.na(x)
}


nonempty_text = function(txt) {
	if (is.character(txt)) {
		nzchar(txt)
	} else rep_len(TRUE, length(txt))
}

number_text_lines = function(txt) {
	if (is.character(txt)) {
		length(strsplit(txt, "\n")[[1]])
	} else 1
}

expr_to_char = function(txt) {
	if (is.character(txt)) {
		txt
	} else {
		as.character(txt)
	}
}


encode_expr = function(txt) {
	if (is.character(txt)) {
		txt
	} else {
		paste0("__expr__", as.character(txt))
	}
}

decode_expr = function(txt) {
	if (substr(txt, 1, 8) == "__expr__") {
		txt = substr(txt, 9, nchar(txt))
		parse(text = txt)[[1]]
	} else {
		txt
	}
}


is_num_string = function(x) {
	suppressWarnings(!is.na(as.numeric(x)))
}
