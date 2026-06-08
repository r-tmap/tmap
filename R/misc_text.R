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
	if (!is.na(txt) && substr(txt, 1, 8) == "__expr__") {
		txt = substr(txt, 9, nchar(txt))
		parse(text = txt)[[1]]
	} else {
		txt
	}
}

# Is a label expression-encoded (i.e. produced by encode_expr)?
is_encoded_expr = function(txt) {
	is.character(txt) & !is.na(txt) & substr(txt, 1, 8) == "__expr__"
}

# Decode a character vector of (possibly expression-encoded) labels into a form
# accepted by grid::textGrob(). When none of the labels are expressions the
# input character vector is returned unchanged (so plain text keeps its exact
# behaviour, including multi-line "\n" labels). When at least one label is an
# expression, an expression vector (plotmath) is returned; plain strings in the
# mix become plotmath string constants, which render as literal text.
decode_expr_vec = function(txt) {
	if (!is.character(txt)) return(txt)
	if (!any(is_encoded_expr(txt))) return(txt)
	as.expression(lapply(txt, decode_expr))
}

# Decode expression-encoded labels to plain character (deparsed), for backends
# that cannot render plotmath (e.g. Leaflet/HTML). Consistent with how titles
# and credits fall back to expr_to_char() in view mode.
decode_expr_chr = function(txt) {
	if (!is.character(txt)) return(expr_to_char(txt))
	enc = is_encoded_expr(txt)
	txt[enc] = substr(txt[enc], 9, nchar(txt[enc]))
	txt
}


is_num_string = function(x) {
	suppressWarnings(!is.na(as.numeric(x)))
}
