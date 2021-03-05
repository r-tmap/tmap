view_format_popups <- function(id=NULL, titles, format, values) {
	
	htmlEsc = if (format$html.escape) {
		htmltools::htmlEscape
	} else {
		function(x) x
	}
	
	isnull <- vapply(values, is.null, logical(1))
	
	titles <- titles[!isnull]
	titles[names(titles)!=""] <- names(titles)[names(titles)!=""]
	
	values <- values[!isnull]
	
	islist <- is.list(format) && length(format)>0 && is.list(format[[1]])
	if (!islist) {
		format <- lapply(1:length(titles), function(i) format)
	}
	
	
	if (!is.null(id)) {
		labels <- paste("<b>", htmlEsc(id), "</b>", sep="")
	} else {
		labels <- ""
	}
	
	titles_format <- vapply(titles, htmlEsc, character(1))
	values_format <- mapply(function(v, f) {
		if (inherits(v, "units")) {
			popup_append <- paste0(" ", as.character(attr(v, "units")))
		} else {
			popup_append <- ""
		}
		numbers <- htmlEsc(if (is.numeric(v)) do.call("fancy_breaks", c(list(vec=as.numeric(v), intervals=FALSE), f)) else v)
		paste0(numbers, popup_append)
	}, values, format, SIMPLIFY = FALSE)
	
	
	labels2 <- mapply(function(l, v) {
		paste0("<tr><td style=\"color: #888888;\"><nobr>", l, "</nobr></td><td align=\"right\"><nobr>", v, "</nobr></td>")
	}, titles_format, values_format, SIMPLIFY=FALSE)
	
	labels3 <- paste0(do.call("paste", c(labels2, list(sep="</tr>"))), "</tr>")
	
	padding_right <- ifelse(length(titles_format) > 13, 15, 0) # add padding for horizontal scroll bar. These will appear on most browsers when there are over 13 normal lines (tested: RStudio, FF, Chrome)
	
	x <- paste0("<style> div.leaflet-popup-content {width:auto !important;overflow-y:auto; overflow-x:hidden;}</style><div style=\"max-height:25em;padding-right:", padding_right, "px;\"><table>
			   <thead><tr><th colspan=\"2\">", labels, "</th></thead></tr>", labels3, "</table></div>")
	
	x
}
