
#' @method tmapLeafletCompPrepare tm_title
#' @export
tmapLeafletCompPrepare.tm_title = function(comp, o) {
	comp
}


#' @method tmapLeafletCompHeight tm_title
#' @export
tmapLeafletCompHeight.tm_title = function(comp, o) {
	comp
}

#' @method tmapLeafletCompWidth tm_title
#' @export
tmapLeafletCompWidth.tm_title = function(comp, o) {
	comp
}


#' @method tmapLeafletLegPlot tm_title
#' @export
tmapLeafletLegPlot.tm_title = function(cmp, lf, o) {
	if (inherits(lf, "shiny.tag.list")) {
		ncld <- length(lf[[1]])
		lf[[1]] <- mapply(function(l, i) {
			title <- l$children[[1]]$title
			if (title!="") {
				l$children[[1]] <- l$children[[1]] %>% htmlwidgets::onRender(paste("
					function(el, x) {
						var tldiv = el.getElementsByClassName(\"leaflet-top leaflet-left\")[0];
						var titlediv = document.createElement('div');
						titlediv.className = \"info legend leaflet-control\";
						titlediv.innerHTML = \"<b>", title, "</b>\";
						tldiv.insertBefore(titlediv, tldiv.childNodes[0]);
					}", sep="")
				)
			}
			l
		}, lf[[1]], 0:(ncld-1), SIMPLIFY = FALSE)
	} else {
		title <- cmp$text
		if (title!="") {
			lf <- lf %>% htmlwidgets::onRender(paste("
						function(el, x) {
							var tldiv = el.getElementsByClassName(\"leaflet-top leaflet-left\")[0];
							var titlediv = document.createElement('div');
							titlediv.className = \"info legend leaflet-control\";
							titlediv.innerHTML = \"<b>", title, "</b>\";
							tldiv.insertBefore(titlediv, tldiv.childNodes[0]);
						}", sep="")
			)
		}
	}
	lf
	
}
