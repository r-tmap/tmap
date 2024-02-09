
#' @export
tmapLeafletCompPrepare.tm_title = function(comp, o) {
	comp$show = TRUE
	comp
}


#' @export
tmapLeafletCompHeight.tm_title = function(comp, o) {
	comp
}

#' @export
tmapLeafletCompWidth.tm_title = function(comp, o) {
	comp
}


#' @export
tmapLeafletLegPlot.tm_title = function(comp, lf, o) {
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
		title <- comp$text
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



#' @export
tmapLeafletCompPrepare.tm_compass = function(comp, o) {
	comp$show = TRUE
	comp
}


#' @export
tmapLeafletCompHeight.tm_compass = function(comp, o) {
	comp
}

#' @export
tmapLeafletCompWidth.tm_compass = function(comp, o) {
	comp
}


#' @export
tmapLeafletLegPlot.tm_compass = function(comp, lf, o) {
	lf %>% leafem::addLogo(system.file(paste0("img/compass_", comp$type, ".png"), package = "tmap"), src = "local", position = leaflet_pos(comp$position), width = 120, height = 120)
}



#' @export
tmapLeafletCompPrepare.tm_scalebar = function(comp, o) {
	comp$show = TRUE
	comp
}


#' @export
tmapLeafletCompHeight.tm_scalebar = function(comp, o) {
	comp
}

#' @export
tmapLeafletCompWidth.tm_scalebar = function(comp, o) {
	comp
}


#' @export
tmapLeafletLegPlot.tm_scalebar = function(comp, lf, o) {
	lf %>% leaflet::addScaleBar(position = leaflet_pos(comp$position))
}




#' @export
tmapLeafletCompPrepare.tm_credits = function(comp, o) {
	comp
}


#' @export
tmapLeafletCompHeight.tm_credits = function(comp, o) {
	comp
}

#' @export
tmapLeafletCompWidth.tm_credits = function(comp, o) {
	comp
}


#' @export
tmapLeafletLegPlot.tm_credits = function(comp, lf, o) {
	#lf %>% leafem::addLogo(system.file(paste0("img/credits_", comp$type, ".png"), package = "tmap"), src = "local", position = paste(unlist(comp$position[c("pos.v", "pos.h")]), collapse = ""), width = 120, height = 120)
	message("tm_credits not implemented yet for view mode")
	lf
}


#' @export
tmapLeafletCompPrepare.tm_mouse_coordinates = function(comp, o) {
	comp$show = TRUE
	comp
}


#' @export
tmapLeafletCompHeight.tm_mouse_coordinates = function(comp, o) {
	comp
}

#' @export
tmapLeafletCompWidth.tm_mouse_coordinates = function(comp, o) {
	comp
}


#' @export
tmapLeafletLegPlot.tm_mouse_coordinates = function(comp, lf, o) {
	lf %>% leafem::addMouseCoordinates()
}


