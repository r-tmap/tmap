
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
tmapLeafletCompPlot.tm_title = function(comp, lf, o) {
	if (inherits(lf, "shiny.tag.list")) {
		ncld <- length(lf[[1]])
		lf[[1]] <- mapply(function(l, i) {
			title <- l$children[[1]]$title %||% ""
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
		title <- comp$text %||% ""
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
tmapLeafletCompPlot.tm_compass = function(comp, lf, o) {
	lf %>% leafem::addLogo(system.file(paste0("img/compass_", comp$type, ".png"), package = "tmap"), position = leaflet_pos(comp$position), width = 120, height = 120)
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
tmapLeafletCompPlot.tm_scalebar = function(comp, lf, o) {
	lf %>% leaflet::addScaleBar(position = leaflet_pos(comp$position))
}




#' @export
tmapLeafletCompPrepare.tm_credits = function(comp, o) {
	comp$show = TRUE
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
tmapLeafletCompPlot.tm_credits = function(comp, lf, o) {
	#lf %>% leafem::addLogo(system.file(paste0("img/credits_", comp$type, ".png"), package = "tmsap"), src = "local", position = paste(unlist(comp$position[c("pos.v", "pos.h")]), collapse = ""), width = 120, height = 120)
	#message("tm_credits not implemented yet for view mode")
	lf %>% leaflet::addTiles(urlTemplate = "", attribution = comp$text)
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
tmapLeafletCompPlot.tm_mouse_coordinates = function(comp, lf, o) {
	lf %>% leafem::addMouseCoordinates()
}



#' @export
tmapLeafletCompPrepare.tm_minimap = function(comp, o) {
	comp$show = TRUE
	comp$height = 20 * comp$height
	comp$width = 20 * comp$width

	extra = comp[setdiff(intersect(names(comp), names(formals(leaflet::addMiniMap))), c("position", "map"))]

	comp$specified_tiles = !is.na(comp$server)

	comp$args = c(list(toggleDisplay = comp$toggle), extra)
	comp
}


#' @export
tmapLeafletCompHeight.tm_minimap = function(comp, o) {


	comp
}

#' @export
tmapLeafletCompWidth.tm_minimap = function(comp, o) {
	comp
}


#' @export
tmapLeafletCompPlot.tm_minimap = function(comp, lf, o) {
	comp$args$tiles = if (comp$specified_tiles) {
		comp$server
	} else if (length(.TMAP_LEAFLET$tiles)) {
		.TMAP_LEAFLET$tiles[[1]][[1]]$server[1]
	} else {
		o$basemap.server[1]
	}
	lf2 = do.call(addMiniMap, c(list(map = lf, position = leaflet_pos(comp$position)), comp$args))
	if (!comp$specified_tiles && (length(comp$args$tiles) > 0)) {
		lf2 <- lf2 %>%
			htmlwidgets::onRender("
			    function(el, x) {
			      var myMap = this;
			      myMap.on('baselayerchange',
			        function (e) {
			          myMap.minimap.changeLayer(L.tileLayer.provider(e.name));
			        })
			    }")
	}
	lf2
}

#' @export
tmapLeafletCompPrepare.tm_logo = function(comp, o) {
	height = 20 * comp$height
	comp$logo = lapply(comp$file, function(lf){
		x = tmap_icons(lf)
		scale = height / x$iconHeight
		x$iconWidth = x$iconWidth * scale
		x$iconHeight = x$iconHeight * scale
		x$iconAnchorX = x$iconAnchorX * scale
		x$iconAnchorY = x$iconAnchorY * scale
		x
	})

	comp$asp = vapply(comp$logo, function(lg) {
		lg$iconWidth / lg$iconHeight
	}, FUN.VALUE = numeric(1))
	comp$show = TRUE
	comp
}


#' @export
tmapLeafletCompHeight.tm_logo = function(comp, o) {
	comp
}

#' @export
tmapLeafletCompWidth.tm_logo = function(comp, o) {
	comp
}

#' @export
tmapLeafletCompPlot.tm_logo = function(comp, lf, o) {
	ws = c(rev(cumsum(vapply(comp$logo, function(l)l$iconWidth,FUN.VALUE = numeric(1)) + 10))[-1], 0)

	for (i in 1:length(comp$logo)) {
		lf = lf %>% leafem::addLogo(comp$logo[[i]]$iconUrl, position = leaflet_pos(comp$position), width = comp$logo[[i]]$iconWidth, height = comp$logo[[i]]$iconHeight, offset.x = 10 + ws[i], offset.y = 20)
	}
	lf
}



#' @export
tmapLeafletCompPrepare.tm_inset_image = function(comp, o) {
	comp$file = comp$x
	comp = tmapLeafletCompPrepare.tm_logo(comp, o)
	class(comp) = c("tm_logo", class(comp))
	comp
}

#' @export
tmapLeafletCompPrepare.tm_inset_grob = function(comp, o) {
	height = 20 * comp$height
	width = 20 * comp$width

	grob.dim = c(width=width, height=height, render.width=width, render.height=height)

	comp$logo = list(grob2icon(comp$x, grob.dim = grob.dim, just = c(0.5, 0.5)))
	comp$logo = lapply(comp$logo, function(x){
		scale = height / x$iconHeight
		x$iconWidth = unname(x$iconWidth * scale)
		x$iconHeight = unname(x$iconHeight * scale)
		x$iconAnchorX = unname(x$iconAnchorX * scale)
		x$iconAnchorY = unname(x$iconAnchorY * scale)
		x
	})

	comp$asp = vapply(comp$logo, function(lg) {
		lg$iconWidth / lg$iconHeight
	}, FUN.VALUE = numeric(1))
	comp$show = TRUE
	class(comp) = c("tm_logo", class(comp))
	comp

}

#' @export
tmapLeafletCompPrepare.tm_inset_gg = function(comp, o) {
	rlang::check_installed("ggplot2", reason = "for plotting ggplot2 charts")
	comp$x = ggplot2::ggplotGrob(comp$x)
	tmapLeafletCompPrepare.tm_inset_grob(comp, o)
}
