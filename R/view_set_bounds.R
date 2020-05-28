view_set_bounds <- function(lf, gt) {
	if (is.logical(gt$set.bounds) && !is.null(lf$x$limits)) {
		lims <- unname(unlist(lf$x$limits, use.names = FALSE)[c(3,1,4,2)])
	} else {
		lims <- gt$set.bounds
	}
	if (!(identical(gt$set.bounds, FALSE))) {
		lf <- lf %>% setMaxBounds(lims[1], lims[2], lims[3],lims[4])
	}
	if (!is.na(gt$set.zoom.limits[2])) { # 2nd is checked to bypass (-1000, NA) used for simple CRS
		if (is.na(gt$set.view[1])) {
			gt$set.view <- c(mean.default(lims[c(1,3)]), mean.default(lims[c(2,4)]), gt$set.zoom.limits[1])
		}
	}
	if (length(gt$set.view) == 1 && !is.na(gt$set.view[1])) {
		gt$set.view <- c(mean.default(lims[c(1,3)]), mean.default(lims[c(2,4)]), gt$set.view)
	}
	
	if (!is.na(gt$set.view[1]) && !gt$global_bbox_specified) {
		set.view <- gt$set.view
		
		if (!is.null(names(set.view))) {
			if (!setequal(names(set.view), c("lon", "lat", "zoom"))) stop("Incorrect set.view names. They should be \"lon\", \"lat\", and \"zoom\"", call. = FALSE)
			set.view <- unname(set.view[c("lon", "lat", "zoom")])
		}
		
		lf <- lf %>% setView(set.view[1], set.view[2], set.view[3])
	} else {
		bbx <- unname(gt$bbox)
		if (!is.null(bbx)) lf <- lf %>% fitBounds(bbx[1], bbx[2], bbx[3], bbx[4]) #setView(view[1], view[2], view[3])
	}
	
	if (!is.null(gt$center)) lf <- lf %>% addMarkers(gt$center$lon, gt$center$lat, label = gt$center$query)
	
	lf
}
