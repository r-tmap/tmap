process_gps <- function(gps, shps, datasets, gmeta, nx, p, a, s, shps_lengths, interactive, return.asp) {
	
	if (!interactive) {

		if (gmeta$legend.outside) {
			leg_ids <- seq(1, nx, by=gmeta$ncol * gmeta$nrow)
			gp_leg <- lapply(leg_ids, function(li) {
				gli <- gps[[li]]
				gli$tm_layout <- within(gli$tm_layout, {
					legend.only <- TRUE
					legend.width <- .9
					legend.height <- .9
					
					if (title.snap.to.legend) {
						title.size <- title.size / scale.extra
					} else {
						title <- ""
					}
					legend.title.size <- legend.title.size / scale.extra
					legend.text.size <- legend.text.size / scale.extra
					legend.hist.size <- legend.hist.size / scale.extra
					grid.show <- FALSE
					scale.show <- FALSE
					compass.show <- FALSE
					credits.show <- FALSE
					logo.show <- FALSE
				})
				gli
			})
			gps <- lapply(gps, function(gp) {
				gp$tm_layout$legend.show <- FALSE
				if (gp$tm_layout$title.snap.to.legend) gp$tm_layout$title <- ""
				gp
			})
		} else {
			gp_leg <- NULL
		}
		
		if (gmeta$attr.outside) {
			attr_ids <- seq(1, nx, by=gmeta$ncol * gmeta$nrow)
			gp_attr <- lapply(attr_ids, function(ai) {
				gai <- gps[[ai]]
				gai$tm_layout <- within(gai$tm_layout, {
					legend.only <- TRUE
					legend.show <- FALSE
					title <- ""
				})	
				gai
			})
			
			gps <- lapply(gps, function(gp) {
				gp$tm_layout$scale.show <- FALSE
				gp$tm_layout$compass.show <- FALSE
				gp$tm_layout$credits.show <- FALSE
				gp$tm_layout$logo.show <- FALSE
				gp
			})
		} else {
			gp_attr <- NULL
		}
		
		
		## show aspect ratios in design mode
		if (gmeta$design.mode && !interactive) {
			masterShapeName <- x[[s$masterID]]$shp_name
			showBrown <- gmeta$gasp!=sasp
			showGreen <- !(!is.na(gmeta$asp) && gmeta$asp==0 && nx==1)
			pretext <- c("specified (asp argument of tm_layout)", "device (yellow)", "device without outer margins (green)",  "facets region (brown)", "frame (blue)", paste("master shape, ", masterShapeName, ", (red)", sep=""))
			posttext <- format(c(gmeta$asp, v$dasp, a$tasp, gmeta$gasp, sasp, masp))
			if (!showBrown) {
				pretext <- pretext[-4]
				posttext <- posttext[-4]
			}
			if (!showGreen) {
				pretext <- pretext[-3]
				posttext <- posttext[-3]
			}
			
			lns <- nchar(pretext) + nchar(posttext)
			l <- max(max(nchar(pretext)) + max(nchar(posttext)) + 1, 25)
			medtext <- vapply(l-lns, function(i)paste(rep(" ", i), collapse=""), character(1))
			
			texts <- c(paste("----------------aspect ratios--", paste(rep("-", l-25), collapse=""), sep=""),
					   paste("|", pretext, medtext, posttext, "|"),
					   paste(rep("-", l+6), collapse=""))
			
			for (tx in texts) message(tx)
		}
		
		# shortcut used by save_tmap
		if (return.asp && !interactive) return(gmeta$gasp)
	} else {
		gp_leg <- NULL
		gp_attr <- NULL
	}
	
	## shapes have been subset (diff_shapes) and cropped. Therefore, the corresponding aesthetics have to be subset accordingly:
	if (p$diff_shapes) {
		matchIDs <- lapply(shps, function(ss) lapply(ss, function(s) if (inherits(s, "Raster")) s[] else s$tmapID))
	} else {
		matchIDs <- lapply(shps, function(s) if (inherits(s, "Raster")) s[] else s$tmapID)
		matchIDs <- lapply(1:nx, function(i) matchIDs)
	}
	
	gps <- mapply(function(gp, masterID) {
		gp[1:s$nshps] <- mapply(function(gpl, indices, l) {
			gpl$npol <- length(indices)
			lapply(gpl, function(gplx) {
				if ((is.vector(gplx) || is.factor(gplx)) && length(gplx)==l) {
					gplx <- gplx[indices]	
				} else {
					gplx
				}
			})
		},  gp[1:s$nshps], masterID, shps_lengths, SIMPLIFY=FALSE)
		gp
	}, gps, matchIDs, SIMPLIFY=FALSE)
	
	# } else {
	
	# 
	# gps <- lapply(gps, function(gp) {
	# 	gp[1:nshps] <- mapply(function(gpl, indices, l) {
	# 		gpl$npol <- length(indices)
	# 		lapply(gpl, function(gplx) {
	# 			if ((is.vector(gplx) || is.factor(gplx)) && length(gplx)==l) {
	# 				gplx <- gplx[indices]	
	# 			} else {
	# 				gplx
	# 			}
	# 		})
	# 	},  gp[1:nshps], matchIDs, shps_lengths, SIMPLIFY=FALSE)
	# 	gp
	# })
	
	# }
	
	#print(matchIDs)
	
	list(gps=gps,
		 matchIDs=matchIDs,
		 gp_leg=gp_leg,
		 gp_attr=gp_attr)
}