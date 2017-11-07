process_gps <- function(gps, shps, x, gm, nx, interactive, return.asp) {
	title.snap.to.legend <- NULL
	scale.extra <- NULL

	
	gps <- mapply(function(gp, i) {
		# process credits text per facet
		gm$credits.show <- sapply(gm$credits.show, "[[", i)
		if (!is.null(gm$credits.text)) gm$credits.text <- get_text_i(gm$credits.text, i)
		#if (!is.null(gm$credits.text)) gm$credits.text <- sapply(gm$credits.text, "[[", i)
		gm[c("credits.text", "credits.size", "credits.col", "credits.alpha", "credits.align",
			 "credits.bg.color", "credits.bg.alpha", "credits.fontface", "credits.fontfamily",
			 "credits.position", "credits.just", "credits.id")] <- lapply(
			 	gm[c("credits.text", "credits.size", "credits.col", "credits.alpha", "credits.align",
			 		 "credits.bg.color", "credits.bg.alpha", "credits.fontface", "credits.fontfamily",
			 		 "credits.position", "credits.just", "credits.id")],
			 	function(gm2) {
			 		gm2[gm$credits.show]	
			 	})
		gm$credits.show <- any(gm$credits.show)
		
		# process logos per facet
		gm$logo.show <- sapply(gm$logo.show, "[[", i)
		if (!is.null(gm$logo.file)) {
			gm$logo.file <- lapply(gm$logo.file, function(lf)lf[[i]])
			gm$logo.height <- lapply(gm$logo.height, function(lh)lh[[i]])
			gm$logo.width <- lapply(gm$logo.width, function(lw)lw[[i]])
		}
		#if (!is.null(gm$credits.text)) gm$credits.text <- sapply(gm$credits.text, "[[", i)
		gm[c("logo.file", "logo.position", "logo.just", "logo.height", "logo.width", "logo.halign", "logo.margin", "logo.id")] <- lapply(
			gm[c("logo.file", "logo.position", "logo.just", "logo.height", "logo.width", "logo.halign", "logo.margin", "logo.id")],
			function(gmi) {
				gmi[gm$logo.show]	
			})
		gm$logo.show <- any(gm$logo.show)
		
		gp$tm_layout <- gm
		gp$tm_layout$title <- gp$tm_layout$title[i]
		gp
	}, gps, 1:nx, SIMPLIFY=FALSE)
		
	if (!interactive) {

		if (gm$legend.outside) {
			leg_ids <- seq(1, nx, by=gm$pp) #ncol * gm$nrow)
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
		
		if (gm$attr.outside) {
			attr_ids <- seq(1, nx, by=gm$ncol * gm$nrow)
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
		if (gm$design.mode && !interactive) {
			masterShapeName <- x[[gm$shape.masterID]]$shp_name
			showBrown <- gm$gasp!=gm$shape.sasp
			showGreen <- !(!is.na(gm$asp) && gm$asp==0 && nx==1)
			pretext <- c("specified (asp argument of tm_layout)", "device (yellow)", "device without outer margins (green)",  "facets region (brown)", "frame (blue)", paste("master shape, ", masterShapeName, ", (red)", sep=""))
			posttext <- format(c(gm$asp, gm$shape.dasp, gm$shape.tasp, gm$gasp, gm$shape.sasp, gm$shape.masp))
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
		if (return.asp && !interactive) return(gm$gasp)
	} else {
		gp_leg <- NULL
		gp_attr <- NULL
	}
	
	## shapes have been subset (diff_shapes) and cropped. Therefore, the corresponding aesthetics have to be subset accordingly:
	if (gm$shape.diff_shapes) {
		matchIDs <- lapply(shps[1:nx], function(ss) lapply(ss, function(s) if (inherits(s, "Raster")) s[] else s$tmapID))
	} else {
		matchIDs <- lapply(shps, function(s) if (inherits(s, "Raster")) s[] else s$tmapID)
		matchIDs <- lapply(1:nx, function(i) matchIDs)
	}
	gps <- mapply(function(gp, masterID) {
		gp[1:gm$shape.nshps] <- mapply(function(gpl, indices, l) {
			npol_old <- gpl$npol
			gpl$npol <- length(indices)
			lapply(gpl, function(gplx) {
				if ((is.vector(gplx) || is.factor(gplx)) && length(gplx)==npol_old) {
					gplx[indices]	
				} else {
					gplx
				}
			})
		},  gp[1:gm$shape.nshps], masterID, gm$shape.shps_lengths, SIMPLIFY=FALSE)
		gp
	}, gps, matchIDs, SIMPLIFY=FALSE)
	

	
	list(gps=gps,
		 matchIDs=matchIDs,
		 gp_leg=gp_leg,
		 gp_attr=gp_attr)
}