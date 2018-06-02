process_gps <- function(gps, shps, x, gm, nx, interactive, return.asp) {
	title.snap.to.legend <- NULL
	scale.extra <- NULL

	
	gps <- mapply(function(gp, i) {
		# process credits text per facet
		gm$credits.show <- vapply(gm$credits.show, "[[", logical(1), i)
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
		gm$logo.show <- vapply(gm$logo.show, "[[", logical(1), i)
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
		
	layerids <- NULL
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
		if (gm$design.mode && !interactive && gm$show.messages) {
			masterShapeName <- x[[gm$shape.id[gm$shape.masterID]]]$shp_name
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
		
		# shortcut used by tmap_save
		if (return.asp && !interactive) return(gm$gasp)
	} else {
		if (nx>=2 && gm$as.layers) {
			nLayers <- length(gps[[1]]) - 1L
			
			layerids <- unlist(lapply(1:nLayers, function(i) {
				rep(i, ifelse(i %in% gm$layer_vary, nx, 1))
			}))
			
			layers <- lapply(1:nLayers, function(i) {
				if (i %in% gm$layer_vary) {
					mapply(function(gpsi, showLeg) {
						gpsii <- gpsi[[i]]
						if (!showLeg) {
							nms <- names(gpsii)
							legend.show.items <- nms[substr(nms, nchar(nms)-10, nchar(nms)) == "legend.show"]

							if (length(legend.show.items)) {
								fs <- paste0("free.scales.", substr(legend.show.items, 1, nchar(legend.show.items)-12))
								gpsii[legend.show.items] <- gm[fs] #as.list(rep(FALSE, length(legend.show.items)))
							}
						}
						gpsii
					}, gps, c(TRUE, rep(FALSE, nx-1)), SIMPLIFY = FALSE)
					
					# lapply(gps, function(gpsi) {
					# 	gpsi[[i]]
					# })
				} else {
					gps[[1]][i]
				}
			})
			layers <- do.call(c, layers)
			names(layers) <- paste0("tmLayer", 1L:length(layers))
			
			#varnames <- layers[[1]]$varnames
			
			
			
			gpsL <- gps[[1]]["tm_layout"]
			gpsL[[1]]$shp_name <- unlist(lapply(1:nLayers, function(i) {
				if (i %in% gm$layer_vary) {
					if (is.null(gpsL$tm_layout$panel.names)) {
						nms <- unname(vapply(gps, function(gpsi) {
							gpsii <- gpsi[[i]]
							if (gpsii$any.legend) {
								nm <- names(which(vapply(gpsii$varnames, function(vn)!is.na(vn[1]), logical(1))))[1]
								gpsii[[paste0(nm, ".legend.title")]]
							} else {
								as.character(NA)
							}
						}, character(1)))
						if (any(is.na(nms))) nms <- gm$title
						nms
					} else {
						gpsL$tm_layout$panel.names	
					}
				} else {
					gpsL[[1]]$shp_name[i]
				}
			})) # gpsL[[1]]$shp_name[layerids]
			
			gps <- list(plot1 = c(layers, gpsL))
			

			if (gm$shape.diff_shapes) {
				shps_layers <- lapply(1:nLayers, function(i) {
					if (i %in% gm$layer_vary) {
						lapply(shps, function(shpsi) {
							shpsi[[i]]
						})
					} else {
						shps[[1]][i]
					}
				})
				shps <- do.call(c, shps_layers)
			} else {
				shps <- shps[layerids]
			}
			
			
			gm$shape.nshps <- length(shps)
			gm$shape.diff_shapes <- FALSE
			gm$shape.shps_lengths <- append(gm$shape.shps_lengths, rep(gm$shape.shps_lengths[gm$layer_vary], nx - 1), after = gm$layer_vary)
			
			nx <- 1
		}
		
		
		
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
			if (is.null(gpl$npol)) return(gpl)
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
		 shps = shps,
		 nx = nx,
		 matchIDs=matchIDs,
		 layerids = layerids,
		 gp_leg=gp_leg,
		 gp_attr=gp_attr)
}
