prearrange_element_order <- function(x) {
	a <- names(x)

	aid <- 1L:length(a)
	bid <- 1L:length(a)

	## add basemap if not already defined, place first if exist, and rename tm_basemap to tm_tiles
	## tm_basemap() will get tmap_options basemaps
	if (!any(a=="tm_basemap")) {
		b <- c("tm_basemap", a)
		bid <- c(0, bid) # 0 will be used to find which x element is imputed with tm_basemap()
	} else {
		bmid <- which(a=="tm_basemap")
		b <- c(a[bmid], a[setdiff(aid, bmid)])
		bid <- c(bid[bmid], bid[setdiff(bid, bmid)])
	}
	
	## add overlay if not already defined at the tail
	## tm_tiles() will get tmap_options overlays
	if (!any(a=="tm_tiles")) {
		b <- c(b, "tm_tiles")
		bid <- c(bid, -1) # -1 will be used to find which x element is imputed with tm_tiles()
	}
	
	a[a=="tm_basemap"] <- "tm_tiles"
	b[b=="tm_basemap"] <- "tm_tiles"
	
	if (b[1] == "tm_tiles") {
		b <- c("tm_dummy", b)
		bid <- c(NA, bid)
	}
	
	if (!b[1] %in% c("tm_dummy", "tm_shape")) {
		stop("First element should be tm_shape", call. = FALSE)
	}
	
	

	# lid <- which(a %in% c("tm_fill", "tm_borders", "tm_lines", "tm_symbols", "tm_raster"))
	# tid <- which(a %in% "tm_basemap", "tm_tiles")
	
		
	sid <- which(b %in% c("tm_dummy", "tm_shape"))
	
	y <- rep(0, length(b)); y[sid] <- 1
	
	cluster.id <- cumsum(y)
	
	bc <- split(b, cluster.id)
	bcid <- split(bid, cluster.id)
	

	res <- mapply(function(bci, bcidi) {
		lid <- which(bci %in% c("tm_fill", "tm_borders", "tm_lines", "tm_symbols", "tm_raster", "tm_text"))
		
		if (bci == "tm_shape" && !length(lid)) stop("no layer elements defined after tm_shape", call. = FALSE)
		if (bci == "tm_dummy" && length(lid)) stop("tm_shape element missing", call. = FALSE)
		
		tid <- which(bci == "tm_tiles")
		if (length(tid) > 1) {
			# if (length(bci) %in% tid) {
			# 	tid <- tid[tid!=length(bci)]
			# 	bci <- c(bci[-length(bci)], "tm_dummy", bci[length(bci)])
			# 	bcidi <- c(bcidi[-length(bcidi)], NA, bcidi[length(bcidi)])
			# }
			if (2 %in% tid) {
				tid <- tid[tid!=2]
				bci <- c("tm_dummy", bci[2], bci[-2])
				bcidi <- c(NA, bcidi[2], bcidi[-2])
			}
			
			tid <- which(bci == "tm_tiles")
			sid <- which(bci == "tm_shape")
			tid <- tid[tid>sid]
			
			if (length(tid) > 1) {
				
				# add tm_shape/tm_dummy for tm_tiles 2,..,k in reverse order
				lid <- which(bci %in% c("tm_fill", "tm_borders", "tm_lines", "tm_symbols", "tm_raster", "tm_text"))
				for (tidi in rev(tid[-1])) {
					if (any(lid>tidi)) {
						# copy tm_shape element
						bci <- append(bci, "tm_shape", tidi)
						bcidi <- append(bcidi, bcidi[sid], tidi)
						lid[lid > tidi] <- 0 # only look for layer elements in between tile elements
					}
					bci <- append(bci, "tm_dummy", tidi - 1)
					bcidi <- append(bcidi, NA, tidi - 1)
				}
			}
		}
		list(b = bci, bid = bcidi)
	}, bc, bcid, SIMPLIFY = FALSE)
	
	bc2 <- unname(do.call(c, lapply(res, "[[", 1)))
	bcid2 <- unname(do.call(c, lapply(res, "[[", 2)))
	bc2[bc2=="tm_dummy"] <- "tm_shape"
	
	
	

	dummy <- list(tm_shape = list(shp = NULL, shp_name = "dummy", is.master = FALSE))
	
	x2 <- rep(dummy, length(bc2))
	xid <- which(!is.na(bcid2) & !(bcid2 %in% c(-1, 0)))
	x2[xid] <- x[bcid2[xid]]
	if (length(which(bcid2==0))) x2[which(bcid2==0)] <- rep(tm_basemap(), length(which(bcid2==0)))
	if (length(which(bcid2==-1))) x2[which(bcid2==-1)] <- rep(list(tm_tiles = list(server = NA, group = NA, alpha = NA, grouptype = "overlay")), length(which(bcid2==-1)))
	names(x2) <- bc2
	
	x2
}
