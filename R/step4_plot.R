process_components = function(cdt, o) {
	cdt$class = sapply(cdt$comp, function(l) l$position$type)
	cdt$cell.h = sapply(cdt$comp, function(l) {x = l$position$cell.h; if (is.null(x)) NA else x})
	cdt$cell.v = sapply(cdt$comp, function(l) {x = l$position$cell.v; if (is.null(x)) NA else x})
	cdt$pos.h = sapply(cdt$comp, function(l) {x = l$position$pos.h; if (is.null(x)) NA else x})
	cdt$pos.v = sapply(cdt$comp, function(l) {x = l$position$pos.v; if (is.null(x)) NA else x})
	
	gs = tmap_graphics_name()
	
	funP = paste0("tmap", gs, "CompPrepare")
	funW = paste0("tmap", gs, "CompWidth")
	funH = paste0("tmap", gs, "CompHeight")

	# leg_ins = which(cdt$class == "in")
	# if (length(leg_ins)) {
	# 	for (i in leg_ins) {
	# 		cdt$comp[[i]]$group.just = c(cdt$cell.h[i], cdt$cell.v[i])
	# 	}
	# }

	cdt$comp = lapply(cdt$comp, function(comp) do.call(funP, list(comp = comp, o = o)))
	cdt$comp = lapply(cdt$comp, function(comp) do.call(funW, list(comp = comp, o = o)))
	cdt$comp = lapply(cdt$comp, function(comp) do.call(funH, list(comp = comp, o = o)))

	cdt[, ':='(facet_row = character(), facet_col = character())]
	cdt$stack_auto = vapply(cdt$comp, function(l) {
		s = l$stack
		length(s) > 1
	}, FUN.VALUE = logical(1))
	cdt$stack = vapply(cdt$comp, function(l) {
		s = l$stack
		if (length(s) > 1 && "manual" %in% names(s)) s["manual"] else s[1]
	}, FUN.VALUE = character(1))
	
	
	getLW = function(x) sapply(x, function(y) {
		yW = y$Win
		if (is.null(yW)) 0 else yW
	})
	getLH = function(x) sapply(x, function(y) {
		yH = y$Hin
		if (is.null(yH)) 0 else yH
	})
	# attempt to determine margins
	cdt[, legW := getLW(comp)]
	cdt[, legH := getLH(comp)]
	
	cdt
}

process_components2 = function(cdt, o) {
	# when facets are wrapped:
	
	
	
	
	if (o$type != "grid" && o$n > 1) {
		# # (o$nrows > 1 && o$ncols > 1) || 
		# if ((o$nrows == 1 && o$legend.position.all$v == "center") || (o$ncols == 1 && o$legend.position.all$h == "center")) {
		# 	# put all legends together (so ignoring col and row position) when 1) multiple rows and colums or 2) and 3) when facets for a row and there is still more place on the side than top/bottom (and likewise for one col)
		# 	cdt[class != "in", by1__ := NA]
		# 	cdt[class != "in", by2__ := NA]
		# } else 
		if (o$nrows == 1) {
			# -use by2 and not by1 when they form a row
			cdt[, by2__ := by1__]
			cdt[, by1__ := NA]
		} 
	}
	
	stacks = o$legend.stack
	
	
	# place legends inside if needed
	#if (o$ncols > 1 && o$nrows > 1) {
	if (o$type == "wrap") {
		# all free legends inside
		cdt[!is.na(by1__) | !is.na(by2__) & class == "autoout", ':='(class = "in")]	
	} else if (o$type == "grid") {
		# all free-per-facet legends inside
		cdt[!is.na(by1__) & !is.na(by2__) & class == "autoout", ':='(class = "in")]	
	}
	

	
	# update auto position (for 'all', 'rows', 'columns' legends)
	cdt[is.na(by1__) & is.na(by2__) & class == "autoout", ':='(cell.h = o$legend.position.all$cell.h, cell.v = o$legend.position.all$cell.v)]
	cdt[!is.na(by1__) & is.na(by2__) & class == "autoout", ':='(cell.h = o$legend.position.sides$cell.h, cell.v = "by")]
	cdt[is.na(by1__) & !is.na(by2__) & class == "autoout", ':='(cell.h = "by", cell.v = o$legend.position.sides$cell.v)]
	
	cdt[is.na(by1__) & is.na(by2__) & class == "autoout", ':='(stack = ifelse(stack_auto, ifelse(cell.h == "center", stacks["per_row"], ifelse(cell.v == "center", stacks["per_col"], stacks["all"])), stack))]
	cdt[!is.na(by1__) & is.na(by2__) & class == "autoout", ':='(stack = ifelse(stack_auto, stacks["per_row"], stack))]
	cdt[is.na(by1__) & !is.na(by2__) & class == "autoout", ':='(stack = ifelse(stack_auto, stacks["per_col"], stack))]
	
	
	cdt[class == "autoout", class := "out"]
	
	
	lai = which(cdt$class == "autoin")
	if (length(lai)) {
		cdt$comp = lapply(cdt$comp, function(l) {
			l$position[c("pos.h", "pos.v")] = as.list(o$legend.autoin.pos)
			l
		})
		cdt[class == "autoin", ":="(pos.h = o$legend.autoin.pos[1], pos.v = o$legend.autoin.pos[2], class = "in")]
	}

	vby = any(cdt$cell.v == "by")
	hby = any(cdt$cell.h == "by")
	
	toC = function(x) {
		paste(x, collapse = "_")
	}
	
	
	# manual outside legends -2 is top or left, -1 is bottom or right
	cdt[class %in% c("autoout", "out"), ':='(facet_row = ifelse(cell.v == "center", ifelse(vby, "1", toC(1:o$nrows)), ifelse(cell.v == "by", as.character(by1__), ifelse(cell.v == "top", as.character(-2), as.character(-1)))),
											 facet_col = ifelse(cell.h == "center", ifelse(hby, "1", toC(1:o$ncols)), ifelse(cell.h == "by", as.character(by2__), ifelse(cell.h == "left", as.character(-2), as.character(-1)))))]
	
	
	
	cdt
}

step4_plot = function(tm, vp) {
	tmx = tm$tmo
	o = tm$o
	aux = tm$aux
	cmp = tm$cmp
	
	# split tm in case of as.layers in tm_facets
	# TODO

	# get name of graphics engine (for function names e.g. tmapGridInit)
	gs = tmap_graphics_name()
	
	o = prepreprocess_meta(o, vp)
	
	# get legends from layer data and put them in "components data.table" (cdt)
	cdt = step4_plot_collect_legends(tmx)
	
	
	if (length(cmp)) {
		cdt2 = data.table::rbindlist(lapply(cmp, function(cp) {
			data.table(by1__ = as.integer(NA),
					   by2__ = as.integer(NA),
					   by3__ = as.integer(NA),
					   comp = list(cp))	
		}))
		cdt = data.table::rbindlist(list(cdt, cdt2))
	}
	
		

	cdt = process_components(cdt, o)
	
	
	
	# determine panel type, inner margins, and automatic comp placement
	o = preprocess_meta(o, cdt)
	
	# function to get shape object
	get_shpTM = function(shpDT, by1, by2, by3) {
		b = list(by1, by2, by3)
		bynames = intersect(names(shpDT), paste0("by", 1:3, "__"))
		byids = as.integer(substr(bynames, 3, 3))
		
		sel = rep(TRUE, nrow(shpDT))
		if (length(bynames)) {
			for (i in 1L:length(bynames)) {
				sel = sel & shpDT[[bynames[i]]] %in% b[[byids[i]]]		
			}
		}
		shpDT$shpTM[which(sel)]
	}
	
	# function to subset data
	get_dt = function(dt, by1, by2, by3) {
		b = list(by1, by2, by3)
		bynames = intersect(names(dt), paste0("by", 1:3, "__"))
		byids = as.integer(substr(bynames, 3, 3))
		
		sel = rep(TRUE, nrow(dt))
		if (length(bynames)) {
			for (i in 1:length(bynames)) {
				sel = sel & dt[[bynames[i]]] %in% b[[byids[i]]]			
			}
		}
		dt[which(sel),]
	}
	
	# function to get bbox per facet
	get_bbox = function(by1, by2, by3) {
		bbxs = lapply(tmain, function(tmi) {
			shpTM = get_shpTM(tmi$shpDT, by1, by2, by3)
			mdt = get_dt(tmi$mapping_dt, by1, by2, by3)
			bbxs2 = lapply(shpTM, stm_bbox, tmapID = mdt$tmapID__)
			bbx = stm_merge_bbox(bbxs2)
			if (is.na(bbx)) bbx else tmaptools::bb(bbx, asp.limit = 10)
		})
		list(list(bb_ext(stm_merge_bbox(bbxs), o$inner.margins)))
	}
	
	# main group (that determines bounding box)
	# TODO take into account multiple main groups (see step1_rearrange and get_main_ids)
	tmain = tmx[[o$main]][[1]]
	
	# create table with meta data for the facets (row, col id, bbox, asp)
	d = data.table::data.table(do.call(expand.grid, lapply(structure(o$nby, names = c("by1", "by2", "by3")), seq_len)))
	d[, i := seq_len(nrow(d))]
	grps = c("by1", "by2", "by3")[o$free.coords]
	d[, bbox:=do.call(get_bbox, as.list(.SD)), by = grps, .SDcols = c("by1", "by2", "by3")]
	d[, asp:=get_asp(bbox)]
	
	# determine automatic position of inside comp
	if (!any(o$free.coords) && any(cdt$class == "autoin")) {
		shp = tmain[[1]]$shpDT$shpTM[[1]]$shp
		# TODO take into account multiple main shapes
		# TODO take use areas instead of coordinates for polygons
		if (inherits(shp, c("sf", "sfc"))) {
			bbx = d$bbox[[1]]
			co = sf::st_coordinates(sf::st_centroid(shp))
			
			xn = (co[,1]-bbx[1])/(bbx[3]-bbx[1])
			yn = (co[,2]-bbx[2])/(bbx[4]-bbx[2])
			cornerID = which.max(c(
				bl = min(sqrt((xn^2) + (yn^2)), na.rm = TRUE),
				tl = min(sqrt((xn^2) + ((1-yn)^2)), na.rm = TRUE),
				tr = min(sqrt(((1-xn)^2) + ((1-yn)^2)), na.rm = TRUE),
				br = min(sqrt(((xn-1)^2) + (yn^2)), na.rm = TRUE)))
			
			o$legend.autoin.pos = switch(names(cornerID), tl = c("left", "top"), tr = c("right", "top"), bl = c("left", "bottom"), br = c("right", "bottom"))
		} else {
			o$legend.autoin.pos = c("left", "top")	
		}
	} else {
		o$legend.autoin.pos = c("left", "top")
	}
	
	# calculate margins, number of rows and colums, etc.
	o = process_meta(o, d, cdt)
	cdt = process_components2(cdt, o)
	
	o$ng = length(tmx)

	
	# determine row and col ids	
	if (o$panel.type == "xtab") {
		d[, row := as.integer((i - 1) %% o$nrows + 1)]
		d[, col := as.integer((((i - 1) %/% o$nrows + 1) - 1) %% o$ncols + 1)]
	} else {
		# wrap
		if (o$facet.flip) {
			d[, row := as.integer((i - 1) %% o$nrows + 1)]
			d[, col := as.integer((((i - 1) %/% o$nrows + 1) - 1) %% o$ncols + 1)]
		} else {
			d[, col := as.integer((i - 1) %% o$ncols + 1)]
			d[, row := as.integer((((i - 1) %/% o$ncols + 1) - 1) %% o$nrows + 1)]
		}
		
	}
	d[, page := as.integer(i - 1) %/% (o$nrows * o$ncols) + 1]
	
	# prepare function names
	FUNinit = paste0("tmap", gs, "Init")
	FUNaux = paste0("tmap", gs, "Aux")
	FUNrun = paste0("tmap", gs, "Run")
	FUNshape = paste0("tmap", gs, "Shape")
	FUNoverlay = paste0("tmap", gs, "Overlay")
	FUNwrap = paste0("tmap", gs, "Wrap")
	FUNxtab = paste0("tmap", gs, "Xtab")
	
	# init
	do.call(FUNinit, list(o = o))
	
	
	
	## prepare aux layers
	# create table with bounding boxes (the only important property, apart from settings)
	db = data.table(bbox = unique(d$bbox[!is.na(d$asp)]))
	db[, i:=1L:nrow(db)]
	d[, bi:=db$i[match(d$bbox, db$bbox)]]
	
	# prepare aux layers and return group label (in case it is not user specified)
	aux_group_def = lapply(aux, function(a) {
		FUNaux_prep = paste0("tmap", gs, a$mapping.fun, "Prep")
		do.call(FUNaux_prep, list(a = a$args, b = db$bbox, o = o))
	})
	aux_group = mapply(function(a, agd) {
		if (is.na(a$group)) agd else as.character(a$group)
	}, aux, aux_group_def, USE.NAMES = FALSE)
	
	# find lid (layer plot id values) for aux layers
	aux_lid = vapply(aux, function(a) a$lid, FUN.VALUE = numeric(1))
	
	# data frame for layer ids
	q = do.call(rbind, c(lapply(1L:o$ng, function(ig) {
		tmxi = tmx[[ig]]
		nl = length(tmxi$layers)
		lid = vapply(tmxi$layers, function(l) {l$lid}, FUN.VALUE = numeric(1))
		group = vapply(tmxi$layers, function(l) {l$group}, FUN.VALUE = character(1))
		data.frame(gid = ig, glid = 1:nl, lid = lid, group = group)
	}), if (length(aux_lid)) list(data.frame(gid = 0, glid = 1L:length(aux), lid = aux_lid, group = aux_group)) else NULL))
	
	q$lid2 = 0
	qnotnull = (q$lid != 0)
	if (any(qnotnull)) q$lid2[qnotnull] = rank(q$lid[qnotnull])
	
	q = q[order(q$lid2), ]
	q$pane = "tilePane"
	q$pane[q$lid2 > 0] = paste0("tmap", 400 + q$lid2[q$lid2 > 0])
	
	# q data frame:
	# gid = tmap-group counter
	# glid = layer counter inside tmap-group
	# lid = possibly-user-defined layer order number
	# lid2 = same as lid, but 1,2,3,...
	# pane = pane name (for view mode)
	# group = group name (for selecting layers in view mode)
	
	
	do.call(FUNaux, list(o = o, q = q))
	
	
	# plot xtab headers
	if (o$panel.type == "xtab") {
		for (k in 1:o$npages) {
			labrows = o$panel.labels[[1]]
			labcols = o$panel.labels[[2]]
			if (length(labrows) == o$nrows) for (i in 1:o$nrows) do.call(FUNxtab, list(label = labrows[i], facet_row = i, facet_page = k, o = o)) 
			if (length(labcols) == o$ncols) for (j in 1:o$ncols) do.call(FUNxtab, list(label = labcols[j], facet_col = j, facet_page = k, o = o)) 

		}
	}
	
	
	
	for (i in seq_len(nrow(d))) {
		bbx = d$bbox[[i]]
 		if (o$panel.type == "wrap") do.call(FUNwrap, list(label = o$panel.labels[[1]][d$i[i]], facet_row = d$row[i], facet_col = d$col[i], facet_page = d$page[i], o = o)) 
 		if (is.na(d$asp[i])) next 
		do.call(FUNshape, list(bbx = bbx, facet_row = d$row[i], facet_col = d$col[i], facet_page = d$page[i], o = o))
 			
		for (qi in 1L:nrow(q)) {
			gid = q$gid[qi]
			glid = q$glid[qi]
			pane = q$pane[qi]
			group = q$group[qi]
			if (gid > 0) {
				# data layer
				bl = tmx[[gid]]$layers[[glid]]
				shpTM = get_shpTM(bl$shpDT, d$by1[i], d$by2[i], d$by3[i])[[1]]
				mdt = get_dt(bl$mapping_dt, d$by1[i], d$by2[i], d$by3[i])
				
				id = paste0("f", sprintf("%03d", i), "g", sprintf("%02d", gid), "l", sprintf("%02d", glid))
				
				if (nrow(mdt) != 0) {
					gp = bl$gp
					
					FUN = paste0("tmap", gs, bl$mapping_fun)
					
					do.call(FUN, list(shpTM = shpTM, dt = mdt, gp = gp, bbx = bbx, facet_col = d$col[i], facet_row = d$row[i], facet_page = d$page[i], id = id, pane = pane, group = group, o = o))
				}
				
			} else {
				# aux layer
				a = aux[[glid]]							
				FUNaux_plot = paste0("tmap", gs, a$mapping.fun)
				id = paste0("aux", sprintf("%03d", glid))
				do.call(FUNaux_plot, list(bi = d$bi[i], bbx = bbx, facet_col = d$col[i], facet_row = d$row[i], facet_page = d$page[i], id = id, pane = pane, group = group, o = o))
				
			}
		}
		do.call(FUNoverlay, list(facet_row = d$row[i], facet_col = d$col[i], facet_page = d$page[i], o = o))
	}

	
	
	
	


	

	
	
	is_in = cdt$class == "in"
	if (any(is_in)) {
		legs_in = lapply(which(is_in), function(i) {
			d2 = data.table::copy(d)
			legsi = cdt[i, ]
			if (o$type != "grid" && o$nrows == 1) {
				# reverse above
				d2[, by2 := by1]
				d2[, by1 := 1]
			}
			
			if (is.na(legsi$by1__)) d2[, by1:= NA]
			if (is.na(legsi$by2__)) d2[, by2:= NA]
			if (is.na(legsi$by3__)) d2[, by3:= NA]
			legsi = merge(legsi, d2[, c("by1", "by2", "by3", "row", "col"), with = FALSE], by.x = c("by1__", "by2__", "by3__"), by.y = c("by1", "by2", "by3"))
			legsi[, ':='(facet_row = as.character(row), facet_col = as.character(col), row = NULL, col = NULL)]
			legsi
		})
		cdt = data.table::rbindlist(c(list(cdt[!is_in]), legs_in))
	}
	

	legfun = paste0("tmap", gs, "Legend")
	
	toI = function(x) {
		as.integer(strsplit(x, split = "_")[[1]])
	}
	
	
	if (nrow(cdt) > 0L) for (k in seq_len(o$npages)) {
		klegs = cdt[is.na(by3__) | (by3__ == k), ]
		klegs[, pos.h.id := pos.h][pos.h %in% c("left", "center", "right"), pos.h.id:="lower"][pos.h %in% c("LEFT", "CENTER", "RIGHT"), pos.h.id:="upper"]
		klegs[, pos.v.id := pos.v][pos.v %in% c("top", "center", "bottom"), pos.v.id:="lower"][pos.v %in% c("TOP", "CENTER", "BOTTOM"), pos.v.id:="upper"]
		klegs[, id:=paste(pos.h.id, pos.v.id, sep = "__")]
		
		klegs[, do.call(legfun, args = list(comp = .SD$comp, o = o, facet_row = toI(.SD$facet_row[1]), facet_col = toI(.SD$facet_col[1]), facet_page = k, class = .SD$class[1], stack = .SD$stack, pos.h = .SD$pos.h, pos.v = .SD$pos.v)), by = list(facet_row, facet_col, id), .SDcols = c("comp", "facet_row", "facet_col", "class", "stack", "pos.h", "pos.v")]
	}
	
	do.call(FUNrun, list(o = o))
}
