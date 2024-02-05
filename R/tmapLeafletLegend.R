leaflet_pos = function(pos) {
	if (pos$type %in% c("out", "autoout")) {
		sel = c("cell.v", "cell.h")
	} else {
		sel = c("pos.v", "pos.h")
	}
	x = tolower(unlist(pos[sel]))
	
	if (x[1] %in% c("center", "centre")) x[1] = "top"
	if (x[2] %in% c("center", "centre")) x[2] = "left"
	
  paste(x, collapse = "")
}


gp_to_lpar = function(gp, mfun, shape = 20, pick_middle = TRUE) {
	# create a list of gp elements

	lst = c(list(fillColor = {if (!all(is.na(gp$fill))) gp$fill else "#000000"},
				 color = {if (!all(is.na(gp$col))) gp$col else "#000000"},
				 fillOpacity = {if (!all(is.na(gp$fill_alpha))) gp$fill_alpha else 0},
				 opacity = {if (!all(is.na(gp$col_alpha))) gp$col_alpha else 0},
				 'stroke-width' = {if (!all(is.na(gp$lwd))) gp$lwd else 0},
				 'stroke-dasharray' = {if (!all(is.na(gp$lty))) lty2dash(gp$lty) else "none"},
				 size = {if (!all(is.na(gp$size))) gp$size else 1},
				 shape = {if (!all(is.na(gp$shape))) gp$shape else shape}))
	
	lst_isnum = c(fillColor = FALSE, 
				  color = FALSE, 
				  fillOpacity = TRUE,
				  opacity = TRUE, 
				  'stroke-width' = TRUE, 
				  'stroke-dash' = FALSE,
				  size = TRUE, 
				  shape = TRUE)
	
	lst = mapply(function(lsti, isnum) {
		if (!is.character(lsti)) return(lsti)
		
		if (nchar(lsti[1]) > 50) {
			x = cont_split(lsti)
			x = lapply(x, function(i) {
				i[i=="NA"] <- NA
				i
			})
			if (isnum) x = lapply(x, as.numeric)
			if (pick_middle) {
				x = sapply(x, function(i) {
					if (all(is.na(i))) NA else {
						sq = c(5,6,4,7,3,8,2,9,1,10) # priority for middle values
						i[sq[which(!is.na(i)[sq])[1]]]
					}
				})
			}
			return(x)
			
		} else {
			return(lsti)
		}
	}, lst, lst_isnum[names(lst)], SIMPLIFY = FALSE)

	pch2shp = c("rect", "circle", "triangle", "plus", "cross", "diamond", "triangle", 
				"cross", "star", "diamond", "circle", "polygon", "plus", "cross", 
				"triangle", "rect", "circle", "triangle", "diamond", "circle", 
				"circle", "circle", "rect", "diamond", "triangle", "polygon", "stadium") # shapes for pch 0:25 + 26 for stadium (NOTE: last one is a triangle upside-down. Since 21:25 are the defaults, and a polygon is chosen to differentiate from the other triangle)
	lst$shape = get_pch_names(lst$shape)
		
	if (mfun == "Lines") lst$shape = "line"

	lst$width = lst$size * 20
	lst$height = lst$size * 20
	#lst$width[]
	lst$size = NULL
	lst
}



make_equal_list = function(x) {
	cls = class(x)
	n = max(vapply(x, length, integer(1)))
	structure(lapply(x, rep, length.out = n), class = cls)
}


tmapLeaflet_legend = function(cmp, lf, o, orientation) {

	group = "tmp" # TODO
	leg_className = paste("info legend", gsub(" ", "", group, fixed = TRUE))
	layerId =  paste0("legend", sprintf("%02d", .TMAP_LEAFLET$leg_id)) # "legend401" #todo
	.TMAP_LEAFLET$leg_id = .TMAP_LEAFLET$leg_id + 1
	
	# if (length(cmp$gp$col) > 1 || all(is.na(cmp$gp$fill))) {
	# 	pal = cmp$gp$col
	# 	opacity = cmp$gp$col_alpha
	# } else {
	# 	pal = cmp$gp$fill
	# 	opacity = cmp$gp$fill_alpha
	# }
	
	lab = cmp$labels
	val = cmp$dvalues
	title = if (nonempty_text(cmp$title)) expr_to_char(cmp$title) else NULL
	
	legpos = leaflet_pos(cmp$position)

	lf2 = if (cmp$typ == "none") {
		#message("Text based legends not supported in view mode")
		lf
	} else if (cmp$type == "gradient") {
		vary = if ("fill" %in% cmp$varying) "fillColor" else "color"
		#vary_alpha = paste0(vary, "_alpha")
			
		incl.na = cmp$na.show
		if (incl.na) {
			pal = head(cmp$gp2[[vary]], -1)
			colNA = tail(cmp$gp2[[vary]], 1)
			textNA = lab[length(lab)]
		} else {
			pal = cmp$gp2[[vary]]
			colNA = NA
			textNA = NA
		}
		pal = colorNumeric(palette = pal, 
						   domain = val, 
						   na.color=colNA, 
						   alpha = FALSE)
		
		brks = pretty(cmp$limits, 7)
		
		trns = function(x) {
			f = (x-cmp$limits[1]) / diff(cmp$limits)
			y = cmp$tr$fun(cmp$limits)
			x2 = y[1] + diff(y) * f
			cmp$tr$rev(x2)
		}
		
		lf |> leaflegend::addLegendNumeric(position=legpos, 
										   orientation = orientation,
										   group = group,
										   height = {if (orientation == "horizontal") 20 else 200},
										   width = {if (orientation == "vertical") 20 else 200},
										   pal=pal,
										   values=val, 
										   numberFormat = function(x) {
										   	prettyNum(trns(x), format = "f", big.mark = ",", digits =
										   			  	3, scientific = FALSE)
										   },
										   #na.label = textNA, 
										   title=title, 
										   fillOpacity=cmp$gp3$alpha, 
										   layerId = layerId,
										   className = leg_className)

	} else {
		vary = if ("fill" %in% cmp$varying) "fill" else if ("col" %in% cmp$varying) "col" else NA

		

		gp2 = make_equal_list(cmp$gp2)
		#gp2$baseSize = gp2$baseSize[1]
		
		sn = suppressWarnings(as.numeric(gp2$shape))
		sid = which(!is.na(sn))

		#symbols = do.call(Map, c(list(f = leaflegend::makeSymbol), cmp$gp2))
		
		# alternative:
		if (length(sid)) {
			gp2$shape[sid] = "circle" # as dummy
		}
		
		names(gp2)[names(gp2) == 'stroke-width'] = "strokeWidth"
		gp2$baseSize = 20
		#symbols = do.call(leaflegend::makeSymbolIcons, gp2)#$iconUrl
		
		
		symbols = do.call(leaflegend::makeSymbolIcons, gp2)
		
		symbols$iconWidth = gp2$width#, length(symbols$iconUrl))
		symbols$iconHeight = gp2$height#, length(symbols$iconUrl))
		
		if (length(sid)) {
			iconLib <- get("shapeLib", envir = .TMAP)[sn[sid]-999]
			symbols_icons <- merge_icons(iconLib)
			size = gp2$width[sid] / gp2$baseSize
			
			for (i in seq_along(sid)) {
				symbols$iconUrl[sid[i]] = symbols_icons$iconUrl[i]
				symbols$iconWidth[sid[i]] <- symbols_icons$iconWidth[i] * size[i]
				symbols$iconHeight[sid[i]] <- symbols_icons$iconHeight[i] * size[i]
				if (all(c("iconAnchorX", "iconAnchorY") %in% names(symbols_icons))) {
					symbols$iconAnchorX[sid[i]] <- symbols_icons$iconAnchorX[i] * size[i]
					symbols$iconAnchorY[sid[i]] <- symbols_icons$iconAnchorY[i] * size[i]
					
				}
			}
		}
		
		#symbols = symbols$iconUrl
		
		
		
		lf |> leaflegend::addLegendImage(symbols$iconUrl, 
										 labels = lab,
										 width = symbols$iconWidth,
										 height = symbols$iconHeight, 
										 position = legpos,
										 orientation = orientation,
										 labelStyle = "font-size: 14px; vertical-align: middle; margin: 0px;",
										 title = htmltools::tags$div(
										 	title,
										 	style = 'font-size: 14px; text-align: left; margin-bottom: 5px;'),
										 layerId = layerId,
										 className = leg_className)
	}
	backg <- htmltools::tags$style(paste0("#", layerId, " { background: ", substr(cmp$bg.color,1,7), "; opacity: ", cmp$bg.alpha, "}")) 
	lf2 |> htmlwidgets::prependContent(backg)

}

#' @exportS3Method NULL
tmapLeafletLegPlot.tm_legend_standard_portrait = function(cmp, lf, o) {
	tmapLeaflet_legend(cmp, lf, o, orientation = "vertical")
}

#' @exportS3Method NULL
tmapLeafletLegPlot.tm_legend_standard_landscape = function(cmp, lf, o) {
	tmapLeaflet_legend(cmp, lf, o, orientation = "horizontal")
}


tmapLeafletLegend = function(comp, o, facet_row = NULL, facet_col = NULL, facet_page, class, stack, stack_auto, pos.h, pos.v, bbox) {
	lf = get_lf(facet_row, facet_col, facet_page)
	
	rc_text = frc(facet_row, facet_col)
	

	for (cmp in comp) {
		lf = tmapLeafletLegPlot(cmp, lf, o)
	}
		


	assign_lf(lf, facet_row, facet_col, facet_page)
	NULL	
}
