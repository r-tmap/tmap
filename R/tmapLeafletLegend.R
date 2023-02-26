leaflet_pos = function(pos) {
  paste(unlist(pos[c("pos.v", "pos.h")]), collapse = "")
}


gp_to_lpar = function(gp, mfun, pick_middle = TRUE) {
	# create a list of gp elements
	lst = c(list(fillColor = gp$fill,
				 color = gp$col,
				 fillOpacity = gp$fill_alpha,
				 opacity = gp$col_alpha,
				 'stroke-width' = {if (!all(is.na(gp$lwd))) gp$lwd else 0},
				 size = {if (!all(is.na(gp$size))) gp$size else 1},
				 shape = {if (!all(is.na(gp$shape))) gp$shape else 21}))
	
	lst_isnum = c(fillColor = FALSE, 
				  color = FALSE, 
				  fillOpacity = TRUE,
				  opacity = TRUE, 
				  'stroke-width' = TRUE, 
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
				"circle", "circle", "rect", "diamond", "triangle", "polygon") # shapes for pch 0:25 (NOTE: last one is a triangle upside-down. Since 21:25 are the defaults, and a polygon is chosen to differentiate from the other triangle)
	lst$shape = pch2shp[lst$shape + 1]
	if (mfun == "Lines") lst$shape = "line"

	lst$width = lst$size * 20
	lst$height = lst$size * 20
	lst$width[]
	lst$size = NULL
	lst
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

	if (cmp$type == "gradient") {
		vary = if ("fill" %in% cmp$varying) "fillColor" else "col"
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
		lf |> leaflegend::addLegendNumeric(position=legpos, 
										   orientation = orientation,
										   group = group,
										   height = 200,
										   pal=pal,
										   values=val, 
										   #na.label = textNA, 
										   title=title, 
										   fillOpacity=cmp$gp3$alpha, 
										   layerId = layerId,
										   className = leg_className)

	} else {
		vary = if ("fill" %in% cmp$varying) "fill" else if ("col" %in% cmp$varying) "col" else NA
		
		symbols = do.call(Map, c(list(f = leaflegend::makeSymbol), cmp$gp2))
		
		# alternative:
		# names(cmp$gp2)[names(cmp$gp2) == 'stroke-width'] = "strokeWidth"
		# cmp$gp2$baseSize = 20
		# symbols = do.call(leaflegend::makeSymbolIcons, cmp$gp2)$iconUrl
		
		lf |> leaflegend::addLegendImage(symbols, 
										 labels = lab,
										 width = cmp$gp2$width,
										 height = cmp$gp2$height, 
										 position = legpos,
										 orientation = orientation,
										 labelStyle = "font-size: 18px; vertical-align: middle;",
										 title = htmltools::tags$div(
										 	title,
										 	style = 'font-size: 18px; text-align: left; margin-bottom: 5px;'),
										 layerId = layerId,
										 className = leg_className)
	}

}


tmapLeafletLegPlot.tm_legend_standard_portrait = function(cmp, lf, o) {
	tmapLeaflet_legend(cmp, lf, o, orientation = "vertical")
}

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
