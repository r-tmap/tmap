tmapLeafletLegPlot.tm_legend_standard_landscape = function(cmp, lf, o) {
	tmapLeafletLegPlot.tm_legend_standard_portrait(cmp, lf, o)
}


tmapLeafletLegPlot.tm_legend_standard_portrait = function(cmp, lf, o) {
	
	
	group = "tmp" # TODO
	leg_className = paste("info legend", gsub(" ", "", group, fixed = TRUE))
	layerId =  paste0("legend", sprintf("%02d", .TMAP_LEAFLET$leg_id)) # "legend401" #todo
	.TMAP_LEAFLET$leg_id = .TMAP_LEAFLET$leg_id + 1
	
	if (length(cmp$gp$col) > 1) {
		pal = cmp$gp$col
		opacity = cmp$gp$col_alpha
	} else {
		pal = cmp$gp$fill
		opacity = cmp$gp$fill_alpha
	}
	
	lab = cmp$labels
	val = cmp$dvalues
	title = cmp$title
	
	legpos = paste(unlist(cmp$position[c("pos.v", "pos.h")]), collapse = "")
	
	is_cont = (nchar(pal[1]) > 10)
	
	
	if (is_cont) {
		incl.na <- nchar(pal[length(pal)]) < 10
		
		orig <- unlist(lapply(pal, function(x) {
			p <- strsplit(x, split = "-", fixed=TRUE)[[1]]
			if (length(p) == 1) NULL else p[p!="NA"]
		}), use.names = FALSE)
		
		pal <- vapply(pal, function(x) {
			p <- strsplit(x, split = "-", fixed=TRUE)[[1]]
			if (length(p)==1) p[1] else if (p[6]=="NA") p[5] else p[6]
		}, character(1))
		
		if (incl.na) {
			colNA <- unname(pal[length(pal)])
			pal <- pal[-length(pal)]
			textNA <- lab[length(lab)]
		} else {
			colNA <- NA
			textNA <- NA
		}
		
		legvals <- if (!is.na(colNA)) c(val, NA) else val
		
		lf |> addLegend(position=legpos, group = group,
							 pal=colorNumeric(palette = pal, 
							 				 domain = val, 
							 				 na.color=colNA, 
							 				 alpha = FALSE), 
							 values=legvals, 
							 na.label = textNA, title=title, opacity=opacity, layerId = layerId,
							 className = leg_className)
	} else {
		# if (length(pal) != length(val)) {
		# 	colNA <- pal[length(pal)]
		# 	textNA <- lab[length(pal)]
		# 	pal <- pal[-length(pal)]
		# 	lab <- lab[-length(lab)]
		# } else {
		# 	colNA <- NA
		# 	textNA <- NA
		# }
		# orig <- pal
		# 
		# if (!is.na(colNA)) {
		# 	legvals <- c(lab, textNA)
		# 	pal <- c(pal, colNA)
		# } else {
		# 	legvals <- lab
		# }
		
		pal = rep(pal, length.out = length(lab))
		
		if (length(opacity == length(lab))) {
			pal = paste0(pal, num_to_hex(opacity * 255))
		}
		
		legvals = lab
		#print(layerId)
		lf |> addLegend(position=legpos,
				  group = group,
				  colors = pal,
				  labels = legvals,
				  title=title,
				  opacity=opacity,
				  layerId = layerId,
				  className = leg_className)
		
	}
	
	
}



tmapLeafletLegend = function(comp, o, facet_row = NULL, facet_col = NULL, facet_page, class, stack, stack_auto, pos.h, pos.v) {
	lf = get_lf(facet_row, facet_col, facet_page)
	
	rc_text = frc(facet_row, facet_col)
	

	for (cmp in comp) {
		lf = tmapLeafletLegPlot(cmp, lf, o)
	}
		


	assign_lf(lf, facet_row, facet_col, facet_page)
	NULL	
}
