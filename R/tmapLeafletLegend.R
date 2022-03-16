tmapLeafletLegend = function(comp, o, facet_row = NULL, facet_col = NULL, facet_page, class, stack, pos.h, pos.v) {
	lf = get_lf(facet_row, facet_col, facet_page)
	
	rc_text = frc(facet_row, facet_col)
	
	
	
	length(comp)
	for (cmp in comp) {
		group = "tmp" # TODO
		leg_className = paste("info legend", gsub(" ", "", group, fixed = TRUE))
		layerId = "legend401" #todo
		
		if (length(cmp$gp$col) > 1) {
			col = cmp$gp$col
			opacity = cmp$gp$col_alpha
		} else {
			col = cmp$gp$fill
			opacity = cmp$gp$fill_alpha
		}
		legvals = cmp$labels
		title = cmp$title

		legpos = paste(unlist(cmp$position[c("pos.v", "pos.h")]), collapse = "")
		
		lf |> 
			addLegend(position=legpos,
					  group = group,
					  colors = col,
					  labels = legvals,
					  title=title,
					  opacity=opacity,
					  layerId = layerId,
					  className = leg_className) |> 
			assign_lf(facet_row, facet_col, facet_page)
	}
	
	NULL	
}
