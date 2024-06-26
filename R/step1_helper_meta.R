preprocess_meta_step1 = function(o) {
	within(o, {
		
		pc = list(sepia.intensity=color.sepia.intensity, saturation=color.saturation, color.vision.deficiency.sim=color.vision.deficiency.sim)
		color.sepia.intensity = NULL
		color.saturation = NULL
		color.vision.deficiency.sim = NULL
		
		# title.size = title.size * scale
		# legend.title.size = legend.title.size * scale
		# legend.text.size = legend.text.size * scale
		# 
		# panel.label.size = panel.label.size * scale
		
		space.color = ifelse(is.null(space.color), bg.color, space.color[1])
		earth.boundary.color = ifelse(is.null(earth.boundary.color), attr.color, earth.boundary.color[1])
		legend.text.color =  ifelse(is.null(legend.text.color), attr.color, legend.text.color[1])
		legend.title.color = ifelse(is.null(legend.title.color), attr.color, legend.title.color[1])
		title.color = ifelse(is.null(title.color), attr.color, title.color[1])
		
		legend.inside.box = if (!is.logical(legend.frame)) {
			TRUE
		} else {
			legend.frame
		}
		
		if (isTRUE(title.bg.color)) {
			title.bg.color = bg.color
		}
		
		if (isTRUE(frame)) {
			frame = attr.color 
		} else if (isFALSE(frame)) {
			frame = NA
		}
		
		if (is.logical(legend.frame)) {
			if (isTRUE(legend.frame)) {
				legend.frame = attr.color
			}  else {
				legend.frame = NA 
			} 
		}
		# 		
		# 		between.margin.in <- convertHeight(unit(between.margin, "lines") * scale, "inch", valueOnly=TRUE)
		# 		
		# 		between.margin.y <-convertHeight(unit(between.margin.in, "inch"), "npc", valueOnly=TRUE) * gf$nrow
		# 		between.margin.x <-convertWidth(unit(between.margin.in, "inch"), "npc", valueOnly=TRUE) * gf$ncol
		# 		
		
		outer.margins <- rep(outer.margins, length.out = 4)
		
		inner.margins.extra = rep(inner.margins.extra, length.out = 4)
		
		inner.margins = if (is.list(inner.margins)) {
			lapply(inner.margins, function(im) {
				rep(im, length.out = 4) + inner.margins.extra
			})
		} else {
			rep(inner.margins, length.out = 4) + inner.margins.extra
		}
		inner.margins.extra = NULL
		
		attr.color.light = is_light(attr.color)
		
		title.color = do.call("process_color", c(list(col=title.color), pc))
		#main.title.color = do.call("process_color", c(list(col=main.title.color), pc))
		legend.text.color = do.call("process_color", c(list(col=legend.text.color), pc))
		legend.title.color = do.call("process_color", c(list(col=legend.title.color), pc))
		if (!is.na(frame)) frame = do.call("process_color", c(list(col=frame), pc))
		if (!is.na(legend.frame)) legend.frame = do.call("process_color", c(list(col=legend.frame), pc))
		
		panel.label.color = do.call("process_color", c(list(col=panel.label.color), pc))
		panel.label.bg.color = do.call("process_color", c(list(col=panel.label.bg.color), pc))
		
		earth.boundary.color = do.call("process_color", c(list(col=earth.boundary.color), pc))
		
		if (is.na(bg.color) && pc$sepia.intensity != 0) bg.color = "#FFFFFF"
		bg.color = do.call("process_color", c(list(col=bg.color), pc))
		
		if (!is.null(outer.bg.color)) outer.bg.color = do.call("process_color", c(list(col=outer.bg.color), pc))
		
		if (is.na(legend.bg.color)) legend.bg.color = !is.na(legend.frame)
		if (!is.na(legend.bg.color)) {
			legend.bg.color = if (isFALSE(legend.bg.color)) {
				NA
			} else if (isTRUE(legend.bg.color)) {
				bg.color
			} else {
				do.call("process_color", c(list(col=legend.bg.color, alpha=legend.bg.alpha), pc))				}
		} 
		if (!is.na(title.bg.color)) title.bg.color = do.call("process_color", c(list(col=title.bg.color, alpha=title.bg.alpha), pc))
		if (!is.na(earth.boundary.color)) earth.boundary.color = do.call("process_color", c(list(col=earth.boundary.color), pc))
		space.color = do.call("process_color", c(list(col=space.color), pc))
		
		earth.bounds = if (is.logical(earth.boundary)) {
			c(-180, -90, 180, 90)
		} else {
			as.vector(bb(earth.boundary))
		}
		earth.boundary = !isFALSE(earth.boundary)
		
		#earth.boundary.lwd = earth.boundary.lwd * scale
		#frame.lwd = frame.lwd * scale
		
		# set font face and family
		
		
		## inherit values
		
		# fontface
		for (nm in names(o)[grep("fontface", names(o), fixed = TRUE)]) if (is.null(get(nm))) assign(nm, text.fontface)
		for (nm in names(o)[grep("fontfamily", names(o), fixed = TRUE)]) if (is.null(get(nm))) assign(nm, text.fontfamily)
		
		#for (nm in names(o)[c(grep("lwd", names(o), fixed = TRUE), grep("size", names(o), fixed = TRUE))]) assign(nm, get(nm) * scale)

		# if (is.null(legend.title.fontface)) legend.title.fontface = text.fontface
		# if (is.null(legend.title.fontfamily)) legend.title.fontfamily = text.fontfamily
		# 
		# if (is.null(legend.text.fontface)) legend.text.fontface = text.fontface
		# if (is.null(legend.text.fontfamily)) legend.text.fontfamily = text.fontfamily
		# 
		# if (is.null(title.fontface)) title.fontface = text.fontface
		# if (is.null(title.fontfamily)) title.fontfamily = text.fontfamily
		# 
		# if (is.null(main.title.fontface)) main.title.fontface = text.fontface
		# if (is.null(main.title.fontfamily)) main.title.fontfamily = text.fontfamily
		# 
		# if (is.null(panel.label.fontface)) panel.label.fontface = text.fontface
		# if (is.null(panel.label.fontfamily)) panel.label.fontfamily = text.fontfamily
		
		
	})
}
