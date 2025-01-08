preprocess_meta_step1 = function(o) {
	within(o, {

		pc = list(sepia_intensity=color.sepia_intensity, saturation=color.saturation, color_vision_deficiency_sim=color_vision_deficiency_sim)
		color.sepia_intensity = NULL
		color.saturation = NULL
		color_vision_deficiency_sim = NULL

		# title.size = title.size * scale
		# legend.title.size = legend.title.size * scale
		# legend.text.size = legend.text.size * scale
		#
		# panel.label.size = panel.label.size * scale

		space.color = ifelse(is.null(space.color), bg.color, space.color[1])
		earth_boundary.color = ifelse(is.null(earth_boundary.color), attr.color, earth_boundary.color[1])
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
		# 		between_margin.in <- convertHeight(unit(between_margin, "lines") * scale, "inch", valueOnly=TRUE)
		#
		# 		between_margin.y <-convertHeight(unit(between_margin.in, "inch"), "npc", valueOnly=TRUE) * gf$nrow
		# 		between_margin.x <-convertWidth(unit(between_margin.in, "inch"), "npc", valueOnly=TRUE) * gf$ncol
		#

		outer.margins = rep(outer.margins, length.out = 4)

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

		earth_boundary.color = do.call("process_color", c(list(col=earth_boundary.color), pc))

		if (is.na(bg.color) && pc$sepia_intensity != 0) bg.color = "#FFFFFF"
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
		if (!is.na(earth_boundary.color)) earth_boundary.color = do.call("process_color", c(list(col=earth_boundary.color), pc))
		space.color = do.call("process_color", c(list(col=space.color), pc))

		earth.bounds = if (is.logical(earth_boundary)) {
			c(-180, -90, 180, 90)
		} else {
			as.vector(bb(earth_boundary))
		}
		earth_boundary = !isFALSE(earth_boundary)

		earth_boundary.lwd = earth_boundary.lwd * scale
		#frame.lwd = frame.lwd * scale

		# set font face and family


		## inherit values

		# fontface
		for (nm in names(o)[grep("fontface", names(o), fixed = TRUE)]) if (is.null(get(nm))) assign(nm, text.fontface)
		for (nm in names(o)[grep("fontfamily", names(o), fixed = TRUE)]) if (is.null(get(nm))) assign(nm, text.fontfamily)

		# special case: fontface is a visual variable. Therefore, check if value.const etc is NULL if so, replace
		if (is.null(value.const$fontface)) value.const$fontface = text.fontface
		if (is.null(value.blank$fontface)) value.blank$fontface = text.fontface
		if (is.null(value.na$fontface)) value.na$fontface = text.fontface
		if (is.null(value.null$fontface)) value.null$fontface = text.fontface



	})
}
