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

		space.color = space.color %--% attr.color
		earth_boundary.color = earth_boundary.color %--% attr.color
		legend.text.color =  legend.text.color %--% attr.color
		legend.title.color = legend.title.color %--% attr.color
		title.color = title.color %--% attr.color
		frame.color = frame.color %--% attr.color
		legend.frame.color = legend.frame.color %--% attr.color
		panel.label.frame.color = panel.label.frame.color %--% attr.color


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
		frame.color = do.call("process_color", c(list(col=frame.color), pc))
		legend.frame.color = do.call("process_color", c(list(col=legend.frame.color), pc))

		panel.label.color = do.call("process_color", c(list(col=panel.label.color), pc))
		panel.label.bg.color = do.call("process_color", c(list(col=panel.label.bg.color), pc))
		panel.label.frame.color = do.call("process_color", c(list(col=panel.label.frame.color), pc))

		earth_boundary.color = do.call("process_color", c(list(col=earth_boundary.color), pc))

		if (is.na(bg.color) && pc$sepia_intensity != 0) bg.color = "#FFFFFF"
		if (is.na(outer.bg.color) && pc$sepia_intensity != 0) outer.bg.color = "#FFFFFF"

		bg.color = do.call("process_color", c(list(col=bg.color), pc))
		outer.bg.color = do.call("process_color", c(list(col=outer.bg.color), pc))

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
