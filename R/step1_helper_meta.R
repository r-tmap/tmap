preprocess_meta_step1 = function(o) {
	within(o, {

		pc = list(sepia_intensity=color.sepia_intensity, saturation=color.saturation, color_vision_deficiency_sim=color_vision_deficiency_sim)
		color.sepia_intensity = NULL
		color.saturation = NULL
		color_vision_deficiency_sim = NULL

		# color options: replace NA with attr.color
		# process colors: apply sepia and cvd sim
		for (nm in names(o)[grep("color(\\.light|\\.dark)?$", names(o))]) {
			assign(nm, local({
				x = get(nm)
				if (is.na(x)) x = attr.color
				do.call("process_color", c(list(col=x), pc))
			}))
		}

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
		panel.label.rot = rep_len(panel.label.rot, 4L)

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
