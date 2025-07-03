preprocess_meta_step1 = function(o, knit_opts = NULL) {
	within(o, {
		# if (!is.null(knit_opts)) {
		# 	scale = scale / knit_opts$fig.retina
		# }

		pc = list(sepia_intensity=color.sepia_intensity, saturation=color.saturation, color_vision_deficiency_sim=color_vision_deficiency_sim)
		color.sepia_intensity = NULL
		color.saturation = NULL
		color_vision_deficiency_sim = NULL

		# attr color is the standard for all component colors
		# attr.color.light is the 'opposite', only used in compass and scalebar
		#   it is white if the attr.color is dark and black if it is light
		attr.color.light = is_light(attr.color)
		attr.color = do.call(process_color, c(list(col = attr.color), pc))
		attr.color_light = if (attr.color.light) "#000000" else "#ffffff"
		attr.color_light = do.call(process_color, c(list(col = attr.color_light), pc))

		# bg.color = NA was used to disable background
		# as of 4.2, bg = FALSE will be used (#1119)

		# however, if a color is explicitly specified, override bg (also apply this to frame)
		for (nm in c("bg", "outer.bg", "frame")) {
			nmc = paste0(nm, ".color")
			if (nmc %in% o$calls && !is.null(get(nmc))) {
				assign(nm, TRUE)
			}
		}

		for (nm in names(o)[grep("bg\\.color$", names(o))]) {
			tmp = get(nm)
			if (!is.null(tmp) && is.na(tmp)) {
				nm2 = gsub("\\.color", "", nm)
				assign(nm, "white") #dummy
				assign(nm2, FALSE)
				cli::cli_alert("{.field [layout options]} use {.code {nm2} = FALSE} instead of {.code {nm} = NA}")
			}
			rm(tmp)
		}


		# color options: replace NA with attr.color
		# process colors: apply sepia and cvd sim
		for (nm in setdiff(names(o)[grep("color(\\.light|\\.dark)?$", names(o))], "attr.color")) {
			assign(nm, local({
				x = get(nm)
				if (is.na(x)) {
					if (length(grep("color(\\.light)$", nm))) attr.color_light else attr.color
				} else do.call("process_color", c(list(col=x), pc))
			}))
		}

		# radius: replace NA with r
		for (nm in names(o)[grep("\\.r$", names(o))]) {
			assign(nm, local({
				x = get(nm)
				if (is.na(x)) r else x
			}))
		}
		if (is.na(legend.settings.landscape$item.r)) legend.settings.landscape$item.r = r
		if (is.na(legend.settings.portrait$item.r)) legend.settings.portrait$item.r = r

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


		panel.label.rot = rep_len(panel.label.rot, 4L)

		earth_boundary.lwd = earth_boundary.lwd * scale

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
