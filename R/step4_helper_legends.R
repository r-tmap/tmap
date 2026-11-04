step4_plot_collect_charts = function(tmx) {
	dt_template = data.table::data.table(by1__ = integer(0), by2__ =  integer(0), by3__ =  integer(0), comp = list())
	if (!length(tmx)) {
		crts = dt_template
	} else {
		crts = data.table::rbindlist(c(list(dt_template), lapply(tmx, function(tmxi) {
			data.table::rbindlist(lapply(tmxi$layers, function(tml) {

				crts_cached = get("charts", .TMAP)

				legs = c(tml$trans_legend, tml$mapping_legend)

				legs = lapply(legs, function(l) {
					#l$comp = list(crts_cached[l$crtnr])
					l[, comp:=list(crts_cached[l$crtnr])]
					l[, intersect(names(l), names(dt_template)), with = FALSE]
				})
				data.table::rbindlist(legs, fill = TRUE)
			}), fill = TRUE)
		})), fill = TRUE)
		# remove empty legends
		crts = crts[vapply(crts$comp, length, FUN.VALUE = integer(1)) > 1, ]
	}
	crts
}

step4_plot_collect_legends = function(tmx, o = NULL) {
	# collect legends
	dt_template = data.table::data.table(by1__ = integer(0), by2__ =  integer(0), by3__ =  integer(0), legend = list())

	if (!length(tmx)) {
		legs = dt_template
	} else {
		legs = data.table::rbindlist(c(list(dt_template), lapply(tmx, function(tmxi) {
			data.table::rbindlist(lapply(tmxi$layers, function(tml) {

				legs_cached = get("legs", .TMAP)

				legs = c(tml$trans_legend, tml$mapping_legend)

				legs = lapply(legs, function(l) {
					l[, legend:=list(legs_cached[l$legnr])]
					l
				})

				# make vneutral explicit (as a new column)
				legs2 = lapply(legs, function(legs_aes) {
					legs_aes$vneutral = unlist(lapply(legs_aes$legend, function(l) l$vneutral), use.names = FALSE, recursive = FALSE)
					if ("icon_scale" %in% names(legs_aes$legend[[1]])) {
						legs_aes$icon_scale = legs_aes$legend[[1]]$icon_scale
					} else {
						legs_aes$icon_scale = 1
					}
					legs_aes
				})


				# are icons used? If so set icon_scale
				icon_scales = vapply(legs2, function(l) {
					as.numeric(l$icon_scale[1])
				}, FUN.VALUE = numeric(1))
				icon_scale = if (any(icon_scales != 1)) icon_scales[which(icon_scales != 1)[1]] else 1

				clones = vapply(legs2, function(l) {
					a = l$legend[[1]]$aes
					a %||% ""
				}, FUN.VALUE = character(1))


				# find shared legends
				legs2b = mapply(function(l, lnm) {
					w = which(clones == lnm)
					if (length(w) > 0L) {
						legsclone = lapply(legs2[w], function(li) {
							li$legend
						})
						num_legends = vapply(legsclone, length, numeric(1))
						if (any(num_legends != length(l$legend))) warning("legends could not be shared; the aesthetics need the same .free specification", call. = FALSE)


						l$legend = do.call(mapply, args = c(list(FUN = function(l, ...) {
							clns = list(...)
							k = length(l$vvalues)
							l$clones = lapply(clns, function(cl) {
								vv = cl$vvalues
								if (k != length(vv)) cli::cli_abort("legends could not be shared; the number of legend items is different")
								vv
							})
							names(l$clones) = names(clones[w])
							l
						}, SIMPLIFY = FALSE), c(list(l$legend), legsclone)))
					}
					l
				}, legs2, names(legs2), SIMPLIFY = FALSE)

				# cross legends (tm_legend_cross): the legend of the crossed map variable is attached to the
				# cross legend, and not shown itself
				legs2b = legend_cross_attach(legs2b)

				copy_neutral = (length(legs) > 1)

				legs3 = mapply(function(legs_aes, legnm, i) {
					bvars = names(legs_aes)[substr(names(legs_aes), 1, 2) == "by"]

					if (!is.null(legs_aes)) {
						for (k in 1:nrow(legs_aes)) {
							if (!legs_aes$legend[[k]]$active || !legs_aes$legend[[k]]$show) {
								legs_aes$legend[[k]] = list(NULL)
								next
							}

							bval = legs_aes[k, bvars, with = FALSE]
							gp = tml$gp

							# fill gp values with (scaled) aes data
							gpaid = which(paste0("__", legnm) == unlist(gp, use.names = FALSE))
							for (j in gpaid) gp[[j]] = legs_aes$legend[[k]]$vvalues

							# fill gp with neutral values for other graphical variables
							if (copy_neutral) {
								nvalues = mapply(function(lclone, lname) {
									bvars2 = names(lclone)[substr(names(lclone), 1, 2) == "by"]

									bvars_int = intersect(bvars, bvars2)
									lc = if (!length(bvars_int)) lclone[1] else lclone[bval, on = bvars_int]

									# legends of pattern_col, pattern_size, and pattern_density: show the first
									# pattern of the pattern legend (the neutral pattern is "none")
									pvars_h = c("pattern_col", "pattern_size", "pattern_density")
									if (lname == "pattern" && legnm %in% pvars_h) {
										vv = lc$legend[[1]]$vvalues
										vv = vv[!is.na(vv) & vv != "none"]
										if (length(vv)) return(vv[1])
									}
									lc$vneutral
								}, legs2[-i], names(legs2[-i]), SIMPLIFY = FALSE)


								for (j in seq_along(nvalues)) {
									gpid = which(paste0("__", names(nvalues)[j]) == sapply(gp, "[[", 1))
									for (jj in gpid) gp[[jj]] = nvalues[[j]]
								}

							}

							# other legends (e.g. fill) do not show a pattern when the pattern varies
							# (in pattern itself, or in pattern_col, pattern_size, or pattern_density):
							# it has its own legend
							pvars = c("pattern", "pattern_col", "pattern_size", "pattern_density")
							if (!(legnm %in% pvars) && any(pvars %in% names(legs2))) gp$pattern = "none"

							# legends of pattern_col, pattern_size, and pattern_density without a
							# pattern are not shown
							no_pattern = is.character(gp$pattern) &&
								all(is.na(gp$pattern) | gp$pattern == "none")
							if (legnm %in% pvars[-1] && no_pattern) {
								cli::cli_inform(c(paste("{.field [pattern]} {.arg {legnm}} has no effect, since no",
														"{.arg pattern} is specified (the default is {.str none}):",
														"there is no pattern on the map, and therefore no legend."),
												  i = "Specify a pattern, e.g. {.code pattern = \"dots\"}."),
												.frequency = "once",
												.frequency_id = paste0("pattern_legend_", legnm))
								legs_aes$legend[[k]] = list(NULL)
								next
							}

							leleg = legs_aes$legend[[k]]



							# get gp values from shared legends (slave)
							if ("clones" %in% names(leleg)) {
								clist = leleg$clones
								for (j in 1L:length(clist)) {
									cnm = names(clist)[j]
									gp[[cnm]] = clist[[j]]
								}
							}

							# in case the size legend consists of symbols shapes that represented icons
							if (icon_scale > 1 && legnm == "size") {
								gp$size = gp$size * icon_scale
							}

							# text color and pattern color "auto": contrast with the legend item background
							gp = legend_auto_col(gp, o)

							# cross legend: a grid of the items of this map variable (rows) and of the crossed
							# map variable (columns)
							if (!is.null(leleg$cross_legend)) leleg = legend_cross(leleg, gp, tml$gp, legnm)

							leleg$gp = gp
							if (!is.null(leleg$cross_gp)) {
								leleg$gp = leleg$cross_gp
								leleg$cross_gp = NULL
							}
							leleg$vneutral = NULL
							leleg$vvalues = NULL

							if (length(leleg) == 1) {
								legs_aes$legend[[k]] = list(leleg)
							} else {
								legs_aes$legend[[k]] = leleg
							}
						}
					}
					legs_aes
				}, legs2b, names(legs2b), 1:length(legs2b), SIMPLIFY = FALSE)

				data.table::rbindlist(legs3, fill = TRUE)
			}), fill = TRUE)
		})), fill = TRUE)

		# remove empty legends
		legs = legs[vapply(legs$legend, length, FUN.VALUE = integer(1)) > 1, ][, vneutral := NULL]
	}


	# remove colums and rename legend to comp
	set(legs, j= 5:ncol(legs), value = NULL)
	data.table::setnames(legs, "legend", "comp")
	legs
}
