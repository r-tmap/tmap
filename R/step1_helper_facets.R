# function to update the levels fl per facet dimension
update_grp_vars = function(lev = NULL, m = NULL) {
	vl = get("vl", envir = .TMAP)
	vn = get("vn", envir = .TMAP)

	if (!missing(lev)) {
		m = length(lev)
		if (m == 1L && vn == 1L) {
			if (is.null(vl)) {
				vl = lev
				vn = length(lev)
			}
		} else if (m > 1L && vn > 1L && m != vn) {
			stop("Inconsistent number of map variable values.", call. = FALSE)
		} else if (m > vn) {
			vl = lev
			vn = length(lev)
		}
	} else if (!missing(m)) {
		if (m > 1L && vn > 1L && m != vn) {
			stop("Inconsistent number of map variable values.", call. = FALSE)
		} else if (m > vn) {
			vn = m
			vl = NULL
		}
	}
	assign("vl", vl, envir = .TMAP)
	assign("vn", vn, envir = .TMAP)
}

add_used_vars = function(v, convert2density = FALSE, is_title = FALSE) {
	if (convert2density) {
		c2d_vars = get("c2d_vars", envir = .TMAP)
		c2d_vars = unique(c(c2d_vars, v))
		assign("c2d_vars", c2d_vars, envir = .TMAP)
	}

	used_vars = get("used_vars", envir = .TMAP)
	used_vars = unique(c(used_vars, v))

	if (is.null(attr(used_vars, "is_title"))) {
		attr(used_vars, "is_title") = is_title
	} else {
		attr(used_vars, "is_title") = attr(used_vars, "is_title") || is_title
	}

	assign("used_vars", used_vars, envir = .TMAP)
}

get_split_stars_dim = function(lst) {
	if (length(lst)) {
		y = vapply(lst, function(x) x$split_stars_dim, FUN.VALUE = character(1))
		if (any(y != "")) y[which(y != "")[1]] else ""
	} else {
		""
	}
}

is_rev = function(x) !is.null(x) && substr(x, 1, 1) == "-"
remove_min = function(x) substr(x, 2, nchar(x))

# ## estimate number of facets
step1_rearrange_facets = function(tmo, o) {
	nframes = NULL

	#o = tmap_options_mode()
	dev = getOption("tmap.devel.mode")


	# get the final tm_faets object (ignoring group specific args: is.wrap, by, rows, columns, pages)

	#fl = list(1L, 1L, 1L)
	#assign("fl", fl, envir = .TMAP)

	#assign("fl", list(NULL, NULL, NULL), envir = .TMAP)
	#assign("fn", c(1L, 1L, 1L), envir = .TMAP)

	tmo = lapply(tmo, function(tmg) {

		shp = tmg$tms$shp
		smeta = tmapGetShapeMeta1(shp, c(o, tmg$tmf))

		if (dev) timing_add(s3 = "get_shape_meta1")


		assign("vl", NULL, envir = .TMAP)
		assign("vn", 1L, envir = .TMAP)

		assign("used_vars", character(0), envir = .TMAP)
		assign("c2d_vars", character(0), envir = .TMAP)

		precheck_aes = function(a, layer, shpvars, args) {
			b = within(a, {
				if (inherits(value, "tmapAsIs")) {
					if (inherits(scale, "tm_scale_auto")) {
						class(scale) = c("tm_scale_asis", "tm_scale", "list")
						scale$FUN = tmapScaleAsIs
					}
				}

				# check if dimension is or should be used
				if (inherits(value, "tmapVars") && (length(smeta$dims) != 0) && (!is.null(value$dimvalues) || (!is.na(value$x[1]) && length(value$x) == 1L) || (value$multivariate && length(shpvars) == 1L))) {
					if (is.na(value$x)[1]) {
						value$x = smeta$dims[1]
					} else {
						if (length(value$x) > 1L) {
							if (identical(value$n, -1)) { # shortcut used for v3 compat: tm_rgb(r=3,g=2,=b1) when not known if shp had a band
								v3_tm_rgb(value$x[1], value$x[2], value$x[3], dim = smeta$dims[1])

								value$dimvalues = value$x
								value$x = smeta$dims[1]
								value$n = NA
							} else {
								warning("dimvalues specified while more than one dimension name is specified. Only the first will be used", call. = FALSE)
								value$x = value$x[1]
							}
						}
						if (!(value$x %in% smeta$dims)) {
							stop("dimvalues specified, but dimension \"" , x, "\" not found. Available dimension name(s): ", paste(smeta$dims,collapse = ", "), call. = FALSE)
						}
					}
					if (is.null(value$dimvalues)) {
						if (!is.na(value$n)) {
							value$dimvalues = smeta$dims_vals[[value$x]][1L:value$n]
						} else {
							if (!all(value$dimvalues) %in% smeta$dims_vals[[value$x]]) stop("Incorrect dimvalues", call. = FALSE)
						}
					}

					split_stars_dim = value$x
					add_used_vars(value$dimvalues)

					# redefine value for step 2
					if (value$multivariate) {
						update_grp_vars(lev = value$x)
						value = structure(list(as.character(value$dimvalues)), names = paste(as.character(value$dimvalues), collapse = "_"), class = "tmapVars")
					} else {
						update_grp_vars(lev = value$dimvalues)
						value = structure(as.list(as.character(value$dimvalues)), names = as.character(value$dimvalues), class = "tmapVars")
					}

					data_vars = TRUE
					geo_vars = FALSE

				} else {
					split_stars_dim = ""

					#value_orig = value # just for the case of L156
					if (!inherits(value, c("tmapOption", "tmapVars", "tmapAsIs", "tmapSpecial")) && length(value) && is.na(value[[1]][1])) {
						# NA -> value.blank
						value = tmapVV(getAesOption("value.blank", o, aes = aes, layer = layer))
					}

					if (inherits(value, "tmapOption")) {
						#value_orig = tmapVV(getAesOption(value[[1]], o, aes = aes, layer = layer))
						value = tmapVV(getAesOption(value[[1]], o, aes = aes, layer = layer))
						data_vars = FALSE
						geo_vars = FALSE
						#if (!is.list(value_orig)) value = list(value_orig)
						#value = value_orig
						#names(value) = sapply(value, "[", 1)
					} else if (inherits(value, "tmapVars")) {
						if (identical(value$n, -1)) { # shortcut used for v3 compat: tm_rgb(r=3,g=2,=b1) when not known if shp had a band
							v3_tm_rgb(value$x[1], value$x[2], value$x[3])
						}

						if (!is.null(value$dimvalues) && length(smeta$dims) == 0L) {
							error_dimvalues()
						}
						if (!is.na(value$x[1])) {
							if (is.character(value$x)) {
								if (!all(value$x %in% shpvars)) stop("not all variables specified in tm_vars are found", call. = FALSE)
								vars = value$x
							} else {
								if (!all(value$x %in% 1L:length(shpvars))) stop("tm_vars defined for x = ", paste(value$ids, collapse = ", "), " while there are only ", length(shpvars), " variables", call. = FALSE)
								vars = shpvars[value$x]
							}
						} else if (!is.na(value$n)) {
							if (length(shpvars) < value$n) {
								stop("tm_vars defined for n = ", value$n, " while there are only ", length(shpvars), " variables", call. = FALSE)
							}
							vars = shpvars[1L:value$n]
						} else {
							vars = shpvars
						}
						names(vars) = vars

						if (value$animate) {
							.TMAP$animate = c(.TMAP$animate, vars)
						}

						if (value$multivariate) {
							value = structure(list(unname(vars)), names = paste(vars, collapse = "_"), class = "tmapStandard")
						} else {
							value = structure(as.list(vars), class = "tmapStandard")
						}

						data_vars = TRUE
						geo_vars = FALSE
					} else {
						if (inherits(value, "tmapStandard")) {
							uvalue = unlist(value)
							data_vars = all(uvalue %in% shpvars)
							geo_vars = all(uvalue %in% c("AREA", "LENGTH", "MAP_COLORS")) && !data_vars
							if (data_vars || geo_vars) vars = uvalue else vars = character(0)
						} else {
							data_vars = FALSE
							geo_vars = FALSE
							vars = character(0)
						}
					}

					nvars = length(value) #m
					nvari = vapply(value, length, integer(1))

					convert2density = "convert2density" %in% names(scale) && scale$convert2density

					nflvar = nvars
					if (data_vars) {
						flvar = names(value)
						update_grp_vars(lev = flvar)
						add_used_vars(vars, convert2density = convert2density)
					} else if (geo_vars) {
						flvar = names(value)
						update_grp_vars(lev = flvar)
						add_used_vars(vars)
					} else {
						mfun = paste0("tmapValuesSubmit_", aes)
						if (exists(mfun)) {
							value = do.call(mfun, list(x = value, args = args))
						}
						nvars = length(value)
						nflvar = nvars
						nvari = vapply(value, length, integer(1))
						#value =

						#value = value_orig
						update_grp_vars(m = nflvar)
					}
				}
			})
			b
		}

		gs = tmap_graphics_name()

		# preprocess layers: check aes values
		tmg$tmls = lapply(tmg$tmls, function(tml) {
			within(tml, {
				if (!exists(paste0("tmap", gs, mapping.fun))) {
					mode = getOption("tmap.mode")

					modes = get_modes()
					gss = vapply(modes, tmap_graphics_name, FUN.VALUE = character(1))

					exs = vapply(1L:length(modes), function(i) {
						exists(paste0("tmap", gss[i], mapping.fun))
					}, FUN.VALUE = logical(1))
					modes2 = modes[exs]

					if (length(modes2)) {
						cli::cli_abort("Map layer {.code tm_{layer}} not available for mode {.str {mode}}. This map layer is only available for modes {.str {modes2}}")
					} else {
						cli::cli_abort("Map layer {.code tm_{layer}} not available for mode {.str {mode}} and also not for the other modes")
					}
				}

				if (length(trans.aes)) trans.aes = lapply(trans.aes, precheck_aes, layer = tml$layer, shpvars = smeta$vars, args = trans.args)
				if (length(mapping.aes)) mapping.aes = lapply(mapping.aes, precheck_aes, layer = tml$layer, shpvars = smeta$vars, args = mapping.args)

				# get first non-empty split_stars_dim, the dimension specificied by tm_vars_dim
				split_stars_dim = get_split_stars_dim(mapping.aes)

		        if (length(hover) > 1) {
		          cli::cli_abort("hover should have length <= 1, not {length(hover)}.", call = NULL)
		        }

				if (is.na(hover)) {
					hover = id
				} else if (is.logical(hover)) {
					hover = ifelse(hover, id, "")
				}

				if (hover != "" && !hover %in% smeta$vars) rlang::arg_match0(hover, smeta$vars, arg_nm = "hover", error_call = NULL)
				if (hover != "") add_used_vars(hover, is_title = TRUE)
				if (id != "" && !id %in% smeta$vars) rlang::arg_match0(id, smeta$vars, arg_nm = "id", error_call = NULL)
				if (id != "") add_used_vars(id, is_title = TRUE)

				if (isTRUE(popup.vars)) {
					popup.vars = smeta$vars
				} else if (isFALSE(popup.vars)) {
					popup.vars = character(0)
				} else if (is.na(popup.vars[1])) {
					popup.vars = smeta$vars
				}

				if (!all(popup.vars %in% smeta$vars)) {
					rlang::arg_match(popup.vars, values = smeta$vars, multiple = TRUE)
				}
				if (length(popup.vars)) add_used_vars(popup.vars)


				if (length(popup.format) != 0 && !is.null(names(popup.format)) && all(names(popup.format) %in% popup.vars)) {
					popup.called = names(popup.format)
					popup.format = lapply(popup.vars, function(pv) {
						if (pv %in% names(popup.format)) {
							process_label_format(popup.format[[pv]], o$label.format)
						} else {
							process_label_format(list(), o$label.format)
						}
					})
				} else {
					popup.called = character(0)
					one.popup.format = process_label_format(popup.format, o$label.format)
					popup.format = lapply(popup.vars, function(pv) {
						one.popup.format
					})
				}
				names(popup.format) = popup.vars
				attr(popup.format, "called") = popup.called

			})
		})

		vl = get("vl", envir = .TMAP)
		vn = get("vn", envir = .TMAP)

		# split stars if needed (dimension -> attributes)
		split_stars_dim = get_split_stars_dim(tmg$tmls)
		shp = tmapSplitShp(shp, split_stars_dim, smeta)
		if (split_stars_dim != "") {
			smeta = tmapGetShapeMeta1(shp, o)
			if (dev) timing_add(s3 = "get_shape_meta1_2")
		}

		nrsd = length(smeta$dims) # number of required shape dimensions
		nrvd = as.integer(vn > 1L) # number of required variable dimensions (0 or 1)
		nrd = nrsd + nrvd # number of required by-dimensions


		tmg$tmf = within(tmg$tmf, {
				by1 = rows
				by2 = columns
				by3 = pages

				trans_animate = !is.null(.TMAP$animate)
				nad = as.integer(trans_animate)

				if (trans_animate) {
					if (is.null(by3)) {
						by3 = "FRAME__"
					} else {
						if (by3 == "VARS__") {
							if (nrvd == 0) {
								by3 = "FRAME__"
							} else {
								if (is.null(by1)) {
									by1 = "VARS__"
									by3 = "FRAME__"
								} else if (is.null(by2)) {
									by2 = "VARS__"
									by3 = "FRAME__"
								} else {
									ani = names(.TMAP$animate)
									cli::cli_abort("transformation animation for variable {.val {ani}} cannot be used when pages is already specified to multiple variables")
								}

							}
						} else {
							ani = names(.TMAP$animate)
							cli::cli_abort("transformation animation for variable {.val {ani}} cannot be used when pages is already specified to {.val {by3}}")
						}
					}
				}



				# if (animate) {
				# 	if (!is.null(by3) {
				#
				# 		if (nrvd == 1) {
				#
				# 	} else if ()
				#
				# 	}
				# }


				if (!is.null(by)) {
					if (is.null(by1)) {
						by1 = by
					} else if (is.null(by2)) {
						by2 = by
					} else if (is.null(by3)) {
						by3 = by
					} else {
						stop("Too many dimensions") # quick&dirty, to do: move to other checks
					}
				}

				rev1 = is_rev(by1)
				rev2 = is_rev(by2)
				rev3 = is_rev(by3)

				if (rev1) by1 = remove_min(by1)
				if (rev2) by2 = remove_min(by2)
				if (rev3) by3 = remove_min(by3)




				if (nrd > 3L) {
					if (nrsd > 3L) cli::cli_abort("The shape object has more than 3 dimensions, so even {.fn tm_facets_grid} cannot be used.")
					nrvd = 0L
					nrd = 3L
					limitvars = TRUE
				} else {
					limitvars = FALSE
					#todo
					if (nrvd == 1L && !identical(by1, "VARS__") && !identical(by2, "VARS__") && !identical(by3, "VARS__")) {
						if (is.null(by1)) {
							by1 = "VARS__"
						} else if (is.null(by2)) {
							by2 = "VARS__"
						} else if (is.null(by3)) {
							by3 = "VARS__"
						}
					}
				}
				unsigned = setdiff(smeta$dims, c(by1, by2, by3))

				if (length(unsigned)) {
					if (is.null(by1)) by1 = unsigned[1] else if (is.null(by2)) by2 = unsigned[1] else by3 = unsigned[1]
					if (length(unsigned) > 1) {
						if (is.null(by2)) by2 = unsigned[2] else by3 = unsigned[2]
					}
					if (length(unsigned) == 3) {
						by3 = unsigned[3]
					}
				}

			# }

			bys = c(by1, by2, by3)
			if (length(bys)) {
				byvars = intersect(smeta$vars, bys)
				if (length(byvars)) add_used_vars(byvars)
			}
			if (!all(bys %in% c("VARS__", "FRAME__", smeta$vars, smeta$dims))) stop("unknown facet variables", call. = FALSE)
			if (is.na(na.text)) na.text = o$label.na

			if (is.na(type)) {
				type = if (!is.null(by1) && !is.null(by2)) "grid" else "wrapstack"
			}

		})

		smeta$vars = get("used_vars", envir = .TMAP)
		convert2density = get("c2d_vars", envir = .TMAP)
		smvars = if (length(convert2density)) unique(c(smeta$vars, "AREA")) else smeta$vars
		shp = tmapSubsetShp(shp, smvars)

		if (length(convert2density)) {
			for (v in convert2density) {
				sunit = tmg$tms$unit
				if (is.null(sunit)) sunit = o$unit
				shape.unit <- ifelse(sunit == "metric", "km", ifelse(sunit == "imperial", "mi", sunit))
				u = paste(shape.unit, shape.unit)
				if (is.numeric(shp[[v]])) shp[[v]] = shp[[v]] / units::set_units(shp$AREA, u, mode = "standard")
			}
		}


		if (dev) timing_add(s3 = "subset_shp")

		smeta = tmapGetShapeMeta2(shp, smeta, c(o, tmg$tmf))
		if (dev) timing_add(s3 = "get_shape_meta2")

		tmg$tmf = within(tmg$tmf, {

			gl = list(NULL, NULL, NULL)
			gn = c(1L, 1L, 1L)

			for (i in 1L:3L) {
				byi = get(paste0("by", i))
				if (!is.null(byi)) {
					if (byi == "FRAME__") {
						gl[i] = list(NULL)
						gn[i] = as.integer(nframes)
					} else
					if (byi == "VARS__") {
						gl[i] = list(vl)
						gn[i] = vn
					} else if (byi %in% smeta$vars) {
						gl[i] = list(smeta$vars_levs[[byi]])
						gn[i] = length(gl[[i]])
					} else if (byi %in% smeta$dims) {
						gl[[i]] = smeta$dims_val[[match(byi, smeta$dims)]]
						gn[i] = length(gl[[i]])
					}
				}
			}

			if (is.na(free.coords)) {
				if (type %in% c("wrapstack", "wrap", "stack")) {
					free.coords = rep(!any(c(by1, by2, by3) == "VARS__"), 3)
				} else {
					free.coords = c((!is.null(rows) && (rows != "VARS__")), (!is.null(columns)) && (columns != "VARS__"), (!is.null(pages)) && (pages != "VARS__"))
				}
			} else {
				free.coords = rep(free.coords, length.out = 3)
			}


			v = which(vapply(list(by1, by2, by3), identical, FUN.VALUE = logical(1), "VARS__"))

			b = setdiff(which(!vapply(list(by1, by2, by3), is.null, FUN.VALUE = logical(1))), v)

			by123 = paste0("by", 1L:3L)
			by123__ = paste0("by", 1L:3L, "__")

			var__ = by123__[v]
			by__ = by123__[b]


			#fl1 =
		})
		tmg$tms$shp = shp
		tmg$tms$smeta = smeta

		tmg
	})

	tmf = get_tmf(lapply(tmo, function(tmoi) tmoi$tmf))

	tmo$tmf_global = tmf
	tmo
}

