complete_with_comp_group = function(comp, o) {
	# complete all non-called options from tm_legend and tm_<component> with the tm_components specs

	comp_grps = names(o)[grepl("component_", names(o))]
	grps = gsub(".*component_", "", comp_grps)
	grps_lst = strsplit(grps, "^", fixed = TRUE)
	grps_lst = lapply(grps_lst, function(l) {
		if (length(l) == 0) "" else l
	})
	if (is.na(comp$group_id)) {
		comp$group_id = paste(comp$position$type, comp$position$cell.h, comp$position$cell.v, comp$position$pos.h, comp$position$pos.v, comp$position$just.h, comp$position$just.v, sep = "_")
	}

	ids = c("", comp$group_id, comp$group_type)
	comp_ids = paste("component", ids, sep = "_")


	# take called from component itself (component function call)
	if ("called" %in% names(comp)) {
		comp$called_via_comp_group = comp$called
	} else {
		comp$called_via_comp_group = character()
	}

	for (i in seq_along(grps)) {
		if (length(intersect(grps_lst[[i]], ids))) {
			oc = o[[comp_grps[i]]]
			if ("position" %in% names(oc)) {
				oc$position = process_position(oc$position, o)
			}
			if (length(oc)) {
				comp[names(oc)] = oc
				comp$called_via_comp_group = unique(c(comp$called_via_comp_group, names(oc)))
			}
		}
	}

	# TO DO:
	# - loop through grp_ids(2)
	# - if any ids is equal to grp, do old script

	comp
}


impute_comp = function(a, o) {
	ca = class(a)

	call = names(a)
	a$position = process_position(a$position, o)
	a$padding = process_padding(a$padding)

	ot = get_prefix_opt(class = ca[1], o = o)
	if (!is.null(a$position)) ot$position = NULL
	a = complete_options(a, ot)

	# add options from parent classes
	if (length(ca) > 1L) {
		for (i in 2L:max(2L, length(ca)-2L)) {
			oti = get_prefix_opt(class = ca[i], o = o)
			if (!is.null(a$position) || ("tm_add_legend" %in% ca)) oti$position = NULL
			# otherwise the position lists (in and out) get mixed
			# for tm_add_legend, keep it empty because it will be imputed in update_l (using legend. defaults)
			a = complete_options(a, oti)
		}
	}

	if (!("tm_add_legend" %in% ca)) a = complete_with_comp_group(a, o)

	a$call = call

	#a$position = NULL

	if ("margins" %in% names(a)) a$margins = rep_len(a$margins, 4L)

	class(a) = ca
	a
}



update_l = function(o, l, v, mfun, unm, active) {
	# update legend options
	oltype = o["legend.orientation"]
	names(oltype) = "orientation"
	if (all(v %in% c("AREA", "LENGTH", "MAP_COLORS")) && is.null(l$show)) {
		l$show = FALSE
	}

	call = names(l)

	l = complete_options(l, oltype)
	oleg = o[names(o)[substr(names(o), 1, 6) == "legend" & substr(names(o), 1, 15) != "legend.settings"]]
	names(oleg) = substr(names(oleg), 8, nchar(names(oleg)))
	settings_name = paste0("legend.settings.", l$orientation)
	oleg = c(oleg, o[[settings_name]])


	if ("position" %in% names(l) && !is.null(l$position)) {
		l$position = process_position(l$position, o)
		oleg$position = NULL
	}


	l = complete_options(l, oleg)


	# general component options
	ot2 = get_prefix_opt(class = "tm_component", o = o)
	ot2$position = NULL
	l = complete_options(l, ot2)


	l$call = call
	l$mfun = mfun
	l$unm = unm
	l$active = active

	l = complete_with_comp_group(l, o)

	# update legend class
	class(l) = c(paste0("tm_legend", ifelse(!is.null(l$orientation), paste0("_", l$orientation), "")), "tm_legend", "tm_component", class(l))
	l
}

update_crt = function(o, crt, v, mfun, unm, active) {

	#crt_options
	cls = class(crt)

	call = names(crt)

	ocrt = o[substr(names(o), 1, 6) == "chart."]
	names(ocrt) = substr(names(ocrt), 7, nchar(names(ocrt)))


	if ("position" %in% names(crt)) crt$position = process_position(crt$position, o)

	crt = complete_options(crt, ocrt)


	# general component options
	ot2 = get_prefix_opt(class = "tm_component", o = o)
	ot2$position = NULL
	crt = complete_options(crt, ot2)

	crt = complete_with_comp_group(crt, o)


	crt$call = call
	crt$mfun = mfun
	crt$unm = unm
	crt$active = active


	# update legend class
	#class(l) = c(paste0("tm_legend_", l$design, ifelse(!is.null(l$orientation), paste0("_", l$orientation), "")), class(l))
	#l
	class(crt) = cls
	crt
}

warning_group_args = function(args) {
	old = c("group.frame", "resize_as_group", "stack")
	new = c("frame_combine", "resize_as_group", "stack")

	x = intersect(old, names(args))

	if (length(x)) {
		s = do.call(paste, c(mapply(function(o,n,v) {
			cli::format_inline(paste0("{n} = {.val {v}}"))
		}, old[match(x, old)], new[match(x, old)], args[x], SIMPLIFY = FALSE), list(sep = ", ")))

		cli::cli_warn(paste0("Component group arguments, such as {.var {x}}, are deprecated as of 4.1. Please use {.code group_id = {.val ID}} in combination with {.code tm_components(", s, ")} instead."))

		if ("group.frame" %in% names(args)) {
			args$frame_combine = args$group.frame
			args$group.frame = NULL
		}
	}
	args
}
