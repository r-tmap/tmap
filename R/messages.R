message_init = function() {
	.TMAP$messages = character()
	invisible(NULL)
}

message_thrown = function(id) {
	id %in% .TMAP$messages
}

message_reg = function(id) {
	.TMAP$messages = c(.TMAP$messages, id)
	NULL
}

message_comp_scale = function() {
	if (!message_thrown("comp_scale")) {
		message("[plot mode] fit legend/component: Some legend items or map compoments do not fit well, and are therefore rescaled. Set the tmap option 'component.autoscale' to FALSE to disable rescaling.")	
		message_reg("comp_scale")
	}
	NULL
}

message_comp_high_wide = function(stack) {
	if (!message_thrown("comp_scale")) {
		message("[plot mode] legend/component: Some components or legends are too ", ifelse(stack == "vertical", "high", "wide"), " and are therefore rescaled. Set the tmap option 'component.autoscale' to FALSE to disable rescaling.")	
		message_reg("comp_scale")
	}
	NULL
}

message_c4a = function(old_palette_name, info) {
	new1 = info$fullname
	new2 = info$name
	mess = 	paste0("c4a_", old_palette_name)

	if (!message_thrown(mess)) {
		message(paste0("[cols4all] color palettes: use palettes from the R package cols4all. Run 'cols4all::c4a_gui()' to explore them. The old palette name \"", old_palette_name, "\" is named \"", new2, "\" (in long format \"", new1, "\")"))
		message_reg(mess)	
	} 
}

message_nothing_to_show = function(any_groups) {
	if (any_groups) {
		message("[nothing to show] no data layers defined after tm_shape")
	} else {
		message("[nothing to show] no layers defined")
	}
}

message_wrapstack = function(horizontal = TRUE) {
	if (horizontal) {
		message("[facets] use tm_facets_hstack() instead of tm_facets_wrap() to put the legends next to and aligned with the facets")
	} else {
		message("[facets] use tm_facets_vstack() instead of tm_facets_wrap() to put the legends next to and aligned with the facets")
	}
	
}
