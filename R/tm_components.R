tm_title = function(text, size, padding, fontface, fontfamily, stack, just, frame, frame.lwd, frame.r, bg.color, bg.alpha, position, width, height, group.frame, resize.as.group, z) {
	args = lapply(as.list(match.call()[-1]), eval, envir = parent.frame())
	if (!("z" %in% names(args))) args$z = as.integer(NA)
	tm_element_list(do.call(tm_element, c(args, list(subclass = c("tm_title", "tm_component")))))
}

# tm_compass = function( x = 1) {
# 	tm_element_list(tm_element(as.list(environment()), 
# 							   subclass = c("tm_layer", "tm_compass")))
# }

tm_credits = function(text, size, padding, fontface, fontfamily, stack, just, frame, frame.lwd, frame.r, bg.color, bg.alpha, position, width, height, group.frame, resize.as.group, z) {
	args = lapply(as.list(match.call()[-1]), eval, envir = parent.frame())
	if (!("z" %in% names(args))) args$z = as.integer(NA)
	tm_element_list(do.call(tm_element, c(args, list(subclass = c("tm_credits", "tm_component")))))
}
