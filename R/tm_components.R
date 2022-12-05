tm_title = function(title, size, padding, fontface, fontfamily, stack, just, frame, frame.lwd, frame.r, bg.color, bg.alpha, position, width, height, group.frame, resize.as.group) {
	args = lapply(as.list(match.call()[-1]), eval, envir = parent.frame())
	tm_element_list(do.call(tm_element, c(args, list(subclass = c("tm_title", "tm_component")))))
}
