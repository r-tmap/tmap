#' Set the position of map components
#' 
#' Set the position of map components, such as legends, title, compass, scale bar, etc. `tm_pos` is the 
#' 
#' @param cell.h,cell.v The plotting area is overlaid with a 3x3 grid, of which the middle grid cell is the map area. Components can be drawn into any cell. `cell.h` specifies the horizontal position (column) can take values `"left"`, `"center"`, and `"right"`, and `cell.v` specifies the vertical position (row) and can take values `"top"`, `"center"`, and `"bottom"`.
#' @param pos.h,pos.v The position of the component within the cell. The options for `pos.h` are `"left"`, `"center"`, and `"right"`, and for `cell.v` these are `"top"`, `"center"`, and `"bottom"`.
#' @param just.h,just.v The justification of the component in case multiple components are stacked. When they are stacked horizontally, `just.v` determines how compnents that are smaller in height than the available height (determined by the outer.margins if specified and otherwise by the heigest component) are justified: `"top"`, `"center"`, or `"bottom"`. Similarly, `just.h` determines how components are justified horizontally when they are stacked vertically: `"left"`, `"center"`, or `"right"`.
#' @export
#' @name tm_pos
#' @rdname tm_pos
tm_pos = function(cell.h, cell.v, pos.h, pos.v, just.h, just.v) {
	args = lapply(as.list(match.call()[-1]), eval, envir = parent.frame())
	args$type = "na"
	structure(args, class = "tm_pos")
}

#' @export
#' @name tm_pos_out
#' @rdname tm_pos
tm_pos_out = function(cell.h, cell.v, pos.h, pos.v, just.h, just.v) {
	args = lapply(as.list(match.call()[-1]), eval, envir = parent.frame())
	args$type = "out"
	structure(args, class = "tm_pos")
}

#' @export
#' @name tm_pos_in
#' @rdname tm_pos
tm_pos_in = function(pos.h, pos.v, just.h, just.v) {
	args = lapply(as.list(match.call()[-1]), eval, envir = parent.frame())
	args$type = "in"
	structure(args, class = "tm_pos")
}

#' #' @export
#' tm_pos_cell = function(pos.h, pos.v, just.h, just.v) {
#' 	args = lapply(as.list(match.call()[-1]), eval, envir = parent.frame())
#' 	args$type = "cell"
#' 	structure(args, class = "tm_pos")
#' }


#' @export
#' @name tm_pos_out
#' @rdname tm_pos
tm_pos_out = function(cell.h, cell.v, pos.h, pos.v, just.h, just.v) {
	args = lapply(as.list(match.call()[-1]), eval, envir = parent.frame())
	args$type = "out"
	structure(args, class = "tm_pos")
}

#' @export
#' @name tm_pos_auto_out
#' @rdname tm_pos
tm_pos_auto_out = function(cell.h, cell.v, pos.h, pos.v, just.h, just.v) {
	args = lapply(as.list(match.call()[-1]), eval, envir = parent.frame())
	args$type = "autoout"
	structure(args, class = "tm_pos")
}


#' @export
#' @name tm_pos_auto_in
#' @rdname tm_pos
tm_pos_auto_in = function(just.h, just.v) {
	args = lapply(as.list(match.call()[-1]), eval, envir = parent.frame())
	args$type = "autoin"
	structure(args, class = "tm_pos")
}
