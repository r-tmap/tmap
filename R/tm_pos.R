#' Set the position of map components
#' 
#' Set the position of map components, such as legends, title, compass, scale bar, etc. `tm_pos` is the function to position these components: `tm_pos_out` places the components outside the map area and `tm_pos_in` inside the map area. Each \code{position} argument of a map layer or component should be specified with one of these functions. The functions `tm_pos_auto_out` and `tm_pos_auto_in` are used to set the components automatically, and are recommended to use globally, via \code{\link{tmap_options}}. See details how the positioning works.
#' 
#' @param cell.h,cell.v The plotting area is overlaid with a 3x3 grid, of which the middle grid cell is the map area. Components can be drawn into any cell. `cell.h` specifies the horizontal position (column) can take values `"left"`, `"center"`, and `"right"`, and `cell.v` specifies the vertical position (row) and can take values `"top"`, `"center"`, and `"bottom"`. See details for a graphical explanation.
#' @param pos.h,pos.v The position of the component within the cell. The main options for `pos.h` are `"left"`, `"center"`, and `"right"`, and for `cell.v` these are `"top"`, `"center"`, and `"bottom"`. These options can also be provided in upper case; in that case there is no offset (see the tmap option `component.offset`). Also numbers between 0 and 1 can be provided, which determine the position of the component inside the cell (with (0,0) being left bottom). The arguments `just.h` and `just.v` determine the justification point.
#' @param align.h,align.v The alignment of the component in case multiple components are stacked. When they are stacked horizontally, `align.v` determines how components that are smaller in height than the available height (determined by the outer.margins if specified and otherwise by the heigest component) are justified: `"top"`, `"center"`, or `"bottom"`. Similarly, `align.h` determines how components are justified horizontally when they are stacked vertically: `"left"`, `"center"`, or `"right"`.
#' @param just.h,just.v The justification of the components. Only used in case `pos.h` and `pos.v` are numbers.
#' @details 
#' 
#' | | | | | |
#' |---------------:|:---------------|:------------------|:------------------|:------|
#' | | +------------------- | +-------------------------------- | +------------------- | +
#' | | \|     | \|       |\|        |\| |
#' | "top" | \|     | \|       |\|        |\| |
#' | | \|     | \|       |\|        |\| |
#' | | +------------------- | +-------------------------------- | +------------------- | +
#' | | \|     | \|       |\|        |\| |
#' | | \|     | \|       |\|        |\| |
#' | cell.v&emsp; "center"| \|     | \| &emsp;&emsp;Map(s)      |\|        |\| |
#' | | \|     | \|       |\|        |\| |
#' | | \|     | \|       |\|        |\| |
#' | | +------------------- | +-------------------------------- | +------------------- | +
#' | | \|     | \|       |\|        |\| |
#' | "bottom" | \|     | \|       |\|        |\| |
#' | | \|     | \|       |\|        |\| |
#' | | +------------------- | +-------------------------------- | +------------------- | +
#' | |  &emsp;&emsp;`"left"` | &emsp;&emsp;`"center"`      |  &emsp;&emsp; `"right"`     | |
#' | |  | &emsp;&emsp;`cell.h` | | |
#' 
#' \code{tm_pos_in} sets the position of the component(s) inside the maps area, which is equivalent to the center-center cell (in case there are facets, these are all drawn in this center-center cell). 
#' 
#' \code{tm_pos_out} sets the position of the component(s) outside the map. 
#' 
#' The amount of space that the top and bottom rows, and left and right columns occupy is determined by the \code{\link{tm_layout}} arguments \code{meta.margins} and \code{meta.auto.margins}. The former sets the relative space of the bottom, left, top, and right side. In case these are set to \code{NA}, the space is set automatically based on 1) the maximum relative space specified by \code{meta.auto.margins} and 2) the presence and size of components in each cell. For instance, if there is one landscape oriented legend in the center-bottom cell, then the relative space of the bottom row is set to the height of that legend (given that it is smaller than the corresponding value of `meta.auto.margins`), while the other four sides are set to 0.
#' 
#' \code{tm_pos_auto_out} is more complex: the `cell.h` and `cell.v` arguments of should be set to one of the four corners. It does not mean that the components are drawn in a corner. The corner represents the sides of the map that the components are drawn. By default, legends are drawn either at the bottom or on the right-side of the map by default (see `tmap_options("legend.position")`). Only when there are row- and column-wise legends and a general legend (using \code{\link{tm_facets_grid}}), the general legend is drawn in the corner, but in practice this case will be rare.
#' 
#' The arguments `pos.h` and `pos.v` determine where the components are drawn within the cell. Again, with `"left"`, `"center"`, and `"right"` for `pos.h` and `"top"`, `"center"`, and `"bottom"` for `pos.v`. The values can also be specified in upper-case, which influences the offset with the cell borders, which is determined by tmap option `component.offset`. By default, there is a small offset when components are drawn inside and no offset when they are drawn outside or with upper-case.
#' 
#' \code{tm_pos_auto_in} automatically determines `pos.h` and `pos.v` given the available space inside the map. This is similar to the default positioning in tmap3.
#' 
#' In case multiple components are draw in the same cell and the same position inside that cell, they are stacked (determined which the `stack` argument in the legend or component function). The `align.h` and `align.v` arguments determine how these components will be justified with each other.
#' 
#' Note that legends and components may be different for a facet row or column. This is the case when \code{\link{tm_facets_grid}} or \code{\link{tm_facets_stack}} are applied and when scales are set to free (with the \code{.free} argument of the map layer functions). In case a legends or components are draw row- or column wise, and the position of the legends (or components) is right next to the maps, these legends (or components) will be aligned with the maps.
#' 
#' @md
#' @export
#' @name tm_pos
#' @rdname tm_pos
tm_pos = function(cell.h, cell.v, pos.h, pos.v, align.h, align.v, just.h, just.v) {
	args = lapply(as.list(match.call()[-1]), eval, envir = parent.frame())
	args$type = "na"
	structure(args, class = "tm_pos")
}

#' @export
#' @name tm_pos_out
#' @rdname tm_pos
tm_pos_out = function(cell.h, cell.v, pos.h, pos.v, align.h, align.v, just.h, just.v) {
	args = lapply(as.list(match.call()[-1]), eval, envir = parent.frame())
	args$type = "out"
	structure(args, class = "tm_pos")
}

#' @export
#' @name tm_pos_in
#' @rdname tm_pos
tm_pos_in = function(pos.h, pos.v, align.h, align.v, just.h, just.v) {
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
tm_pos_out = function(cell.h, cell.v, pos.h, pos.v, align.h, align.v, just.h, just.v) {
	args = lapply(as.list(match.call()[-1]), eval, envir = parent.frame())
	args$type = "out"
	structure(args, class = "tm_pos")
}

#' @export
#' @name tm_pos_auto_out
#' @rdname tm_pos
tm_pos_auto_out = function(cell.h, cell.v, pos.h, pos.v, align.h, align.v, just.h, just.v) {
	args = lapply(as.list(match.call()[-1]), eval, envir = parent.frame())
	args$type = "autoout"
	structure(args, class = "tm_pos")
}


#' @export
#' @name tm_pos_auto_in
#' @rdname tm_pos
tm_pos_auto_in = function(align.h, align.v, just.h, just.v) {
	args = lapply(as.list(match.call()[-1]), eval, envir = parent.frame())
	args$type = "autoin"
	structure(args, class = "tm_pos")
}
