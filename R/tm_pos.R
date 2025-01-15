#' Set the position of map components
#'
#' Set the position of map components, such as legends, title, compass, scale bar, etc.
#' `tm_pos()` is the function to position these components:
#' `tm_pos_out()` places the components outside the map area, `tm_pos_in()` inside the map area, and `tm_pos_on_top()` on top of the map
#' Each `position` argument of a map layer or component should be specified with
#' one of these functions.
#' The functions `tm_pos_auto_out()` and `tm_pos_auto_in()`
#' are used to set the components automatically, and should be used
#' via [tmap_options()]. See Details how the positioning works.
#'
#' @param cell.h,cell.v The plotting area is overlaid with a 3x3 grid, of which
#'   the middle grid cell is the map area. Components can be drawn into any cell.
#'   `cell.h` specifies the horizontal position (column) can take values
#'   `"left"`, `"center"`, and `"right"`. `cell.v` specifies the vertical position
#'   (row) and can take values `"top"`, `"center"`, and `"bottom"`.
#'   See details for a graphical explanation.
#' @param pos.h,pos.v The position of the component within the cell.
#'   The main options for `pos.h` are `"left"`, `"center"`, and `"right"`.
#'   For `cell.v` these are `"top"`, `"center"`, and `"bottom"`.
#'   These options can also be provided in upper case; in that case there is no offset
#'   (see the tmap option `component.offset`). Also numbers between 0 and 1 can be provided,
#'   which determine the position of the component inside the cell (with (0,0) being left bottom).
#'   The arguments `just.h` and `just.v` determine the justification point.
#' @param align.h,align.v The alignment of the component in case multiple components are stacked.
#'   When they are stacked horizontally, `align.v` determines how components
#'   that are smaller in height than the available height (determined by the outer.margins
#'   if specified and otherwise by the highest component) are justified:
#'   `"top"`, `"center"`, or `"bottom"`. Similarly, `align.h` determines how components
#'   are justified horizontally when they are stacked vertically: `"left"`, `"center"`, or `"right"`.
#' @param just.h,just.v The justification of the components.
#'   Only used in case `pos.h` and `pos.v` are numbers.
#' @details
#' `tm_pos_in()` sets the position of the component(s) inside the maps area,
#' which is equivalent to the center-center cell (in case there are facets,
#' these are all drawn in this center-center cell).
#'
#' `tm_pos_out()` sets the position of the component(s) outside the map.
#'
#' `tm_pos_on_top()` is the same as `tm_pos_out`, but with the cell set to the center cell
#' It may be therefore seem similar to `tm_pos_in()`, but with an essential difference:
#' `tm_pos_in()` takes the map frame into account whereas `tm_pos_on_top()` does not.
#' #'
#' The amount of space that the top and bottom rows, and left and right columns
#' occupy is determined by the [tm_layout()] arguments `meta.margins` and `meta.auto_margins`.
#' The former sets the relative space of the bottom, left, top, and right side.
#' In case these are set to `NA`, the space is set automatically based on 1)
#' the maximum relative space specified by `meta.auto_margins` and 2)
#' the presence and size of components in each cell.
#' For instance, if there is one landscape oriented legend in the center-bottom cell,
#' then the relative space of the bottom row is set to the height of that legend
#' (given that it is smaller than the corresponding value of `meta.auto_margins`),
#' while the other four sides are set to 0.
#'
#' `tm_pos_auto_out()` is more complex: the `cell.h` and `cell.v` arguments should be set
#' to one of the four corners. It does not mean that the components are drawn in a corner.
#' The corner represents the sides of the map that the components are drawn.
#' By default, legends are drawn either at the bottom or on the right-side of
#' the map by default (see `tmap_options("legend.position")`).
#' Only when there are row- and column-wise legends and a general legend (using [tm_facets_grid()]),
#' the general legend is drawn in the corner, but in practice this case will be rare.
#'
#' The arguments `pos.h` and `pos.v` determine where the components are drawn within the cell.
#' Again, with `"left"`, `"center"`, and `"right"` for `pos.h`
#' and `"top"`, `"center"`, and `"bottom"` for `pos.v`.
#' The values can also be specified in upper-case, which influences the offset
#' with the cell borders, which is determined by tmap option `component.offset`.
#' By default, there is a small offset when components are drawn inside and no
#' offset when they are drawn outside or with upper-case.
#'
#' `tm_pos_auto_in()` automatically determines `pos.h` and `pos.v` given the
#' available space inside the map. This is similar to the default positioning in tmap3.
#'
#' In case multiple components are draw in the same cell and the same position
#' inside that cell, they are stacked (determined which the `stack` argument in
#' the legend or component function). The `align.h` and `align.v` arguments
#' determine how these components will be justified with each other.
#'
#' Note that legends and components may be different for a facet row or column.
#' This is the case when [tm_facets_grid()] or [tm_facets_stack()] are applied
#' and when scales are set to free (with the `.free` argument of the map layer functions).
#' In case a legends or components are draw row- or column wise, and the position
#' of the legends (or components) is right next to the maps, these legends
#' (or components) will be aligned with the maps.
#'
#' @seealso \href{https://r-tmap.github.io/tmap/articles/adv_positions}{Vignette about positioning}
#' @export
tm_pos = function(cell.h, cell.v, pos.h, pos.v, align.h, align.v, just.h, just.v) {
	args = lapply(as.list(rlang::call_match()[-1]), eval, envir = parent.frame())
	args$type = "na"
	structure(args, class = "tm_pos")
}

#' @export
#' @rdname tm_pos
tm_pos_in = function(pos.h, pos.v, align.h, align.v, just.h, just.v) {
	args = lapply(as.list(rlang::call_match()[-1]), eval, envir = parent.frame())
	args$type = "in"
	structure(args, class = "tm_pos")
}


#' @export
#' @rdname tm_pos
tm_pos_out = function(cell.h, cell.v, pos.h, pos.v, align.h, align.v, just.h, just.v) {
	args = lapply(as.list(rlang::call_match()[-1]), eval, envir = parent.frame())
	args$type = "out"
	structure(args, class = "tm_pos")
}

#' @export
#' @rdname tm_pos
tm_pos_on_top = function(pos.h, pos.v, align.h, align.v, just.h, just.v) {
	args = lapply(as.list(rlang::call_match()[-1]), eval, envir = parent.frame())
	args$cell.h = "center"
	args$cell.v = "center"
	args$type = "out"
	structure(args, class = "tm_pos")
}

#' @export
#' @rdname tm_pos
tm_pos_auto_out = function(cell.h, cell.v, pos.h, pos.v, align.h, align.v, just.h, just.v) {
	args = lapply(as.list(rlang::call_match()[-1]), eval, envir = parent.frame())
	args$type = "autoout"
	structure(args, class = "tm_pos")
}

#' @export
#' @rdname tm_pos
tm_pos_auto_in = function(align.h, align.v, just.h, just.v) {
	args = lapply(as.list(rlang::call_match()[-1]), eval, envir = parent.frame())
	args$type = "autoin"
	structure(args, class = "tm_pos")
}


