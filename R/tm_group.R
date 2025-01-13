#' Layer group control
#'
#' Controls the layer groups in interactive maps (view mode): the layer control box (radio buttons or check boxes) and at which zoom levels the layers are displayed at.
#'
#' @param name group name that corresponds with the group name specified in the layer functions (e.g.  [tm_polygons()])
#' @param control The group control determines how
#'   layer groups can be switched on and off. Options: `"radio"` for radio
#'   buttons (meaning only one group can be shown), `"check"` for check boxes
#'   (so multiple groups can be shown), and `"none"` for no control
#'   (the group cannot be (de)selected).
#' @param zoom_levels The zoom levels at which the group is displays at. When specified `control` will be set to `"none"`.
#' @seealso \href{https://r-tmap.github.io/tmap/articles/adv_groups}{vignette about layer groups}
#' @export
tm_group = function(name, control = NA, zoom_levels = NA) {
	optname = paste0("view_group_", name)
	x = structure(list(list(name = name, control = control, zoom_levels = zoom_levels)), names= optname)
	do.call(tm_options, x)
}
