get_facet_id = function(row, col, nrow, ncol) {
	col + (row - 1L) * ncol
}

get_lf = function(facet_row, facet_col, facet_page) {
	lfs = get("lfs", envir = .TMAP_LEAFLET)
	nrow = get("nrow", envir = .TMAP_LEAFLET)
	ncol = get("ncol", envir = .TMAP_LEAFLET)

	lfsi = lfs[[facet_page]]

	fr = max(1, facet_row) # facet_row can be -1 or -2
	fc = max(1, facet_col) # facet_row can be -1 or -2

	lfid = get_facet_id(fr, fc, nrow, ncol)

	lfsi[[lfid]]
}

assign_lf = function(lf, facet_row, facet_col, facet_page) {
	lfs = get("lfs", envir = .TMAP_LEAFLET)
	nrow = get("nrow", envir = .TMAP_LEAFLET)
	ncol = get("ncol", envir = .TMAP_LEAFLET)


	fr = max(1, facet_row) # facet_row can be -1 or -2
	fc = max(1, facet_col) # facet_row can be -1 or -2



	lfid = get_facet_id(fr, fc, nrow, ncol)

	lfs[[facet_page]][[lfid]] = lf
	assign("lfs", lfs, envir = .TMAP_LEAFLET)
	NULL
}

getClusterOpts = function(clustering) {
	if (identical(clustering, TRUE)) {
		clusterOpts = leaflet::markerClusterOptions()
	} else if (identical(clustering, FALSE)) {
		clusterOpts = NULL
	} else {
		clusterArgs = setdiff(names(formals(leaflet::markerClusterOptions)), "...")
		if (!all(clusterArgs %in% names(clustering))) cli::cli_abort("{.field clustering} Cluster options should have the same options as {.fun leaflet::markerClusterOptions}")
		clusterOpts = clustering
	}
}


tmapLeafletWrap = function(label, facet_row, facet_col, facet_page, o) {

	NULL
}

tmapLeafletXtab = function(label, facet_row, facet_col, facet_page, o) {
}

blend_lf = function(lf, blend, pane) {
	if (is.null(blend) || blend == "over") return(lf)
	htmlwidgets::onRender(lf, sprintf("
        function(el, x) {
            var pane = el.querySelector('.leaflet-%s-pane');
            if (pane) pane.style.mixBlendMode = '%s';
        }
    ", pane, blend))
}

# Workaround for a leaflegend bug (https://github.com/tomroh/leaflegend/issues):
# leaflegend decides which legend to show for a radio/check-controlled group by
# reading the layer control's label text via `innerText`, which returns "" for
# elements inside a hidden (display:none) ancestor. If the widget first renders
# inside a container that starts hidden (e.g. an inactive Quarto/bslib tab),
# that lookup silently fails, so the shown legend doesn't match the active
# layer (e.g. all legends stay visible instead of just the active one). Once
# the container becomes visible, re-firing the events leaflegend listens for
# makes it redo the lookup (which then succeeds), without needing leaflegend
# itself to change.
fix_leaflegend_hidden_container = function(lf) {
	htmlwidgets::onRender(lf, "
        function(el, x) {
            var myMap = this;
            if (el.offsetParent !== null) return;
            var io = new IntersectionObserver(function(entries, obs) {
                if (entries[0].isIntersecting) {
                    myMap.fire('baselayerchange');
                    myMap.fire('overlayadd');
                    myMap.fire('overlayremove');
                    obs.disconnect();
                }
            });
            io.observe(el);
        }
    ")
}
