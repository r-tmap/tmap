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

# Workaround for a leaflegend bug (https://github.com/tomroh/leaflegend/issues/110),
# fixed upstream in leaflegend 1.3.0: before that, leaflegend decided which
# legend to show for a radio/check-controlled group by reading the layer
# control's label text via `innerText`, which returns "" for elements inside
# a hidden (display:none) ancestor. If the widget first rendered inside a
# container that started hidden (e.g. an inactive Quarto/bslib tab), that
# lookup silently failed, so the shown legend didn't match the active layer
# (e.g. all legends stayed visible instead of just the active one).
#
# leaflegend >= 1.3.0 fixes this itself (`textContent` instead of `innerText`,
# plus re-syncing on `layeradd`/`layerremove` once the map's deferred
# rendering completes), so this workaround is only applied for older
# installs; with leaflegend >= 1.3.0 it is a no-op.
# TODO: once leaflegend >= 1.3.0 can be assumed as the minimum installed
# version (e.g. in a year or so), drop this function and its call site.
fix_leaflegend_hidden_container = function(lf) {
	if (utils::packageVersion("leaflegend") >= "1.3.0") return(lf)

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
