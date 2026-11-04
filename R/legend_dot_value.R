# Dot values of dot density legends (legend argument dot.value)
#
# A dot pattern has one dot per square of s x s inches, where the spacing s = s1 / d, with s1 the spacing at
# density 1 (a quarter of a text line, times the scale) and d the pattern density. With a proportional scale
# (tm_scale_continuous_area(): limits from 0 and d = d_max * sqrt(v / v_max) for a data value v), the number of
# dots per square inch is d^2 / s1^2 = d_max^2 * (v / v_max) / s1^2, so each dot represents
# n = v_max / d_max^2 * s1^2 * A data units, where A is the map area per square inch. This function computes the
# first part, K = v_max / d_max^2 (with v_max per km2), when the scale is applied. The note itself is computed
# when the map is drawn (legend_dot_note), since A depends on the map scale.
legend_dot_info = function(legend, values, values.range, values.scale, limits, trans, x_units, layer) {
	fail = function(reason) {
		message_legend_dot_value(reason)
		NULL
	}
	if (!inherits(values, "tmapSeq")) return(fail("the number of dots is not proportional to the data values"))
	p = seq_power(values$power)
	proportional = identical(trans, "identity") && isTRUE(limits[1] == 0) && values$from == 0 &&
		abs(p - 0.5) < 1e-6 && values.range[1] == 0
	if (!proportional) return(fail("the number of dots is not proportional to the data values"))

	# data values per km2
	unit = legend$dot.unit
	per_km2 = if (!is.null(unit) && !is.na(unit)) {
		a = tryCatch(as.numeric(units::set_units(units::as_units(1, unit), "km^2", mode = "standard")),
					 error = function(e) NA)
		if (is.na(a)) return(fail(cli::format_inline("{.arg dot.unit} {.val {unit}} is not an area unit")))
		1 / a
	} else if (!is.null(x_units)) {
		f = tryCatch(as.numeric(units::set_units(units::as_units(1, x_units), "1/km^2", mode = "standard")),
					 error = function(e) NA)
		if (is.na(f)) return(fail(cli::format_inline("the units of the data ({.val {x_units}}) are not per area")))
		f
	} else 1

	text = if (isTRUE(legend$dot.value)) "1 dot ≈ {n}" else as.character(legend$dot.value)
	d_max = values.scale * values$to * sqrt(values.range[2])
	list(text = text, K = limits[2] * per_km2 / d_max^2)
}

message_legend_dot_value = function(reason = "the number of dots is not proportional to the data values") {
	cli::cli_inform(c("{.field [legend]} the {.arg dot.value} of the legend is not shown: {reason}.",
					  i = "{.arg dot.value} is meant for the legend of {.arg pattern_density} with a dot pattern and {.code pattern_density.scale = tm_scale_continuous_area()}."),
					.frequency = "regularly", .frequency_id = paste0("dot_value_", reason))
	invisible(NULL)
}

# The map area (km2) per square inch at the center of the map (bbox bb, width bb_facet inches), and the ratio
# between the largest and smallest area scale (at 10%, 50%, and 90% of the map height)
map_area_scale = function(bb, bb_facet) {
	crs = sf::st_crs(bb)
	if (is.na(crs) || crs_is_local(crs)) return(NULL)
	bbw = unname(bb[3] - bb[1])
	bbh = unname(bb[4] - bb[2])
	cpi = bbw / bb_facet # coordinates per inch
	d = 0.001 * bbw
	cx = unname(bb[1]) + bbw / 2
	a = vapply(unname(bb[2]) + bbh * c(0.5, 0.1, 0.9), function(cy) {
		pts = sf::st_sfc(sf::st_point(c(cx, cy)), sf::st_point(c(cx + d, cy)), sf::st_point(c(cx, cy + d)),
						 crs = crs)
		dst = suppressWarnings(as.numeric(sf::st_distance(sf::st_transform(pts, 4326)))) / 1000
		dst = matrix(dst, 3)
		(dst[1, 2] / d) * (dst[1, 3] / d) * cpi^2
	}, numeric(1))
	if (any(!is.finite(a)) || any(a <= 0)) return(NULL)
	list(area = a[1], ratio = max(a) / min(a))
}

# Format the number of units per dot
format_dot_value = function(n) {
	n = signif(n, 2)
	format(n, big.mark = ",", scientific = FALSE, trim = TRUE)
}

legend_dot_types = c("dots", "circles", "crosses")

legend_dot_pattern = function(gp) {
	pat = unique(stats::na.omit(gp$pattern))
	length(pat) == 1L && pattern_parse(pattern_norm(pat))$type %in% legend_dot_types
}

# The note of a dot density legend in plot mode
legend_dot_note = function(cmp, bb, bb_facet, o) {
	fail = function(reason) {
		message_legend_dot_value(reason)
		""
	}
	if (!legend_dot_pattern(cmp$gp)) return(fail("the pattern is not a (constant) dot pattern"))
	as = map_area_scale(bb, bb_facet)
	if (is.null(as)) return(fail("the map crs does not have measurable units"))
	if (as$ratio > 1.2) return(fail("the area scale varies too much across the map; use an equal-area projection"))
	s1 = 0.25 * o$lin * o$scale * (o$scale_down %||% 1)
	n = cmp[["dot_info"]]$K * s1^2 * as$area
	gsub("{n}", format_dot_value(n), cmp[["dot_info"]]$text, fixed = TRUE)
}

# The note of a legend (before the map is drawn, a placeholder of the dot value is used to reserve space)
legend_note = function(comp) {
	if (!is.null(comp$note)) return(comp$note)
	if (!is.null(comp[["dot_info"]])) return(gsub("{n}", "000,000", comp[["dot_info"]]$text, fixed = TRUE))
	""
}

# The note of a dot density legend in view mode (leaflet). The spacing of the dots at density 1 is a quarter of
# 0.2 inch (see leaflet_pattern_ids) times the scale, at 96 pixels per inch. The map area per pixel is measured at
# the center of the map (with the distance function of the map crs). The note is updated after zooming and
# panning; with zooming patterns (pattern.zoom), the spacing grows with the zoom level (relative to the initial
# zoom level). The note is hidden when the area scale varies too much (more than 20%) across the map.
leaflet_dot_note = function(lf, cls, dot_info, scale, zoom) {
	htmlwidgets::onRender(lf, "function(el, x, data) {
		var map = this;
		var z0 = map.getZoom();
		// map area (km2) per pixel at container point (x, y)
		var area = function(x, y) {
			var d = 10;
			var p0 = map.containerPointToLatLng([x, y]);
			var px = map.containerPointToLatLng([x + d, y]);
			var py = map.containerPointToLatLng([x, y + d]);
			return (map.distance(p0, px) / 1000 / d) * (map.distance(p0, py) / 1000 / d);
		};
		var update = function() {
			var leg = el.querySelector('.' + data.cls);
			if (!leg) return;
			var note = leg.querySelector('.tmap-dot-note');
			if (!note) {
				note = document.createElement('div');
				note.className = 'tmap-dot-note';
				note.style.fontSize = '12px';
				note.style.marginTop = '4px';
				leg.appendChild(note);
			}
			var sz = map.getSize();
			var a = [area(sz.x / 2, sz.y / 2), area(sz.x / 2, sz.y * 0.1), area(sz.x / 2, sz.y * 0.9)];
			var ok = a.every(function(ai) { return isFinite(ai) && ai > 0; }) &&
				Math.max.apply(null, a) / Math.min.apply(null, a) <= 1.2;
			if (!ok) {
				note.style.display = 'none';
				return;
			}
			note.style.display = '';
			var s = data.s1 * (data.zoom ? Math.pow(2, map.getZoom() - z0) : 1);
			var n = data.K * s * s * a[0];
			var p = Math.pow(10, Math.floor(Math.log10(n)) - 1);
			n = Math.round(n / p) * p;
			var txt = n >= 1 ? Math.round(n).toLocaleString('en-US') : String(Number(n.toPrecision(2)));
			note.textContent = data.text.split('{n}').join(txt);
		};
		map.on('zoomend moveend', update);
		update();
	}", data = list(cls = cls, K = dot_info$K, text = dot_info$text, s1 = 0.25 * 0.2 * scale * 96, zoom = zoom))
}
