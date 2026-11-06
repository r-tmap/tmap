# Pattern engine: one pattern definition, several outputs.
#
# pattern_geom() computes the geometry of one repeating tile (in inches), from a pattern
# specification (see tm_patterns()), the spacing between pattern elements and the line
# width / dot size. Converters turn the geometry into
# - a grob clipped by the outline of polygons (plot mode)
# - an SVG <pattern> (view mode)
# - a PNG file or data URI (web modes, legend swatches)
#
# Most patterns consist of vector primitives (lines and circles). The patterns "terrain", "mottle", and "sketch"
# are raster textures instead (see pattern_texture()), drawn on top of the fill color: "terrain" (random relief)
# and "mottle" (blotches) as semi-transparent highlights (white) and shadows (pattern_col), and "sketch" as
# white paper between pencil strokes. Their strength is set by the density.

# Spacing between pattern elements (inches): 1/4 line at density 1, scaled by o$scale.
# Below density 0.05 no pattern is drawn (NA)
pattern_spacing = function(density, scale = 1, line_in = 0.2) {
	s = 0.25 * line_in * scale / density
	s[is.na(density) | density < 0.05] = NA
	s
}

# Snap an angle (degrees) to the nearest multiple of step (for motifs that only repeat
# in axis-aligned tiles at those angles)
pattern_snap_angle = function(angle, step, type) {
	a = (round(angle / step) * step) %% 180
	if (abs(((angle - a + 90) %% 180) - 90) > 0.5) {
		cli::cli_warn(paste("Pattern type {.val {type}} supports angles in steps of {step} degrees only,",
							"so {angle} is changed to {a}"),
					  .frequency = "regularly", .frequency_id = paste0("pattern_angle_", type))
	}
	a
}

# Deterministic pseudo-random numbers in [0, 1) (leaves the global RNG untouched)
pattern_rand = function(n, seed = 1) {
	x = sin(seq_len(n) * 12.9898 + seed * 78.233) * 43758.5453
	x - floor(x)
}

# Tile geometry for a pattern specification
#
# @param spec pattern specification string, e.g. "hatch(angle=45)"
# @param s spacing between pattern elements (inches)
# @param lwd line width (lwd units, 1/96 inch); dots have a diameter of 3 * lwd / 96 inches
# @param density the density (only used by the raster textures, for which it is the strength)
# @return list with tile width w and height h (inches), and the primitives (inches):
#   - segs: matrix (x0, y0, x1, y1) of continuous lines, which may extend beyond the tile;
#     only the part within the tile belongs to it
#   - elems: matrix (x0, y0, x1, y1) of line elements (e.g. crosses) and
#   - circles: data.frame(x, y, r, filled) of circle elements (dots, circles);
#     elements belong to the tile in which their center lies
#   - raster: the specification of a raster texture (see pattern_raster()), which fills the whole tile
#   NULL for "none" or when s is NA
pattern_geom = function(spec, s, lwd = 1, density = NA) {
	p = pattern_parse(pattern_norm(spec))
	if (p$type == "none" || is.na(s)) return(NULL)
	a = p$pars
	lw_in = lwd / 96

	geom = function(w, h, segs = NULL, elems = NULL, circles = NULL, raster = NULL) {
		list(w = w, h = h, segs = segs, elems = elems, circles = circles, raster = raster)
	}
	seg = function(x0, y0, x1, y1) {
		n = max(length(x0), length(y0), length(x1), length(y1))
		cbind(x0 = rep_len(x0, n), y0 = rep_len(y0, n), x1 = rep_len(x1, n), y1 = rep_len(y1, n))
	}
	# polylines (list of n x 2 matrices) to segments
	poly2seg = function(lst) {
		do.call(rbind, lapply(lst, function(m) {
			k = nrow(m)
			seg(m[-k, 1], m[-k, 2], m[-1, 1], m[-1, 2])
		}))
	}
	transpose = function(g) {
		tm = function(m) {
			if (is.null(m)) return(NULL)
			m = m[, c("y0", "x0", "y1", "x1"), drop = FALSE]
			colnames(m) = c("x0", "y0", "x1", "y1")
			m
		}
		g$segs = tm(g$segs)
		g$elems = tm(g$elems)
		if (!is.null(g$circles)) g$circles[c("x", "y")] = g$circles[c("y", "x")]
		g[c("w", "h")] = g[c("h", "w")]
		g
	}

	# family of parallel lines with perpendicular spacing s at angle (degrees)
	hatch = function(angle, s) {
		angle = angle %% 180
		if (abs(angle) < 0.5 || abs(angle - 180) < 0.5) {
			geom(s, s, seg(0, s / 2, s, s / 2))
		} else if (abs(angle - 90) < 0.5) {
			geom(s, s, seg(s / 2, 0, s / 2, s))
		} else {
			r = angle * pi / 180
			w = s / abs(sin(r))
			h = s / abs(cos(r))
			L = 2 * (w + h)
			x0 = (-2:2) * w
			geom(w, h, seg(x0 - L * cos(r), -L * sin(r), x0 + L * cos(r), L * sin(r)))
		}
	}

	# regular or jittered grid of elements: one per cell of s x s; random tiles span 8 x 8 cells
	grid_points = function(random, s, seed = 1) {
		k = if (random > 0) 8L else 1L
		xy = expand.grid(i = seq_len(k) - 1L, j = seq_len(k) - 1L)
		jit = random * s / 2
		n = nrow(xy)
		list(x = (xy$i + 0.5) * s + (2 * pattern_rand(n, 2 * seed - 1) - 1) * jit,
			 y = (xy$j + 0.5) * s + (2 * pattern_rand(n, 2 * seed) - 1) * jit,
			 w = k * s, h = k * s)
	}

	switch(p$type,
		hatch = hatch(a$angle, s),
		crosshatch = {
			angle = pattern_snap_angle(a$angle, 45, "crosshatch") %% 90
			g1 = hatch(angle, s)
			g2 = hatch(angle + 90, s)
			geom(g1$w, g1$h, rbind(g1$segs, g2$segs))
		},
		dots = ,
		circles = ,
		crosses = {
			gp = grid_points(a$random, s, seed = if (is.null(a$seed)) 1 else a$seed)
			if (p$type == "crosses") {
				d = 0.25 * s
				geom(gp$w, gp$h, elems = rbind(seg(gp$x - d, gp$y, gp$x + d, gp$y),
											   seg(gp$x, gp$y - d, gp$x, gp$y + d)))
			} else {
				r = if (p$type == "dots") 1.5 * lw_in else 0.3 * s
				geom(gp$w, gp$h, circles = data.frame(x = gp$x, y = gp$y, r = r,
													  filled = (p$type == "dots")))
			}
		},
		terrain = ,
		sketch = ,
		mottle = {
			# the spacing at density 1 (s * density) is the unit of the grain; the tile (at least 1 inch, 2 inches for
			# the larger features of "mottle" and "sketch") contains many grains, so that the repetition is hardly visible.
			# The patches of "mottle" are much larger: up to 12 spacings (0.6 inch by default); its tile is 3 to 6 inches
			u = if (is.na(density)) s else s * density
			lmax = a$grain * u * (if (p$type == "mottle") 12 else 1)
			w = switch(p$type, terrain = max(1, 8 * lmax), sketch = max(2, 16 * lmax), mottle = min(max(3, 8 * lmax), 6))
			geom(w, w, raster = list(kind = p$type, strength = if (is.na(density)) 0.8 else min(density, 1), lmax = lmax,
									 angle = if (is.null(a$angle)) 0 else a$angle,
									 levels = if (is.null(a$levels)) 5 else a$levels,
									 contrast = if (is.null(a$contrast)) 1 else a$contrast,
									 random = if (is.null(a$random)) 0 else a$random,
									 seed = if (is.null(a$seed)) 1 else a$seed))
		},
		noise = {
			# fine speckle: 4 dots of random size per cell of s x s, 8 x 8 cells
			k = 8L
			n = k * k * 4L
			geom(k * s, k * s, circles = data.frame(x = pattern_rand(n, 3) * k * s,
													y = pattern_rand(n, 4) * k * s,
													r = (0.4 + 0.8 * pattern_rand(n, 5)) * lw_in,
													filled = TRUE))
		},
		{
			angle = pattern_snap_angle(a$angle, 90, p$type)
			g = switch(p$type,
				dashes = {
					L = a$length * s
					y = c(s / 2, 3 * s / 2)
					geom(2 * L, 2 * s, seg(c(0, L), y, c(L, 2 * L), y))
				},
				bricks = {
					L = a$length * s
					geom(L, 2 * s, seg(x0 = c(0, 0, 0, L / 2), y0 = c(0, s, 0, s),
									   x1 = c(L, L, 0, L / 2), y1 = c(0, s, s, 2 * s)))
				},
				herringbone = {
					# columns of diagonal lines, alternating direction, offset by half a period
					wc = a$length * s / 2
					h = s * sqrt(2)
					y0 = (-3:3) * h
					geom(2 * wc, h, rbind(seg(0, y0, wc, y0 + wc),
										  seg(wc, y0 + h / 2 + wc, 2 * wc, y0 + h / 2)))
				},
				hexagons = {
					# the three edges per hexagon that are not shared with an earlier one
					r = s / sqrt(3)
					w = sqrt(3) * r
					h = 3 * r
					cx = c(0, w / 2)
					cy = c(0, 1.5 * r)
					th = (30 + 60 * 0:3) * pi / 180
					rings = lapply(seq_along(cx), function(i) {
						cbind(cx[i] + r * cos(th), cy[i] + r * sin(th))
					})
					geom(w, h, poly2seg(rings))
				},
				waves = ,
				zigzag = {
					# waves have a period of 2 spacings, zigzag lines of 1
					A = a$amplitude * s
					per = if (p$type == "waves") 2 * s else s
					t = if (p$type == "waves") seq(0, 1, length.out = 33) else c(0, 0.5, 1)
					ys = if (p$type == "waves") A * sin(2 * pi * t) else c(-A, A, -A)
					geom(per, s, poly2seg(list(cbind(t * per, ys + s / 2))))
				})
			if (angle == 90) transpose(g) else g
		})
}

# Clip segments (matrix x0, y0, x1, y1) to the rectangle [0, w] x [0, h] (Liang-Barsky).
# Segments on the right or top border are dropped (they belong to the next tile).
pattern_clip_segs = function(m, w, h) {
	if (is.null(m) || !nrow(m)) return(m)
	# remove floating point noise (e.g. -1e-16 for 0), which would clip lines on the border
	m = round(m, 10)
	dx = m[, "x1"] - m[, "x0"]
	dy = m[, "y1"] - m[, "y0"]
	t0 = rep(0, nrow(m))
	t1 = rep(1, nrow(m))
	keep = rep(TRUE, nrow(m))
	upd = function(p, q) {
		par = p == 0
		keep <<- keep & !(par & q < 0)
		r = q / p
		t0 <<- ifelse(!par & p < 0, pmax(t0, r), t0)
		t1 <<- ifelse(!par & p > 0, pmin(t1, r), t1)
	}
	upd(-dx, m[, "x0"])
	upd(dx, w - m[, "x0"])
	upd(-dy, m[, "y0"])
	upd(dy, h - m[, "y0"])
	keep = keep & (t0 < t1)
	res = cbind(x0 = m[, "x0"] + t0 * dx, y0 = m[, "y0"] + t0 * dy,
				x1 = m[, "x0"] + t1 * dx, y1 = m[, "y0"] + t1 * dy)[keep, , drop = FALSE]
	eps = 1e-9 * max(w, h)
	onright = abs(res[, "x0"] - w) < eps & abs(res[, "x1"] - w) < eps
	ontop = abs(res[, "y0"] - h) < eps & abs(res[, "y1"] - h) < eps
	zero = abs(res[, "x1"] - res[, "x0"]) < eps & abs(res[, "y1"] - res[, "y0"]) < eps
	res[!onright & !ontop & !zero, , drop = FALSE]
}

# Shift segments (matrix) or circles (data.frame) to the 8 neighboring tiles (and itself)
pattern_shift9 = function(x, w, h) {
	if (is.null(x) || !nrow(x)) return(x)
	sh = expand.grid(dx = c(-w, 0, w), dy = c(-h, 0, h))
	if (is.matrix(x)) {
		do.call(rbind, lapply(seq_len(nrow(sh)), function(i) {
			x + rep(c(sh$dx[i], sh$dy[i], sh$dx[i], sh$dy[i]), each = nrow(x))
		}))
	} else {
		do.call(rbind, lapply(seq_len(nrow(sh)), function(i) {
			x$x = x$x + sh$dx[i]
			x$y = x$y + sh$dy[i]
			x
		}))
	}
}

# Continuous lines of one tile: all parts of the periodic pattern within the tile, once
pattern_tile_segs = function(g) {
	m = pattern_clip_segs(pattern_shift9(g$segs, g$w, g$h), g$w, g$h)
	if (is.null(m) || !nrow(m)) return(m)
	m[!duplicated(round(m / max(g$w, g$h), 8)), , drop = FALSE]
}

# Primitives to draw a single tile (SVG pattern, PNG): the continuous lines within the tile, and the
# elements of the tile and its neighbors (parts outside the tile are clipped by the tile region)
pattern_tile_prims = function(g, margin = 0) {
	# only the elements of the neighboring tiles that reach into the tile (margin: half of the line width)
	elems = pattern_shift9(g$elems, g$w, g$h)
	if (!is.null(elems) && nrow(elems)) {
		keep = pmax(elems[, "x0"], elems[, "x1"]) >= -margin & pmin(elems[, "x0"], elems[, "x1"]) <= g$w + margin &
			pmax(elems[, "y0"], elems[, "y1"]) >= -margin & pmin(elems[, "y0"], elems[, "y1"]) <= g$h + margin
		elems = elems[keep, , drop = FALSE]
	}
	circles = pattern_shift9(g$circles, g$w, g$h)
	if (!is.null(circles) && nrow(circles)) {
		r = circles$r + margin
		keep = circles$x + r >= 0 & circles$x - r <= g$w & circles$y + r >= 0 & circles$y - r <= g$h
		circles = circles[keep, , drop = FALSE]
	}
	list(segs = rbind(pattern_tile_segs(g), elems),
		 circles = circles)
}

# Primitives covering the areas bboxes (matrix with columns xmin, ymin, xmax, ymax; inches), with
# tiles aligned to the origin (0, 0), so that patterns of neighboring features match. Only the tiles
# that overlap with at least one bounding box are generated (e.g. one per polygon ring).
pattern_area_prims = function(g, bboxes) {
	bboxes = matrix(bboxes, ncol = 4)
	ij = unique(do.call(rbind, lapply(seq_len(nrow(bboxes)), function(k) {
		b = bboxes[k, ]
		as.matrix(expand.grid(i = floor(b[1] / g$w):floor(b[3] / g$w),
							  j = floor(b[2] / g$h):floor(b[4] / g$h)))
	})))
	off = data.frame(dx = ij[, 1] * g$w, dy = ij[, 2] * g$h)
	segs = rbind(pattern_tile_segs(g), g$elems)
	if (!is.null(segs) && nrow(segs)) {
		ns = nrow(segs)
		dx = rep(off$dx, each = ns)
		dy = rep(off$dy, each = ns)
		segs = segs[rep(seq_len(ns), nrow(off)), , drop = FALSE] + cbind(dx, dy, dx, dy)
	}
	circles = g$circles
	if (!is.null(circles) && nrow(circles)) {
		nc = nrow(circles)
		circles = circles[rep(seq_len(nc), nrow(off)), , drop = FALSE]
		circles$x = circles$x + rep(off$dx, each = nc)
		circles$y = circles$y + rep(off$dy, each = nc)
	}
	raster = if (!is.null(g$raster)) c(g$raster, list(w = g$w, h = g$h, off = off))
	list(segs = segs, circles = circles, raster = raster)
}

# Periodic random noise: an n x n matrix (which repeats seamlessly) of the inverse FFT of complex Gaussian noise
# with amplitudes wgt (an n x n matrix in the frequency domain, see pattern_freq()), multiplied by mult (e.g. to
# take a derivative). The state of the global RNG is restored.
pattern_fft_noise = function(wgt, seed = 1, mult = 1) {
	n = nrow(wgt)
	has_seed = exists(".Random.seed", envir = globalenv(), inherits = FALSE)
	if (has_seed) old_seed = get(".Random.seed", envir = globalenv(), inherits = FALSE)
	on.exit(if (has_seed) assign(".Random.seed", old_seed, envir = globalenv()) else rm(".Random.seed", envir = globalenv()))
	set.seed(seed)
	z = matrix(complex(real = stats::rnorm(n * n), imaginary = stats::rnorm(n * n)), n) * wgt
	Re(stats::fft(z * mult, inverse = TRUE))
}

# Frequencies (cycles per tile) of an n x n tile: kx along columns (x), ky along rows (y, from top to bottom),
# and the radial frequency K
pattern_freq = function(n) {
	k = c(0:floor(n / 2), if (n > 2) -(ceiling(n / 2) - 1):-1)
	list(kx = matrix(k, n, n, byrow = TRUE), ky = matrix(k, n, n), K = sqrt(outer(k^2, k^2, "+")))
}

# Bilinear interpolation of a periodic m x m matrix to n x n (n >= m)
pattern_upsample = function(x, n) {
	m = nrow(x)
	if (m == n) return(x)
	p = (seq_len(n) - 1) * m / n
	i0 = floor(p)
	f = p - i0
	i1 = (i0 + 1) %% m
	A = matrix(0, n, m)
	A[cbind(seq_len(n), i0 + 1)] = 1 - f
	A[cbind(seq_len(n), i1 + 1)] = A[cbind(seq_len(n), i1 + 1)] + f
	A %*% x %*% t(A)
}

# Scale x by its q-quantile of absolute values, and clamp to [-1, 1]
pattern_norm1 = function(x, q = 0.98) {
	qx = stats::quantile(abs(x), q, names = FALSE)
	if (qx > 0) pmax(pmin(x / qx, 1), -1) else x * 0
}

# Raster textures: a matrix of n x n pixels, which repeats seamlessly, with wavelengths up to lmax pixels. Cached
# per session.
# - "terrain": random relief, shaded with light from the northwest; values in [-1, 1] (< 0 shadow, > 0 highlight)
# - "mottle": patches of paint in a few (levels) shades with sharp edges: smooth noise (wavelengths of lmax / 4 to
#   lmax, with most variation in the large ones), divided into levels classes of equal area. Values in [-1, 1]
#   (< 0 darker, > 0 brighter), one per shade. Since the classes follow the noise, a patch only borders on patches
#   of the next shades (like contour lines). With random > 0, the patches are cells instead (a Voronoi diagram
#   of jittered grid points, of which the borders are warped by noise), of which a fraction random gets a random
#   shade, and the others the shade of the noise at their center, so that neighbors can have any shade
# - "sketch": pencil strokes in the direction angle (degrees), of which the width is about lmax / 2 and the length
#   about 6 times lmax, with varying pressure and a fine grain; values are roughly standard normal: the paper
#   is visible where the value is below a threshold (see pattern_raster())
pattern_texture = function(kind, n, lmax, seed = 1, angle = 0, levels = 5, random = 0) {
	key = paste(kind, n, signif(lmax, 4), seed, if (kind == "sketch") angle, if (kind == "mottle") paste(levels, random))
	if (!is.null(.TMAP$pattern_cache[[key]])) return(.TMAP$pattern_cache[[key]])
	f = pattern_freq(n)
	K = f$K
	# no waves along the axes: these change the mean of rows (or columns), which would show as horizontal (or
	# vertical) bands, since the tiles repeat them across the map
	no_axes = function(w) {
		w[1, ] = 0
		w[, 1] = 0
		w
	}
	v = switch(kind,
		terrain = {
			# wavelengths between lmax / 4 and lmax, so that the size of the relief follows lmax, but not smaller than
			# 3 pixels: finer relief cannot be shown at this resolution, and a narrower band would give streaks
			kmax = min(4 * n / lmax, n / 3)
			kmin = min(n / lmax, kmax / 2.5)
			wgt = no_axes(ifelse(K >= kmin & K <= kmax & K > 0, K^-2, 0))
			# shading: the slope in the direction of the light (from the northwest), computed exactly in the frequency
			# domain (finite differences would favor diagonal waves near the pixel size). Highlights are where the
			# height increases to the right and downwards (rows are from top to bottom)
			pattern_norm1(pattern_fft_noise(wgt, seed, mult = 1i * (f$kx + f$ky)))
		},
		mottle = {
			# the smooth noise is computed at a lower resolution m (about 32 pixels per lmax), and interpolated
			# (bilinear, periodic), so that large patches are cheap; the classes are computed at full resolution, so
			# the edges are sharp
			m = min(n, max(16L, ceiling(32 * n / lmax)))
			fm = pattern_freq(m)
			kmax = min(4 * n / lmax, m / 3)
			kmin = min(n / lmax, kmax / 2.5)
			wgt = no_axes(ifelse(fm$K >= kmin & fm$K <= kmax & fm$K > 0, fm$K^-1.5, 0))
			v = pattern_upsample(pattern_fft_noise(wgt, seed), n)
			lev = max(2, round(levels))
			cls = findInterval(v, stats::quantile(v, seq_len(lev - 1) / lev, names = FALSE)) + 1L
			if (random > 0) cls = pattern_cells(cls, n, lmax, lev, random, seed)
			matrix(seq(-1, 1, length.out = lev)[cls], n)
		},
		sketch = {
			# strokes: waves across the direction of the strokes with a wavelength of about lmax (at least 3 pixels), and
			# hardly any along it, so they are long and thin
			r = angle * pi / 180
			kpar = f$kx * cos(r) - f$ky * sin(r)
			kperp = f$kx * sin(r) + f$ky * cos(r)
			k0 = min(n / lmax, n / 3)
			wgt = no_axes(exp(-((abs(kperp) - k0) / (0.35 * k0))^2 - (kpar / (k0 / 6))^2))
			strokes = pattern_fft_noise(wgt, seed)
			# pressure: large patches with more or fewer strokes
			kp = n / (5 * lmax)
			wp = no_axes(ifelse(K > 0, exp(-(K / kp)^2), 0))
			pressure = pattern_fft_noise(wp, seed + 1000)
			# grain of the paper: (almost) single pixels
			grain = pattern_fft_noise(no_axes(ifelse(K > n / 4, 1, 0)), seed + 2000)
			z = function(x) (x - mean(x)) / max(stats::sd(x), 1e-12)
			v = z(strokes) + 0.5 * z(pressure) + 0.35 * z(grain)
			z(v)
		})
	if (is.null(.TMAP$pattern_cache)) .TMAP$pattern_cache = list()
	.TMAP$pattern_cache[[key]] = v
	v
}

# Random patches for "mottle" (see pattern_texture()): a Voronoi diagram of a jittered grid of G x G points (a
# spacing of about lmax / 3 pixels, periodic), of which the borders are warped by smooth noise, so that the patches
# are irregular. Each cell gets a random shade (with probability random), or otherwise the class (of cls, an n x n
# matrix of classes 1 to lev) at its center. Neighboring cells with the same shade merge, so that the patches
# differ in size. Returns an n x n matrix of classes.
pattern_cells = function(cls, n, lmax, lev, random, seed = 1) {
	G = max(2L, round(3 * n / lmax))
	gp = n / G
	ng = G * G
	# jittered grid points (cell k = j * G + i + 1, i along x); the jitter is limited, so that the nearest point is
	# always one of the 3 x 3 neighboring grid cells
	fx = 0.1 + 0.8 * pattern_rand(ng, 2 * seed + 11)
	fy = 0.1 + 0.8 * pattern_rand(ng, 2 * seed + 12)
	i0 = rep(0:(G - 1), G)
	j0 = rep(0:(G - 1), each = G)
	cen = cls[cbind(floor((j0 + fy) * gp) %% n + 1, floor((i0 + fx) * gp) %% n + 1)]
	rs = pmin(lev, floor(pattern_rand(ng, 2 * seed + 13) * lev) + 1)
	shade = ifelse(pattern_rand(ng, 2 * seed + 14) < random, rs, cen)
	# warp: smooth noise with wavelengths between 1 and 3 grid spacings, of up to about half a spacing
	mw = min(n, max(16L, 8L * G))
	K = pattern_freq(mw)$K
	wgt = ifelse(K >= G / 3 & K <= G, 1, 0)
	wgt[1, ] = 0
	wgt[, 1] = 0
	warp = function(sd) {
		x = pattern_upsample(pattern_fft_noise(wgt, sd), n)
		x / max(stats::sd(x), 1e-12) * 0.18 * gp
	}
	X = matrix(seq_len(n) - 0.5, n, n, byrow = TRUE) + warp(seed + 3000)
	Y = matrix(seq_len(n) - 0.5, n, n) + warp(seed + 4000)
	ci = floor(X / gp)
	cj = floor(Y / gp)
	best = matrix(Inf, n, n)
	lab = matrix(1L, n, n)
	for (di in -1:1) for (dj in -1:1) {
		ii = ci + di
		jj = cj + dj
		k = (jj %% G) * G + (ii %% G) + 1L
		d = (X - (ii + fx[k]) * gp)^2 + (Y - (jj + fy[k]) * gp)^2
		upd = d < best
		best[upd] = d[upd]
		lab[upd] = k[upd]
	}
	matrix(shade[lab], n)
}

# Raster image (matrix of colors, see grDevices::as.raster()) of a raster texture r (from pattern_geom())
# for a tile of wpx x hpx pixels at dpi pixels per inch:
# - "terrain" and "mottle": highlights in white and shadows in col, of which the opacity is the strength. For
#   "mottle", the shades differ subtly: the highlights are weaker than the shadows, since white quickly
#   washes out the fill color; contrast multiplies both
# - "sketch": the paper (white) where there are no pencil strokes; the strength is the fraction of paper,
#   from 0 (solid fill) to 0.65 (strength 1)
pattern_raster = function(r, wpx, hpx, dpi, col = "grey25", alpha = 1) {
	n = max(wpx, hpx)
	kind = if (is.null(r$kind)) "terrain" else r$kind
	v = pattern_texture(kind, n, r$lmax * dpi, seed = r$seed, angle = r$angle,
						levels = if (is.null(r$levels)) 5 else r$levels,
						random = if (is.null(r$random)) 0 else r$random)[seq_len(hpx), seq_len(wpx), drop = FALSE]
	if (kind == "sketch") {
		# the threshold below which the paper is visible, with a soft edge of a few pixels
		t = stats::qnorm(0.65 * min(r$strength, 1))
		a = pmin(1, pmax(0, (t - v) / 0.3 + 0.5)) * alpha
		return(grDevices::as.raster(matrix(grDevices::rgb(1, 1, 1, a), hpx, wpx)))
	}
	if (kind == "mottle") v = v * ifelse(v > 0, 0.2, 0.4) * (if (is.null(r$contrast)) 1 else r$contrast)
	a = pmin(1, abs(v) * r$strength * alpha * grDevices::col2rgb(col, alpha = TRUE)[4] / 255)
	cs = grDevices::col2rgb(col)
	img = ifelse(v > 0,
				 grDevices::rgb(1, 1, 1, a),
				 grDevices::rgb(cs[1] / 255, cs[2] / 255, cs[3] / 255, a))
	grDevices::as.raster(matrix(img, hpx, wpx))
}

# Pixels per inch of the current device (for raster textures): the dpi of tmap_save(), 200 for vector devices
# (pdf, svg, ...), and otherwise the resolution reported by the device. Some devices (e.g. png(type = "quartz"))
# report 72 regardless of their resolution, so it is at least 144.
pattern_device_dpi = function() {
	d = .TMAP$device_dpi
	if (!is.null(d) && identical(d$dev, grDevices::dev.cur())) return(d$dpi)
	dev = names(grDevices::dev.cur())
	if (any(startsWith(dev, c("pdf", "cairo_pdf", "postscript", "cairo_ps", "svg", "devSVG", "xfig", "pictex")))) return(200)
	r = tryCatch(graphics::par("cra")[2] / graphics::par("cin")[2], error = function(e) NA)
	if (is.na(r) || !is.finite(r)) return(144)
	min(max(r, 144), 600)
}

# Grob of primitives (inches, relative to the current viewport). Each kind of primitive is drawn
# as one path: within a clipping path, the drawing time is mainly determined by the number of
# strokes/fills, so one path with many subpaths is much faster than many segments or circles.
# Segments are subpaths of two points; closed with bevel joins, they look like butt-ended lines.
# Circles are polygons; dots are filled with the "winding" rule, so overlapping dots merge.
pattern_prims_grob = function(pr, col = "grey25", lwd = 1, alpha = 1) {
	grobs = list()
	if (!is.null(pr$segs) && nrow(pr$segs)) {
		m = pr$segs
		n = nrow(m)
		gp = grid::gpar(col = col, fill = NA, lwd = lwd, alpha = alpha, linejoin = "bevel")
		grobs = c(grobs, list(grid::pathGrob(x = c(rbind(m[, "x0"], m[, "x1"])),
											 y = c(rbind(m[, "y0"], m[, "y1"])),
											 id = rep(seq_len(n), each = 2L),
											 default.units = "in", gp = gp)))
	}
	cc = pr$circles
	if (!is.null(cc) && nrow(cc)) {
		circ_path = function(d, k, gp, rule) {
			th = seq(0, 2 * pi, length.out = k + 1L)[-1L]
			grid::pathGrob(x = c(outer(cos(th), d$r) + rep(d$x, each = k)),
						   y = c(outer(sin(th), d$r) + rep(d$y, each = k)),
						   id = rep(seq_len(nrow(d)), each = k),
						   default.units = "in", rule = rule, gp = gp)
		}
		if (any(cc$filled)) {
			gp = grid::gpar(fill = col, col = NA, alpha = alpha)
			grobs = c(grobs, list(circ_path(cc[cc$filled, ], 12L, gp, "winding")))
		}
		if (any(!cc$filled)) {
			gp = grid::gpar(fill = NA, col = col, lwd = lwd, alpha = alpha)
			grobs = c(grobs, list(circ_path(cc[!cc$filled, ], 24L, gp, "evenodd")))
		}
	}
	rr = pr$raster
	if (!is.null(rr) && nrow(rr$off)) {
		dpi = pattern_device_dpi()
		wpx = max(1L, round(rr$w * dpi))
		hpx = max(1L, round(rr$h * dpi))
		img = pattern_raster(rr, wpx, hpx, dpi, col = col, alpha = alpha)
		# the tiles are aligned to the pixels of the device (whole pixels, and shifted by the position of the viewport
		# on the device), since seams between semi-transparent tiles would be visible
		loc = tryCatch(grid::deviceLoc(grid::unit(0, "npc"), grid::unit(0, "npc"), valueOnly = TRUE),
					   error = function(e) list(x = 0, y = 0))
		fx = (loc$x * dpi) %% 1
		fy = (loc$y * dpi) %% 1
		tw = wpx / dpi
		th = hpx / dpi
		grobs = c(grobs, list(grid::rasterGrob(img, x = grid::unit(round(rr$off$dx / rr$w) * tw - fx / dpi, "in"),
											   y = grid::unit(round(rr$off$dy / rr$h) * th - fy / dpi, "in"),
											   width = grid::unit(tw, "in"), height = grid::unit(th, "in"),
											   just = c("left", "bottom"), interpolate = FALSE)))
	}
	do.call(grid::gList, grobs)
}

# Pattern drawn within the outline of a grob (plot mode), e.g. polygons from sf::st_as_grob(),
# or a rectangle for legend items. The pattern is generated at drawing time for the bounding
# box of the grob, and clipped by it (clipping paths, R >= 4.1). This is used instead of
# grid::pattern() fills, since tiling patterns are not drawn correctly on some devices (cairo).
#
# Throughout the converters, scale only affects the spacing; size is the final line width
# (lwd), so the caller applies the scale factors to size (as for other line widths).
#
# @param line_in height of a line in inches (o$lin); NA: computed at drawing time from the
#   current font size
# @param clip_sf the geometry (sfc) of which clip is the grob (in the native units of the viewport); used to
#   clip the pattern geometrically on devices that do not support clipping paths
pattern_area_grob = function(clip, spec, density, size = 1, col = "grey25", scale = 1, alpha = 1,
							 line_in = NA, name = NULL, clip_sf = NULL) {
	pspec = list(spec = spec, density = density, size = size, col = col, scale = scale,
				 alpha = alpha, line_in = line_in)
	grid::gTree(clip = clip, clip_sf = clip_sf, pspec = pspec, name = name, cl = "tmap_pattern_area")
}

# Does the current graphics device support clipping paths? Devices that do not support them (e.g. the
# windows() device, also as backend of the RStudio graphics device) may report FALSE or NA (unknown),
# and would ignore the clipping path, so that patterns run outside the polygons (#1135). In that case,
# the pattern is clipped geometrically. The option tmap.pattern.clip ("path" or "geometry") overrides this.
pattern_clip_paths = function() {
	opt = getOption("tmap.pattern.clip")
	if (!is.null(opt)) return(identical(opt, "path"))
	caps = tryCatch(grDevices::dev.capabilities("clippingPaths")$clippingPaths, error = function(e) NA)
	isTRUE(caps)
}

# Clip the primitives (inches) geometrically by the geometry clip_sf (native units of the current viewport):
# line segments are intersected with the polygons, and elements are kept when their center lies within them
pattern_clip_geometry = function(pr, clip_sf) {
	# native units -> inches (linear)
	x0 = grid::convertX(grid::unit(0, "native"), "inch", valueOnly = TRUE)
	x1 = grid::convertX(grid::unit(1, "native"), "inch", valueOnly = TRUE)
	y0 = grid::convertY(grid::unit(0, "native"), "inch", valueOnly = TRUE)
	y1 = grid::convertY(grid::unit(1, "native"), "inch", valueOnly = TRUE)
	geom = sf::st_set_crs(sf::st_geometry(clip_sf), NA)
	geom = geom * matrix(c(x1 - x0, 0, 0, y1 - y0), 2) + c(x0, y0)
	poly = suppressWarnings(sf::st_union(sf::st_make_valid(geom)))

	if (!is.null(pr$segs) && nrow(pr$segs)) {
		m = pr$segs
		lines = sf::st_sfc(sf::st_multilinestring(lapply(seq_len(nrow(m)), function(i) {
			matrix(m[i, c("x0", "x1", "y0", "y1")], 2)
		})))
		inter = suppressWarnings(sf::st_intersection(lines, poly))
		crds = if (length(inter)) sf::st_coordinates(inter) else NULL
		pr$segs = if (is.null(crds) || !nrow(crds)) NULL else {
			# consecutive points of the same line part form segments
			lid = if ("L2" %in% colnames(crds)) paste(crds[, "L1"], crds[, "L2"]) else crds[, "L1"]
			k = which(lid[-1] == lid[-nrow(crds)])
			cbind(x0 = crds[k, "X"], y0 = crds[k, "Y"], x1 = crds[k + 1, "X"], y1 = crds[k + 1, "Y"])
		}
	}
	if (!is.null(pr$circles) && nrow(pr$circles)) {
		cc = pr$circles
		pts = sf::st_sfc(lapply(seq_len(nrow(cc)), function(i) sf::st_point(c(cc$x[i], cc$y[i]))))
		inside = lengths(sf::st_intersects(pts, poly)) > 0
		pr$circles = cc[inside, , drop = FALSE]
	}
	pr
}

# Bounding boxes (inches) of the clipping grob: one per ring for path/polygon grobs (as created by
# sf::st_as_grob()), otherwise the bounding box of the grob
pattern_clip_bboxes = function(cl) {
	if (inherits(cl, c("pathgrob", "polygon")) && length(cl$x) > 1) {
		x = grid::convertX(cl$x, "in", valueOnly = TRUE)
		y = grid::convertY(cl$y, "in", valueOnly = TRUE)
		ring = if (!is.null(cl$id.lengths)) {
			rep(seq_along(cl$id.lengths), cl$id.lengths)
		} else if (!is.null(cl$id)) cl$id else rep(1L, length(x))
		cbind(tapply(x, ring, min, na.rm = TRUE), tapply(y, ring, min, na.rm = TRUE),
			  tapply(x, ring, max, na.rm = TRUE), tapply(y, ring, max, na.rm = TRUE))
	} else {
		cbind(grid::convertX(grid::grobX(cl, "west"), "in", valueOnly = TRUE),
			  grid::convertY(grid::grobY(cl, "south"), "in", valueOnly = TRUE),
			  grid::convertX(grid::grobX(cl, "east"), "in", valueOnly = TRUE),
			  grid::convertY(grid::grobY(cl, "north"), "in", valueOnly = TRUE))
	}
}

#' @export
#' @importFrom grid makeContent
makeContent.tmap_pattern_area = function(x) {
	ps = x$pspec
	line_in = if (is.na(ps$line_in)) {
		grid::convertHeight(grid::unit(1, "lines"), "inch", valueOnly = TRUE)
	} else ps$line_in
	s = pattern_spacing(ps$density, scale = ps$scale, line_in = line_in)
	g = pattern_geom(ps$spec, s, lwd = ps$size, density = ps$density)
	if (is.null(g)) return(grid::setChildren(x, grid::gList()))

	cl = x$clip
	# A clipping path replaces the clipping region of the parent viewports (e.g. the map frame),
	# so the pattern is also limited to the current viewport: the tiles to its extent, and the
	# primitives clipped to it (dots are kept when their center lies within it)
	vw = grid::convertWidth(grid::unit(1, "npc"), "inch", valueOnly = TRUE)
	vh = grid::convertHeight(grid::unit(1, "npc"), "inch", valueOnly = TRUE)
	bb = matrix(pattern_clip_bboxes(cl), ncol = 4)
	bb = cbind(pmax(bb[, 1], 0), pmax(bb[, 2], 0), pmin(bb[, 3], vw), pmin(bb[, 4], vh))
	bb = bb[bb[, 1] < bb[, 3] & bb[, 2] < bb[, 4], , drop = FALSE]
	if (!nrow(bb)) return(grid::setChildren(x, grid::gList()))
	pr = pattern_area_prims(g, bb)
	pr$segs = pattern_clip_segs(pr$segs, vw, vh)
	if (!is.null(pr$circles) && nrow(pr$circles)) {
		cc = pr$circles
		pr$circles = cc[cc$x >= 0 & cc$x <= vw & cc$y >= 0 & cc$y <= vh, , drop = FALSE]
	}

	cvp = grid::current.viewport()
	if (pattern_clip_paths()) {
		vp = grid::viewport(xscale = cvp$xscale, yscale = cvp$yscale, clip = cl)
	} else {
		# no support for clipping paths: clip geometrically (map), or by the viewport (legend items,
		# which are clipped by a rectangle filling the viewport)
		if (!is.null(x$clip_sf)) {
			pr = tryCatch(pattern_clip_geometry(pr, x$clip_sf), error = function(e) pr)
			# raster textures cannot be clipped geometrically
			if (!is.null(pr$raster)) {
				cli::cli_warn("The pattern type {.val {pr$raster$kind}} requires a graphics device that supports clipping paths (e.g. png() with type = \"cairo\", pdf(), or ragg::agg_png()), so it is not drawn",
							  .frequency = "once", .frequency_id = paste0("pattern_raster_clip_", pr$raster$kind))
				pr$raster = NULL
			}
		}
		vp = grid::viewport(xscale = cvp$xscale, yscale = cvp$yscale, clip = "on")
	}
	prims = pattern_prims_grob(pr, col = ps$col, lwd = ps$size, alpha = ps$alpha)
	grid::setChildren(x, grid::gList(grid::gTree(children = prims, vp = vp)))
}

# SVG <pattern> element (view mode). Units: CSS pixels (96 per inch)
pattern_svg = function(spec, density, size = 1, col = "grey25", id = "tmap-pattern", scale = 1,
						line_in = 0.2, alpha = 1, dpi = 96, bg = NA, bg_alpha = 1) {
	g = pattern_geom(spec, pattern_spacing(density, scale = scale, line_in = line_in), lwd = size, density = density)
	if (is.null(g)) return(NULL)
	f = function(x) format(round(x * dpi, 3), scientific = FALSE, trim = TRUE)
	colh = substr(grDevices::rgb(t(grDevices::col2rgb(col)), maxColorValue = 255), 1, 7)
	opa = alpha * grDevices::col2rgb(col, alpha = TRUE)[4] / 255
	lw = size * dpi / 96
	pr = pattern_tile_prims(g, margin = size / 96 / 2)
	els = character()
	if (!is.null(pr$segs) && nrow(pr$segs)) {
		m = pr$segs
		els = c(els, paste0('<line x1="', f(m[, "x0"]), '" y1="', f(g$h - m[, "y0"]),
							'" x2="', f(m[, "x1"]), '" y2="', f(g$h - m[, "y1"]), '"/>'))
	}
	if (!is.null(pr$circles) && nrow(pr$circles)) {
		cc = pr$circles
		fill = ifelse(cc$filled, paste0(' fill="', colh, '" stroke="none"'), ' fill="none"')
		els = c(els, paste0('<circle cx="', f(cc$x), '" cy="', f(g$h - cc$y), '" r="', f(cc$r), '"',
							fill, '/>'))
	}
	if (!is.null(g$raster)) {
		# raster texture: a PNG tile at twice the resolution (sharp on high resolution screens)
		uri = pattern_data_uri(spec, density = density, size = size, col = col, scale = scale, line_in = line_in,
							   alpha = alpha, dpi = 2 * dpi)
		els = c(els, paste0('<image href="', uri, '" x="0" y="0" width="', f(g$w), '" height="', f(g$h),
							'" preserveAspectRatio="none"/>'))
	}
	# background (the fill color of the feature), so that fill and pattern can be drawn as one path
	bgrect = if (is.null(bg) || is.na(bg)) "" else {
		bgh = substr(grDevices::rgb(t(grDevices::col2rgb(bg)), maxColorValue = 255), 1, 7)
		bgo = (if (is.na(bg_alpha)) 1 else bg_alpha) * grDevices::col2rgb(bg, alpha = TRUE)[4] / 255
		paste0('<rect width="', f(g$w), '" height="', f(g$h), '" fill="', bgh, '" fill-opacity="', bgo,
			   '" stroke="none"/>')
	}
	paste0('<pattern id="', id, '" patternUnits="userSpaceOnUse" width="', f(g$w),
		   '" height="', f(g$h), '">', bgrect,
		   '<g stroke="', colh, '" stroke-width="', lw, '" stroke-opacity="', opa,
		   '" fill="none" fill-opacity="', opa, '" stroke-linecap="butt">',
		   paste(els, collapse = ""), '</g></pattern>')
}

# PNG file of one tile (web modes, legend swatches). Cached in the session temp directory.
# The tile size is rounded to whole pixels (the geometry is stretched slightly) to avoid seams.
#' @export
#' @rdname tmap_internal
pattern_png = function(spec, density, size = 1, col = "grey25", scale = 1, line_in = 0.2, alpha = 1,
						dpi = 96, bg = NA) {
	g = pattern_geom(spec, pattern_spacing(density, scale = scale, line_in = line_in), lwd = size, density = density)
	if (is.null(g)) return(NULL)
	key = rlang::hash(list(spec, density, size, col, scale, line_in, alpha, dpi, bg))
	file = file.path(tempdir(), paste0("tmap_pattern_", key, ".png"))
	if (file.exists(file)) return(file)

	wpx = max(1L, round(g$w * dpi))
	hpx = max(1L, round(g$h * dpi))
	grDevices::png(file, width = wpx, height = hpx, res = dpi,
				   bg = if (is.na(bg)) "transparent" else bg)
	on.exit(grDevices::dev.off())
	grid::grid.newpage()
	# stretch the tile to the rounded pixel size
	grid::pushViewport(grid::viewport(x = 0, y = 0, just = c("left", "bottom"),
									  width = grid::unit(g$w, "in"),
									  height = grid::unit(g$h, "in")))
	if (!is.null(g$raster)) {
		grid::grid.raster(pattern_raster(g$raster, wpx, hpx, dpi, col = col, alpha = alpha),
						  x = 0, y = 0, width = 1, height = 1, just = c("left", "bottom"), interpolate = FALSE)
		return(file)
	}
	sx = (wpx / dpi) / g$w
	sy = (hpx / dpi) / g$h
	pr = pattern_tile_prims(g, margin = size / 96 / 2)
	if (!is.null(pr$segs)) pr$segs = pr$segs * rep(c(sx, sy, sx, sy), each = nrow(pr$segs))
	if (!is.null(pr$circles)) {
		pr$circles$x = pr$circles$x * sx
		pr$circles$y = pr$circles$y * sy
	}
	grid::grid.draw(pattern_prims_grob(pr, col = col, lwd = size, alpha = alpha))
	file
}

# Data URI of a PNG tile, e.g. for CSS background-image of legend swatches
pattern_data_uri = function(...) {
	file = pattern_png(...)
	if (is.null(file)) return(NA_character_)
	paste0("data:image/png;base64,", base64enc::base64encode(file))
}

pattern_css = function(...) {
	uri = pattern_data_uri(...)
	if (is.na(uri)) return("")
	paste0("background-image: url(", uri, "); background-repeat: repeat;")
}

# PNG file of a legend swatch (view mode): fill, pattern, and border, px x px pixels at 96 dpi,
# so that the pattern has the same size as on the map (CSS pixels). Cached in the session temp
# directory.
#' @export
#' @rdname tmap_internal
pattern_swatch_png = function(spec, density, size = 1, col = "grey25", fill = NA, fill_alpha = 1,
							  border = NA, border_alpha = 1, lwd = 1, scale = 1, px = 24) {
	na1 = function(x, d) if (is.null(x) || is.na(x)) d else x
	key = rlang::hash(list("swatch", spec, density, size, col, fill, fill_alpha, border,
						   border_alpha, lwd, scale, px))
	file = file.path(tempdir(), paste0("tmap_swatch_", key, ".png"))
	if (!file.exists(file)) {
		grDevices::png(file, width = px, height = px, res = 96, bg = "transparent")
		inner = grid::unit(1, "npc") - grid::unit(na1(lwd, 1), "points")
		r = grid::rectGrob(width = inner, height = inner)
		gp_fill = grid::gpar(fill = na1(fill, NA), alpha = na1(fill_alpha, 1), col = NA)
		grid::grid.draw(grid::editGrob(r, gp = gp_fill))
		if (!is.na(spec) && spec != "none") {
			pg = pattern_area_grob(r, spec, density = na1(density, 0.8), size = na1(size, 1),
								   col = na1(col, "grey25"), scale = scale, line_in = 0.2)
			grid::grid.draw(pg)
		}
		gp_border = grid::gpar(fill = NA, col = na1(border, NA), alpha = na1(border_alpha, 1),
							   lwd = na1(lwd, 1))
		grid::grid.draw(grid::editGrob(r, gp = gp_border))
		grDevices::dev.off()
	}
	file
}
