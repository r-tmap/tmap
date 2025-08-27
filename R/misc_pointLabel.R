# function adapted from car::pointLabel
# also used in tmap3
# changes are documented
# input: x, y, width and height of rectangles, bounding box (in 'sf' format')
pointLabel2 <- function (x, y, width, height, bbx,
						 method = c("SANN", "GA"),
						 gap = 0,
						 trace = FALSE, ...) {

	asp = tmaptools::get_asp_ratio(bbx)

	toUnityCoords <- function(xy) {
		if (asp > 1) {
			list(x = (xy$x - bbx[1])/(bbx[3] - bbx[1]) * asp,
				 y = (xy$y - bbx[2])/(bbx[4] - bbx[2]))
		} else {
			list(x = (xy$x - bbx[1])/(bbx[3] - bbx[1]),
				 y = (xy$y - bbx[2])/(bbx[4] - bbx[2])/asp)
		}


	}
	toUserCoords <- function(xy) {
		if (asp > 1) {
			list(x = bbx[1] + xy$x/asp * (bbx[3] - bbx[1]),
				 y = bbx[2] + xy$y * (bbx[4] - bbx[2]))
		} else {
			list(x = bbx[1] + xy$x * (bbx[3] - bbx[1]),
				 y = bbx[2] + xy$y * asp * (bbx[4] - bbx[2]))
		}

	}

	z <- grDevices::xy.coords(x, y, recycle = TRUE)
	z <- toUnityCoords(z)
	x <- z$x
	y <- z$y

	# CHANGED: width and height are specified by user
	width <- ((width + gap) / (bbx[3] - bbx[1])) * asp
	height <- ((height + gap) / (bbx[4] - bbx[2])) / asp


	method <- match.arg(method)
	n_labels <- length(x)

	gen_offset <- function(code) c(-1, -1, -1, 0, 0, 1, 1, 1)[code] *
		(width/2) + (0+1i) * c(-1, 0, 1, -1, 1, -1, 0, 1)[code] *
		(height/2)
	rect_intersect <- function(xy1, offset1, xy2, offset2) {
		w <- pmin(Re(xy1 + offset1/2), Re(xy2 + offset2/2)) -
			pmax(Re(xy1 - offset1/2), Re(xy2 - offset2/2))
		h <- pmin(Im(xy1 + offset1/2), Im(xy2 + offset2/2)) -
			pmax(Im(xy1 - offset1/2), Im(xy2 - offset2/2))
		w[w <= 0] <- 0
		h[h <= 0] <- 0
		w * h
	}

	objective <- function(gene) {
		offset <- gen_offset(gene)
		if (!is.null(rectidx1))
			area <- sum(rect_intersect(xy[rectidx1] + offset[rectidx1],
									   rectv[rectidx1], xy[rectidx2] + offset[rectidx2],
									   rectv[rectidx2]))
		else area <- 0
		n_outside <- sum(Re(xy + offset - rectv/2) < 0 | Re(xy +
																offset + rectv/2) > asp | Im(xy + offset - rectv/2) <
						 	0 | Im(xy + offset + rectv/2) > 1/asp)
		res <- 1000 * area + n_outside
		res
	}
	xy <- x + (0+1i) * y
	rectv <- width + (0+1i) * height
	rectidx1 <- rectidx2 <- array(0, (length(x)^2 - length(x))/2)
	k <- 0
	for (i in 1:length(x)) for (j in seq_len(i - 1)) {
		k <- k + 1
		rectidx1[k] <- i
		rectidx2[k] <- j
	}
	canIntersect <- rect_intersect(xy[rectidx1], 2 * rectv[rectidx1],
								   xy[rectidx2], 2 * rectv[rectidx2]) > 0
	rectidx1 <- rectidx1[canIntersect]
	rectidx2 <- rectidx2[canIntersect]
	if (trace)
		cat("possible intersects =", length(rectidx1), "\n")
	if (trace)
		cat("portion covered =", sum(rect_intersect(xy, rectv,
													xy, rectv)), "\n")
	GA <- function() {
		n_startgenes <- 1000
		n_bestgenes <- 30
		prob <- 0.2
		mutate <- function(gene) {
			offset <- gen_offset(gene)
			doesIntersect <- rect_intersect(xy[rectidx1] + offset[rectidx1],
											rectv[rectidx1], xy[rectidx2] + offset[rectidx2],
											rectv[rectidx2]) > 0
			for (i in which(doesIntersect)) {
				gene[rectidx1[i]] <- sample(1:8, 1)
			}
			for (i in seq(along = gene)) if (runif(1) <= prob)
				gene[i] <- sample(1:8, 1)
			gene
		}
		crossbreed <- function(g1, g2) ifelse(sample(c(0, 1),
													 length(g1), replace = TRUE) > 0.5, g1, g2)
		genes <- matrix(sample(1:8, n_labels * n_startgenes,
							   replace = TRUE), n_startgenes, n_labels)
		for (i in 1:10) {
			scores <- array(0, NROW(genes))
			for (j in 1:NROW(genes)) scores[j] <- objective(genes[j,
																  ])
			rankings <- order(scores)
			genes <- genes[rankings, ]
			bestgenes <- genes[1:n_bestgenes, ]
			bestscore <- scores[rankings][1]
			if (bestscore == 0) {
				if (trace)
					cat("overlap area =", bestscore, "\n")
				break
			}
			genes <- matrix(0, n_bestgenes^2, n_labels)
			for (j in 1:n_bestgenes) for (k in 1:n_bestgenes) genes[n_bestgenes *
																		(j - 1) + k, ] <- mutate(crossbreed(bestgenes[j,
																													  ], bestgenes[k, ]))
			genes <- rbind(bestgenes, genes)
			if (trace)
				cat("overlap area =", bestscore, "\n")
		}
		nx <- Re(xy + gen_offset(bestgenes[1, ]))
		ny <- Im(xy + gen_offset(bestgenes[1, ]))
		list(x = nx, y = ny)
	}
	SANN <- function() {
		gene <- rep(8, n_labels)
		score <- objective(gene)
		bestgene <- gene
		bestscore <- score
		T <- 2.5
		for (i in 1:50) {
			k <- 1
			for (j in 1:50) {
				newgene <- gene
				newgene[sample(1:n_labels, 1)] <- sample(1:8,
														 1)
				newscore <- objective(newgene)
				if (newscore <= score || runif(1) < exp((score -
														 newscore)/T)) {
					k <- k + 1
					score <- newscore
					gene <- newgene
				}
				if (score <= bestscore) {
					bestscore <- score
					bestgene <- gene
				}
				if (bestscore == 0 || k == 10)
					break
			}
			if (bestscore == 0)
				break
			if (trace)
				cat("overlap area =", bestscore, "\n")
			T <- 0.9 * T
		}
		if (trace)
			cat("overlap area =", bestscore, "\n")
		nx <- Re(xy + gen_offset(bestgene))
		ny <- Im(xy + gen_offset(bestgene))
		list(x = nx, y = ny)
	}
	if (method == "SANN")
		xy <- SANN()
	else xy <- GA()
	xy <- toUserCoords(xy)
	invisible(xy)
}
