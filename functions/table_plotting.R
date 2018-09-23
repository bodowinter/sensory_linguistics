## Function for plotting raster:

plot_raster <- function() {
	rect(xleft = 0, xright = 5, ybottom = 0, ytop = 5,
		lwd = line_width + 2.5, xpd = NA)
	segments(x0 = 1:4, y0 = 0, y1 = 5, lwd = line_width)
	segments(y0 = 1:4, x0 = 0, x1 = 5, lwd = line_width)
	}

## Function for plotting axes:

plot_axes <- function(x) {
	for (i in 0:4) {
		rasterImage(get(x[i + 1]),
			xleft = 0.1 + i, xright = 0.9 + i,
			ybottom = 5.1, ytop = 5.9, xpd = NA)
		rasterImage(get(x[5 - i]),
			xleft = -0.9, xright = -0.1,
			ybottom = 0.1 + i, ytop = 0.9 + i, xpd = NA)
		}	
	}
	
## Function for plotting Shen boxes:

plot_shen <- function() {
	rect(xleft = 1, xright = 5,
		ybottom = 4, ytop = 5, col = shen_color)
	rect(xleft = 2, xright = 5,
		ybottom = 3, ytop = 4, col = shen_color)
	rect(xleft = 3, xright = 5,
		ybottom = 2, ytop = 3, col = shen_color)
	rect(xleft = 4, xright = 5,
		ybottom = 1, ytop = 2, col = shen_color)
	rect(xleft = 3, xright = 4,
		ybottom = 0, ytop = 1, col = shen_color)
	}

## Create function for plotting table:

plot_counts <- function(xtab, cex = 2, ypos = 0.5, stdres = F, middle = T) {
	for (i in 1:5) {
		for (j in 1:5) {
			if (i != j) {
				if (stdres) {
						text(ifelse(xtab[i, j] > 0, paste0('+', xtab[i, j]), xtab[i, j]),
							x = j - 0.5, y = 5 + ypos - i,
							font = 2, cex = cex)
					} else {
						text(xtab[i, j],
							x = j - 0.5, y = 5 + ypos - i,
						font = 2, cex = cex)
					}
				}
			if (middle) {
			if (i == j) {
				text(str_c('(', xtab[i, j], ')'),
					x = j - 0.5, y = 5 + 0.5 - i,
					font = 2, cex = cex)
				}
			}
			}
		}
	}

## Create function for plotting table:

plot_percent <- function(xtab, cex = 2, ypos = 0.5) {
	for (i in 1:5) {
		for (j in 1:5) {
			if (i != j) {
				text(str_c(xtab[i, j], '%'),
					x = j - 0.5, y = 5 + ypos - i,
					font = 2, cex = cex)
				}
			if (i == j) {
				text(str_c('(', str_c(xtab[i, j], '%'), ')'),
					x = j - 0.5, y = 5 + 0.5 - i,
					font = 2, cex = cex)
				}
			}
		}
	}

## Plot rowsums and column sums:

plot_margins <- function(xtab, rect = T, cex = 2, percent = F) {
	diag(xtab) <- NA
	if (rect) {
		rect(xleft = 0:4, xright = 1:5,
			ybottom = -0.7, ytop = 0, xpd = NA, lwd = line_width)
		rect(ybottom = 0:4, ytop = 1:5,
			xleft = 5, xright = 5.7, xpd = NA, lwd = line_width)
		}
	by_rows <- rowSums(xtab, na.rm = T)
	by_columns <- colSums(xtab, na.rm = T)
	if (percent) {
			text(str_c(round(colMeans(xtab, na.rm = T), 0), '%'),
				x = 0:4 + 0.5, y = -0.35, xpd = NA, font = 2, cex = cex)
			text(rev(str_c(round(rowMeans(xtab, na.rm = T), 0), '%')),
				x = 5.35, y = 0:4 + 0.5, xpd = NA, font = 2, cex = cex)
		} else {
			text(by_columns,
				x = 0:4 + 0.5, y = -0.35, xpd = NA, font = 2, cex = cex)
			text(rev(by_rows),
				x = 5.35, y = 0:4 + 0.5, xpd = NA, font = 2, cex = cex)
			}
	}


