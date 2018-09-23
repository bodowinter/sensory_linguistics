library(plotrix)	# for plotting circles

boxplot_fnc <- function(lyn, y, ylims = c(0, 5),
	ylab = 'Log10 Frequency', which_x = 0,
	y_axis = seq(0, 5, 1), hlty = 1) {

	x <- boxplot(as.formula(str_c(y, ' ~ Clust')), lyn, plot = F)$stats
	quartz('', 11, 6.5)
	par(mai = c(2, 1.5, 0.5, 0.5))
	plot(1, 1, type = 'n',
		xlim = c(1, 13),
		ylim = ylims,
		xlab = '', ylab = '',
		xaxt = 'n', yaxt = 'n',
		bty = 'n')
	abline(h = which_x, lty = hlty, lwd = 2)
	mtext(side = 2, text = ylab, cex = 1.8,
		font = 2, line = 3.2)
	axis(side = 1,
		at = seq(1.5, 12.5, 1),
		labels = class_order,
		las = 2, font = 2,
		cex.axis = 1.15,
		col.ticks = NA,
		col = NA,
		line = -1.45,
		lwd.ticks = 2)
	axis(side = 2, at = y_axis,
		lwd = 2, lwd.ticks = 2,
		font = 2, las = 2)
		
	for (i in 1:12) {	# plot rectangles
		segments(x0 = i + 0.5,
		x1 = i + 0.5,
		y0 = x[1, i],
		y1 = x[5, i], lwd = 2)
	rect(xleft = i + x_fac,
		xright = i + 1 - x_fac,
		ybottom = x[2, i],
		ytop = x[4, i], col = color_order[i],
		lwd = 2)
	segments(x0 = i + x_fac,
		x1 = i + 1 - x_fac, lwd = 2,
		y0 = x[3, i])
	rasterImage(get(image_order[i]),
		xleft = i + x_fac_img,
		xright = i + 1 - x_fac_img,
		ybottom = x[3, i] - diff(ylims) * y_fac_img,
		ytop = x[3, i] + diff(ylims) * y_fac_img)
	draw.circle(x = i + 0.5,
		y = x[3, i], radius = 0.2, lwd = 1.5)
		}
	}