## Bodo Winter
## September 17, 2015; Edited August 1, 2017

## Function for letters:

plot_AB <- function(AB = '(a)',
	xlim = c(0.5, 5.5), ylim = c(0, 10),
	xfactor = 0.03, yfactor = 0.04) {
	xrange <- abs(xlim[2] - xlim[1])
	yrange <- abs(ylim[2] - ylim[1])
	xpos <- xlim[1] + (xrange * xfactor)
	ypos <- ylim[2] - (yrange * yfactor)
	text(xpos, ypos, labels = AB, font = 2, cex = 1.5)
	}

## Function for setting up an empty plot:

emptyplot <- function(xlim = c(0.5, 5.5), ylim = c(0, 10),
	AB = '', xfactor = 0.03, yfactor = 0.04, ...) {
	plot(1, 1, xlab = '', ylab = '', xaxt = 'n',
		yaxt = 'n', type = 'n', xlim = xlim, ylim = ylim, ...)
	box(lwd = 2)
	if (AB != '' & length(AB) > 0) {
		plot_AB(xlim, ylim, AB = AB,
		xfactor = xfactor, yfactor = yfactor)
		}
	}