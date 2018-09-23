## Bodo Winter
## September 17, 2015
## Plotting functions for dissertation
## Function for plotting density:

## Function for computing density for a specific value, given a bandwidth:
## Code from 'user10525':
## http://stats.stackexchange.com/questions/32093/how-to-draw-mean-median-and-mode-lines-in-r-that-end-at-density

kg <- function(x, vals) return(mean(dnorm((x - vals) / bw.nrd0(vals))) / bw.nrd0(vals))

## The actual density function:

plot_density <- function(x, mean = T,
	this_color = rgb(0.8, 0.6, 0.2, 0.4),
	transparency = T, mean_color = 'black',
	border_col = 'black') {
		
	## Compute density:
	
	dens_obj <- density(x)

	## Extract the density:

	dens <- as.data.frame(dens_obj[1:2])

	## Restrict densities to observed range:

	dens <- dens[dens$x > min(x) & dens$x < max(x),]

	polygon(x = c(dens$x, rev(dens$x)),
		y = c(dens$y, rep(0, length(dens$x))),
		col = this_color, border = F)
	
	if(mean) {
		if (!transparency) { segments(x0 = mean(x), x1 = mean(x),
			y0 = 0, y1 = kg(mean(x), x) - 0.004, lwd = 3, col = mean_color) }
		if (transparency) { segments(x0 = mean(x), x1 = mean(x),
			y0 = 0, y1 = kg(mean(x), x) - 0.004, lwd = 3, col = mean_color) }
		}
	
	segments(x0 = dens$x[1], x1 = dens$x[1],
		y0 = 0, y1 = dens$y[1], lty = 2)
	segments(x0 = dens$x[length(dens$x)], x1 = dens$x[length(dens$x)],
		y0 = 0, y1 = dens$y[length(dens$x)], lty = 2)
	points(dens$x, dens$y, lwd = 2, type = 'l', col = border_col)
	points(dens$x, dens$y, lwd = 2, type = 'l', col = border_col)
	}
