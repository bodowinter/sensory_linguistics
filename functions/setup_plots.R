## Bodo Winter
## September 17, 2015

## Function for setting up empty plotting window:

setup_plots <- function (N = 1) {
	if ( N == 1 ) {
		quartz('', 8, 6)
		par(mai = c(1.15, 1.25, 0.75, 0.75))	
		}
	if ( N == 2 ) {
		quartz('', 11, 5)
		par(mfrow = c(1, 2), omi = c(1, 1.1, 0.85, 0.25), mai = c(0, 0.25, 0, 0))
		}
	if ( N == 3 ) {
		quartz('', 9, 8)
		par(omi = c(0.8, 1, 0, 0),
			mai = c(0.35, 0.25, 0.75, 0.5))
		layout(matrix(c(1,1,2,2,0,3,3,0), 2, 4, byrow=TRUE), respect=FALSE)		
		}
	if ( N == 5 ) {
		quartz('', 10, 7)
		par(omi = c(0.6, 1, 0, 0),
			mai = c(0.45, 0.15, 0.5, 0.25))
		layout(matrix(c(1,1,2,2,3,3,0,4,4,5,5,0), 2, 6, byrow=TRUE), respect=FALSE)		
		}
	}
