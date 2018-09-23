## Bodo Winter
## August 24, 2018

cosine_sim <- function(x, y) {
	numerator <- x %*% y
	denominator <- sqrt(x %*% x * y %*% y)
	return(as.vector(numerator/denominator))
	}

cosine_sim_fast <- function(x, y) {
	(x %*% y) / (sqrt(x %*% x * y %*% y))
	}