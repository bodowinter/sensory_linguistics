## Bodo Winter
## May 6, 2017
## Function for calculating adjusted standardized Pearson's residuals
## and Chi-Square tests with missing diagonals
## (code adapted from CrossTable() in package "gmodels"):

stdres_fnc <- function(M, na.pres = T) {
	RS <- rowSums(M, na.rm = T)
	CS <- colSums(M, na.rm = T)
	GT <- sum(M, na.rm = T)
	expected <- outer(RS, CS) / GT
	ASR <- (M - expected) / sqrt(expected * 
		((1 - RS/GT) %*% t(1 - CS/GT)))
	if(na.pres) diag(ASR) <- NA
	return(ASR)
	}

chisq.test_na <- function(M) {		# only works for tables that are larger than 2 X 2
	RS <- rowSums(M, na.rm = T)
	CS <- colSums(M, na.rm = T)
	GT <- sum(M, na.rm = T)
	expected <- outer(RS, CS) / GT
	xchisq <- sum(((M - expected) ^ 2) / expected, na.rm = T)
	mydf <- ((nrow(M) - 1) * (ncol(M) - 1)) - length(diag(M))
	myp <- pchisq(xchisq, df = mydf, lower.tail = F)
	mytext <- paste(paste0('chisq = ', round(xchisq, 2)), paste0('df = ', mydf), sep = ', ')
	mytext <- paste(mytext, paste0('p = ', myp), sep = ', ')
	cat(mytext)
	}

