## March 7, 2016
## Rough draft of a prediction function for generalized linear MIXED models
## - takes the model as first argument ('fit'),
## - a dataframe with values to get predictions for ('newdata')
## - an argument specifying the type of GLM (gaussian/logistic/poisson)

predict.glmm <- function(fit, newdata, type = 'gaussian') {
		
	## Extract DV:	
	
	DV = strsplit(as.character(fit@call), ' ~ ')[[2]][1]
	
	## Append empty DV column:
	
	newdata = cbind(newdata, rep(0, nrow(newdata)))
	colnames(newdata)[ncol(newdata)] = DV
	
	## Extract model matrix:
	
    mm <- model.matrix(terms(fit), newdata)
    
    ## Add fitted vals:
    
    newdata[, DV] <- predict(fit, newdata, re.form = NA)
    
    ## Extract confidence intervals:
    
    pvar1 <- diag(mm %*% tcrossprod(vcov(fit), mm))
    newdata$UB <- newdata[, DV] + 1.96 * sqrt(pvar1)
    newdata$LB <- newdata[, DV] - 1.96 * sqrt(pvar1)
    
    ## Transform if working with a non-gaussian GLMM:
    
    if (type == 'poisson') {
    	newdata$UB <- exp(newdata$UB)
    	newdata$LB <- exp(newdata$LB)
    	newdata[,DV] <- exp(newdata[,DV])
    	}

    if (type == 'binomial') {
    	newdata$UB <- plogis(newdata$UB)
    	newdata$LB <- plogis(newdata$LB)
    	newdata[,DV] <- plogis(newdata[,DV])
    	}
    
    return(newdata)
    }