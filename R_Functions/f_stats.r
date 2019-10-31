f_stat1	<- function(X, d, k){
	# calculates the univariate F statistics from a multipe linear regression
	# d ~ a_0 + a_1 x_1 + ... + a_b x_b
    # if there are NA coefs
    sum <- summary(lm <- lm(d ~ 1 + ., data = data.frame(d, X)))
    if(dim(sum$coefficients)[1] != (k+1)){
        ret <- numeric(k)
        ret[which(is.na(coef(lm)))] <- 0
        ret[which(!is.na(coef(lm)))] <- sum$coefficients[2:(dim(sum$coefficients)[1]+1) , "t value"]^2
        return(ret)
    } else {
        return(sum$coefficients[2:(k+1) , "t value"]^2)
    }
}

# dim(summary(lm(d ~ 1 + ., data = data.frame(d, X)))$coefficients)
f_stat2_old	<- function(X, d, k){
    # calculates the univariate F statistics from a multipe linear regression
    # d ~ a_1 x_1 + ... + a_b x_b
    # dim(data.frame(d, X))
    sum <- summary(lm <- lm(d ~ -1 + ., data = data.frame(d, X)))
    # coefs(sum)
    return(sum$coefficients[1:k , "t value"]^2)
    
}



f_stat2	<- function(X, d, k){
	# calculates the univariate F statistics from a multipe linear regression
	# d ~ a_1 x_1 + ... + a_b x_b
    # dim(data.frame(d, X))
    sum <- summary(lm <- lm(d ~ 1 + ., data = data.frame(d, X)))
    # coefs(sum)
    # if there are NA coefs, sum responds only non NA and therefore the length of sum = k+1 is less!
    if(dim(sum$coefficients)[1] != (k+1)){
        ret <- numeric(k)
        ret[which(is.na(coef(lm)[-1]))] <- 0
        ret[which(!is.na(coef(lm)[-1]))] <- sum$coefficients[2:(dim(sum$coefficients)[1]) , "t value"]^2
        return(ret)
    } else {
        return(sum$coefficients[2:(k+1) , "t value"]^2)
    }
}

f_stat2_phigh	<- function(X, d, k){
    # calculates the univariate F statistics from a multipe linear regression
    # d ~ a_1 x_1 + ... + a_b x_b
    # dim(data.frame(d, X))
    sum <- summary(lm <- lm(d ~ -1 + ., data = data.frame(d, X)))
    # coefs(sum)
    return(sum$coefficients[1:k , "t value"]^2)
    
}

f_stat3	<- function(X, d, k){
	# calculates the univariate F statistics from a multipe linear regression
	# d ~ a_1 x_1 + ... + a_b x_b
	lm 			<- lm(d ~ 1 + ., data = data.frame(d, X))
	sq_coefs 	<- coeftest(lm, df = Inf, vcov = NeweyWest(lm, lag = 1, prewhite = FALSE))[2:(k+1) , "z value"]^2
	return(sq_coefs)
}


f_stat4	<- function(X, d, k){
	# calculates the univariate F statistics from a multipe linear regression
	# d ~ a_1 x_1 + ... + a_b x_b
	lm 			<- lm(d ~ 1 + ., data = data.frame(d, X))
	sq_coefs 	<- coeftest(lm, df = Inf, vcov = NeweyWest(lm, lag = 1, prewhite = TRUE))[2:(k+1) , "z value"]^2
	return(sq_coefs)
}

f_stat5	<- function(X, d, k){
	# calculates the univariate F statistics from a multipe linear regression
	# d ~ a_1 x_1 + ... + a_b x_b
	lm 			<- lm(d ~ 1 + ., data = data.frame(d, X))
	sq_coefs 	<- coeftest(lm, df = Inf, vcov = NeweyWest(lm, lag = 10, prewhite = FALSE))[2:(k+1) , "z value"]^2
	return(sq_coefs)
}
