


calcVarTN <- function(mu = 0.75, sigma_sq = 1, a = 0.5, b = 1){
		alpha 		<- (a - mu)/sqrt(sigma_sq)
		beta 		<- (b - mu)/sqrt(sigma_sq)
		Z 			<- pnorm(beta) - pnorm(alpha)
		sigma_sq_TN <- sigma_sq * (1  +  ((alpha * dnorm(alpha) - beta * dnorm(beta))/Z) +  ((dnorm(alpha) - dnorm(beta))/Z)^2 )
		return(sigma_sq_TN=sigma_sq_TN)
}


# calcVarTN(mu = 0.75, sigma_sq = 1, a = 0.5, b = 1) 
# [1] 0.02066024


# calcVarTN(mu = 0.75, sigma_sq = 10, a = 0.5, b = 1) 
# [1] 0.02081598


# calcVarTN(mu = 0.75, sigma_sq = 100, a = 0.5, b = 1) 
# [1] 0.0208316
