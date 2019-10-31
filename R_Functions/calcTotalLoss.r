##########################################################################################  
##########################################################################################
## Supplement for   "forecastMiNLP.r"						            
## Functions for Use in Forecast
## Author:          Stefan Rameseder                                              

calcTotalLoss 	<- function(par, AL, y, y_max, T, bf = bf, goBack = 6){
	########## Input:
	# - par: parameters of model
	# - AL: Auction loss value
	# - y: a timeseries matrix
	# - y_max: the maximally accepted timeserives values
	# - T: length of time series
	# - bf: the bidding function depending on the parameters
	# - goBack: how many values should one go back
	########## Output:
	# - totLoss: the loss calculated as loss = acc_t(y_t^max - bf(par)) + (1-acc_t) AuctionLoss_t
	

	# 2. define functions
	mloss <- function(par, rho, y_mat, y_max){
	bids <- y_mat %*% par
	bools <- 
	return( rho*sum((y_max - bids)[bools]) + (1-rho)*sum(!bools) ) # p*f(x)+(1-p)*g(x)
	}

	
	bids	<- bf(par) 
	acc 	<- bids <= y_max
	loss	<- sum((y_max[-(1:(goBack+1))] - bf(par, y[-T, ], goBack))[acc]) + sum(!acc)* AL
	return(totLoss =  rho*sum((y_max - bids)[bools]) + (1-rho)*sum(!bools) )
}



