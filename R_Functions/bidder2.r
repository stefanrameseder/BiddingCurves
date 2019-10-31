
## Bid Functions:
## Each bid function is basically a weighted sum of past values for the future bid
## Each bid function needs two attributes
# 1. How many steps it goes back
# 2. How many parameters are needed

	
bidder2 <- function(par = runif(7, -1, 1), y, demand, goBack = 8){ #bf <- bid_function1
	# The bid function devlivers for a parameter vector par and data y_t, t = 1, ..., T 
	# a bid for T+1
	# Input: 
	# - par (depending on the parameters in the bid function)
	# - y (a matrix of time series)
	# - demand (the demand at each auction)
	# Output: 
	# - bid a vector of length T - goBack + 1
	# bf_T(par): y_t = par1 y_t-1^max + par2 y_t-1^wavg + par3 y_t-2^wavg + par4 y_t-6^max + par5 y_t-6^wavg + par4 y_t-7^max + par5 y_t-7^wavg 
	T			<- dim(y)[1] # dim(y_t_1)
	y_t_1		<- cbind(y[-(1:goBack) , c("MaximumCapacityPrice", "AverageCapacityPrice")]) # dim(y_t_1)
	y_t_2		<- cbind(y[c(-(1:(goBack-1)), -T), c("AverageCapacityPrice")])# dim(y_t_2)
	y_t_7		<- cbind(y[-c(1, (T-goBack+2):T), c("MaximumCapacityPrice", "AverageCapacityPrice")]) # dim(y_t_7)
	y_t_8		<- cbind(y[-((T-goBack+1):T), c("MaximumCapacityPrice", "AverageCapacityPrice")]) # dim(y_t_8)
	Y_mat		<- as.matrix(cbind(y_t_1, y_t_2,y_t_7, y_t_8))
  class(Y_mat)
	class(as.matrix(par)) 
  class(Y_mat)
  class(par[1])
  return(bid = as.matrix(Y_mat) %*% as.matrix(par)) #dim(Y_mat), length(par), class(Y_mat), par <- sol$par
}



# Additionally: How many lags do we need:
attributes(bidder2) <- list(goBack = 8, nPar = 7)	
