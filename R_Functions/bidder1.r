	
bidder1 <- function(par = runif(5, -1, 1), y, demand, goBack = 6){ #bf <- bid_function1
	# The bid function devlivers for a parameter vector par and data y_t, t = 1, ..., T 
	# a bid for T+1
	# Input: 
	# - par (depending on the parameters in the bid function)
	# - y (a matrix of time series)
	# - demand (the demand at each auction)
	# Output: 
	# - bid a vector of length T - goBack + 1
	# bf_T(par) = par1 y_t-1^max + par2 y_t-1^wavg + par3 y_t-2^wavg + par4 y_t-8^max + par5 y_t-8^wavg 
	T			<- dim(y)[1]
	#y_t_1		<- cbind(y[-(1:goBack) , c("Date", "MaximumCapacityPrice", "AverageCapacityPrice")]) # dim(y_t_1)
	#y_t_2		<- cbind(y[c(-(1:goBack)+1, -T), c("Date","AverageCapacityPrice")])# dim(y_t_2)
	#y_t_8		<- cbind(y[-((T-goBack+1):T), c("Date","MaximumCapacityPrice", "AverageCapacityPrice")]) # dim(y_t_8)
	y_t_1		<- cbind(y[-(1:goBack) , c("MaximumCapacityPrice", "AverageCapacityPrice")]) # dim(y_t_1)
	y_t_2		<- cbind(y[c(-(1:goBack)+1, -T), c("AverageCapacityPrice")])# dim(y_t_2)
	y_t_8		<- cbind(y[-((T-goBack+1):T), c("MaximumCapacityPrice", "AverageCapacityPrice")]) # dim(y_t_8)
	Y_mat		<- as.matrix(cbind(y_t_1, y_t_2, y_t_8))
	return(bid = Y_mat %*% par) #dim(Y_mat), length(par), class(Y_mat), par <- sol$par
}

# Additionally: How many lags do we need:
attributes(bidder1) <- list(goBack = 6, nPar = 5)	


