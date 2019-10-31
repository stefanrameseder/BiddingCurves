
# Define Loss Function
lossFunction <- function(par, rho, y_mat, y_max){
	# Input: 
	# -par: vector of length k
	# -rho: scalar reciproke of auction loss
	# -y_mat: t x k matrix of time series
	# -y_max: t x 1 
	# Attention: y_max contains the one point in the future of y_mat
	# Output:
	# -the Auction loss defined as 
    # sum{ [max_t - b_t(data up to t-1)]*acc_t + AL * (1-acc_t)} 
	bids 		<- y_mat %*% par # 
	bools 		<- bids <= y_max # which bids were smaller or equal the maximum 
	return(al =  rho*sum((y_max - bids)[bools]) + (1-rho)*sum(!bools) ) # p*f(x)+(1-p)*g(x)
}

