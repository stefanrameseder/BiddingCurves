modMINLP <- function(t, K, nPar, lossFunction, rho, yMAT, yMAX, low, up, maxit=1000, temp = 1000){	
	
	# Performs the Mixed Integer non linear programming task 

    # If estimation sample is all, only the first three steps are omitted
	if( K == "all"){
		K <- t-3
	}
	
	# par=NULL chooses starting values by its own
	solGenSA <- GenSA(  par=NULL,
            			fn = lossFunction, 
            			rho=rho, 
            			y_mat=yMAT[(t-K):(t-1), ], 
            			y_max=yMAX[(t-K+1):t], # cbind(y_mat[1:(t-1), ],y_max[2:t])
            			lower=low, upper=up,
            			control=list(maxit=maxit, smooth=FALSE, verbose=FALSE, temperature=temp))

	optPar 		<- solGenSA$par
	forecast 	<- yMAT[t, ] %*% optPar # y[(t+1), ] # forecast for t+1
	return(c(fc = forecast, acc = (forecast<=yMAX[t+1]), val = solGenSA$value, optPar = optPar))
}