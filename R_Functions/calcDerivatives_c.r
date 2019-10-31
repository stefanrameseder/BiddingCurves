
calcDerivatives_c <- function(simMeanBias_c_obj, der = 1, main = paste0("Bspline with first derivatives"), X_true_mean = 0){
	# simMeanBias_obj = power
	Basis_der_ad 	<- eval.basis( evalarg = simMeanBias_c_obj$comp_dom, basisobj = simMeanBias_c_obj$basis, Lfdobj = der) 

	X_der_ad_m 		<- matrix(NA, nrow = simMeanBias_c_obj$n, ncol = simMeanBias_c_obj$T) # for each function as column there are nbasis elements for functions

	for( t in 1:simMeanBias_c_obj$T ){  
		# t-th column equals scores times basis as long as domain (comp_c) corresponds
		#print(t)
		# on which domain is the function living
		domain_i	<- which.max( simMeanBias_c_obj$comp_c[t] <= simMeanBias_c_obj$comp_dom )  # comp_dom[57] # theoretisch minus 1
		# Derivative of basis times the scorces from before
		X_der_ad_m[ 1:domain_i , t] <- Basis_der_ad[ 1:domain_i,] %*% simMeanBias_c_obj$scores_m[ ,t]
	}
	
	X_der_emp_mean			<- rowMeans(X_der_ad_m, na.rm = TRUE)
	
	return(list(X_der_ad_m = X_der_ad_m, simMeanBias_c_obj = simMeanBias_c_obj, X_der_emp_mean = X_der_emp_mean))
} 

