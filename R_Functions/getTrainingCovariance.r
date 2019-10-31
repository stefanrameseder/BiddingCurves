

# save(t, cov_train_m, cov_train_m, dates,  discretization_large_domain, fitted_object, fitted_mean, observed_short_domain, 
	# prediction_of_fitted_object, nderiv, n_pc, cov_small_train_m, substitute_sm_cov_into_large, u_m, com_dom_discretization, 
	# file = "rebuild_problems.RData")
	
	

getTrainingCovariance <- function(training_index, fitted_objects, fitted_mean, discretization){
	## ###########################################################################
	## getTraining Covariane Matrix given some fitted objects on some discretizied intervall                     
	## ###########################################################################
	## Input:
	# - training_index: vector of indizes for functions to be used
	# - fitted_objects: a list of fitted objects which should be used with predict
	# - fitted_mean: for centralizing a fitted mean object
	# - discretization
	## Output:
	# - Corresponding empirical covariance matrix
	
	X_prime_train_m			<- matrix( NA , nrow = length(discretization), ncol = length(training_index))

	counter <- 1
	for(t in training_index){ 
		#print(t)
		X_prime_train_m[, counter] 			<- as.vector(predict(fitted_objects[[t]], xarg = discretization, nderiv = 1)) - predict(fitted_mean, newdata = discretization)
		counter 							<- counter + 1
	}
	
	cov_train_m 			<- ( X_prime_train_m %*% t(X_prime_train_m) ) / length(training_index)

	return(cov_train_m)
}