predictSmalltoBigDomain <- function(cov_train_m,  discretization_large_domain, fitted_object, fitted_mean, observed_short_domain, 
									prediction_of_fitted_object = TRUE, nderiv = 0, n_pc = 2, cov_small_train_m = FALSE, 
									substitute_sm_cov_into_large = FALSE){
	## ###########################################################################
	## Predict functions observed on small domains with given covariance matrix on large domain to large domain                     
	## ###########################################################################
	## Input:
	# - cov_train_m: (X.train.mat %*% t(X.train.mat))/T.train; Covariance matrix of trained data on large domain
	# - discretization_large_domain: Complete domain for which curves should be predicted
	# - observed_short_domain: Domain on which curves were observed
	# - fitted_object: fitted object which should be predicted for the observed values
	# - nderiv = 0: which derivative should be predicted of our fitted object
	# - n_pc = 3: number of principal components which should be used
	## Output:
	# - List of Recovered values on discretization_large_domain for different purposes
	
	if( !any(substitute_sm_cov_into_large, is.matrix(cov_small_train_m)) ){
		stop("If you want to substitute your Training Covariance Matrix, you should provide a Matrix or set it to TRUE")
	}

	## Number of discretization points; Indices for observed and unobserved values
	p                		<- length(discretization_large_domain) 		# Number of discretization points
	
	## NA Points for the pediction points
	n_pred_points			<- p - length(observed_short_domain) 
	
	if (prediction_of_fitted_object){ # if fitted_object can be predicted; in case of GCV
	
		x_short_v				<- c(predict(fitted_object, xarg = observed_short_domain, nderiv = nderiv), rep(NA, times = n_pred_points))
		x_short_demeaned_v 		<- c(	predict(fitted_object, xarg = observed_short_domain, nderiv = nderiv) - 
										predict(fitted_mean, newdata = observed_short_domain),
										rep(NA, times = n_pred_points))
	} else { # if fitted_object cannot be predicted (in case of monPSpline)
		x_short_demeaned_v		<- fitted_object - c(predict(fitted_mean, newdata = observed_short_domain), rep(NA, times = n_pred_points))			
		x_short_v				<- x_short_demeaned_v
	}
	
	non_observed        	<- is.na(x_short_v) 	# indices of outer discretization points
	observed				<- 1:length(observed_short_domain)
	
	if( length(x_short_v) != length(discretization_large_domain) ) {
		stop("Wrong lengths of prediction vectors")
	}  
	
	## cov_small_train_m = FALSE; no susbstitution; else: substitute inner covariance with big sample and outer with training functions
	if(is.matrix(cov_small_train_m)){		
		# indices of inner covariance matrix:
		obs 					<- 1:dim(cov_small_train_m)[1]
		cov_train_m[obs, obs]	<- cov_small_train_m
	}
	
	cov_train_svd 			<- eigen(cov_train_m)
	eval_train_v	    	<- cov_train_svd[[1]]
	efun_train_m    		<- cov_train_svd[[2]]

	scores_train_lm_v     	<- c( lm(x_short_v  			~ efun_train_m[, 1: n_pc])$coef[-1] )
	scores_train_lm_dm_v   	<- c( lm(x_short_demeaned_v  	~ efun_train_m[, 1: n_pc])$coef[-1] )
	
	x_large_recovered_lm_v		<- efun_train_m[, 1: n_pc] %*% scores_train_lm_v  
	x_large_recovered_lm_dm_v	<- efun_train_m[, 1: n_pc] %*% scores_train_lm_dm_v

	## ###########################################################################
	## Compare with rebuilt (cf. graphs) n_pc = 5
	## 	n_pc = 3
	# efun_small_m			<- eigen(cov_small_train_m)$vectors
	# eval_small_v			<- eigen(cov_small_train_m)$values
	# var_shares   			<- (eval_small_v[eval_small_v>0])/sum(eval_small_v[eval_small_v>0])
	# scores_small_v 			<- lm(x_short_demeaned_v[1:dim(cov_small_train_m)[1]]  	~ efun_small_m[, 1: n_pc])$coef[-1]
	# createScreePlot(cumsum(round(var_shares[1:10],digits=4)*100))
	# summary(lm(x_short_demeaned_v[1:dim(cov_small_train_m)[1]]  	~ efun_small_m[, 1: n_pc]))
	# summary(lm(x_short_demeaned_v  									~ efun_train_m[, 1: n_pc]))
	
	# cov_train_m[obs, obs] == cov_small_train_m
	
	# x_small_recovered_lm_v	<- efun_small_m[, 1: n_pc] %*% scores_small_v
	
	# pdf(paste0("graphs/", format(Sys.time(), format="%Y%m%d"), "ext_vs_orig_vs_rebuilt.pdf"),width = 10, height = 10) 
	# plot(	x = observed_short_domain, y = x_short_demeaned_v[1:length(observed_short_domain)], type ="l", 
			# main = paste0("Date: ", dates[t], " with Index: ", t),
			# xlim = c(0, max(discretization_large_domain)), col ="red")
	# abline(h= 0, col= "green")
	# abline( v = range(u_m[,t][ !is.na( u_m[,t]) ]), col = "yellow")
	# abline( v = range(com_dom_discretization), col = "orange")
	# lines( 	x = discretization_large_domain, y = x_large_recovered_lm_dm_v, col ="green")
	# lines(  x = observed_short_domain[1:dim(cov_small_train_m)[1]], y=x_small_recovered_lm_v, col ="blue")
	# legend(	"topright", col = c("red", "green", "blue"), lty = "solid",
			# legend = c("Original", "Extended", "Rebuilt with 3 PCs"), lwd=1.125)
	# grid(col = "black")
	# dev.off()
	
	
	# n_eigenfunctions 		<- 6
	# pdf(paste0("graphs/", format(Sys.time(), format="%Y%m%d"), "ef_of_ci_covariance.pdf"), height = 20, width = 15)
	# par(mfrow=c(n_eigenfunctions/2,2))
	# for(ef in 1:n_eigenfunctions){
	# plot(	x = observed_short_domain[1:dim(cov_small_train_m)[1]],
			# y = efun_small_m[, ef ],
			# col = "red", type = "l", main = paste0("Eigenfunction ", ef ,"\n", round(var_shares[ ef ]*100, digits=4),"%"),
			# ylab = "EUR/MWh", xlab = "Adjusted Demand (MW)")
	# }
	# dev.off()
	## ###########################################################################
	
	return(list( 	"not_dm" = x_large_recovered_lm_v, 
					"demeaned" = x_large_recovered_lm_dm_v,
					"scores_train_lm_dm_v" = scores_train_lm_dm_v))
}