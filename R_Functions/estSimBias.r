
	
estSimBias <- function(simMeanBias_obj, nbasis_est, base_est){ # nbasis_est = nbasis_dgp
	nbasis_dgp		<- dim(simMeanBias_obj$scores)[1]-1
	comp_dom 		<- simMeanBias_obj$comp_dom 
	small_dom 		<- simMeanBias_obj$small_dom 
	basis_est		<- get(paste0("create.", base_est ,".basis"))(rangeval = range(comp_dom), nbasis = nbasis_est)
	X_cd_m 			<- simMeanBias_obj$X_cd_m # dim(X_cd_m) n x T
	X_sd_m 			<- simMeanBias_obj$X_sd_m
	n 				<- simMeanBias_obj$n
	T 				<- simMeanBias_obj$T # str(simMeanBias_obj, max = 1)
	scores_cd		<- simMeanBias_obj$scores_cd
	scores_sd		<- simMeanBias_obj$scores_sd
	
	Y_cd_est 		<- smooth.basis(argvals = comp_dom, y = X_cd_m, fdParobj = basis_est)
	Y_sd_est 		<- smooth.basis(argvals = small_dom, y = X_sd_m, fdParobj = basis_est)
	
	str(Y_cd_est, max = 2)
	
	Y_cd_est$fd$coefs
	
	plot(Y_sd_est$fd$coefs[1, ])
	points(scores_sd[1, ], col = "red")
	
	matplot(X_cd_m, x = comp_dom, type = "l")
	matplot(Y_cd_est$fd$coefs,type = "l", col = add.alpha("black", alpha =0.4))
	matplot(scores_cd,type = "l", col = add.alpha("blue", alpha =0.4), add = TRUE)
	
	
	Y_est$fd$coefs
}

