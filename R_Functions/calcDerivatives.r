
calcDerivatives <- function(simMeanBias_obj, der = 1, main = paste0("Bspline with first derivatives"), X_true_mean = 0){
	# Calculates the derivatives for a simMeanBias_obj
	
	Basis_der_sd 			<- eval.basis( evalarg = simMeanBias_obj$small_dom, basisobj = simMeanBias_obj$basis, Lfdobj = der) 

	X_der_cd				<- Basis_der_cd %*% simMeanBias_obj$scores_cd
	X_der_sd				<- Basis_der_sd %*% simMeanBias_obj$scores_sd
	
	X_der_emp_mcbind		<- cbind(X_der_cd, rbind(X_der_sd, matrix(NA, nrow = length(simMeanBias_obj$comp_dom) - length(simMeanBias_obj$small_dom), ncol = dim(X_der_sd)[2])))
	X_der_emp_mean			<- rowMeans(X_der_emp_mcbind, na.rm = TRUE)
	
	y1_mean 				<- round(mean(rowMeans(X_der_sd)), 2)
	y0_mean					<- round(mean(rowMeans(X_der_cd)), 2)
	dist					<- round(sum(X_der_emp_mean^2), 2)
	
	return(list(X_der_cd = X_der_cd, X_der_sd = X_der_sd, simMeanBias_obj = simMeanBias_obj, y1_mean = y1_mean, y0_mean = y0_mean, dist = dist, 
				X_der_emp_m = X_der_emp_mcbind, X_der_emp_mean = X_der_emp_mean, X_true_mean = X_true_mean))
} 

