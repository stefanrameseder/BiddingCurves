
calcEstimationErrors <- function(comp_dom, small_dom, pooled_mean, int_der_mean, true_mean, n){
	n_sd	 	<- length(small_dom)
	pooled_ssr	<- (pooled_mean-true_mean)^2
	intder_ssr	<- (int_der_mean-true_mean)^2
	
	return(list(	cd_pooled_error = sum(pooled_ssr[1:n_sd]),  
					cd_ftc_error = sum(intder_ssr[1:n_sd]), 
					ncd_pooled_error = sum(pooled_ssr[(n_sd+1):n]), 
					ncd_ftc_error = sum(intder_ssr[(n_sd+1):n])))
}
