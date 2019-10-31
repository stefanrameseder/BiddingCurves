simDataSD <- function(n, xi_1_mean, ev, suppliedSettings){
	# Simulates n skewed n Random variables set.seed(1)
	
	# default settings
    settings <- list(sd_part = 1, upper = c(Inf, 1), lower = c(-Inf, .5), xi_1_d_cor = 0.8, d_mean = 0.75, d_var = 1)       
    
    matching 				<- intersect(names(settings), names(suppliedSettings))
    settings[matching] 	    <- suppliedSettings[matching]
    
	x_fin       <- rnorm(n, mean = xi_1_mean, sd =sqrt(ev))
  
	part        <- settings$sd_part 
	#print(part)
	d           <- as.numeric( (x_fin-xi_1_mean)[1:round(part*n ,0)] > 0)*0.5+ 0.5
	d_otherpart <- sample(x=c(0.5,1), size = n-round(part*n,0), replace = TRUE)
	
	sim_data    <- cbind(x_fin, c(d,d_otherpart)) 
	colnames(sim_data) = c("x", "d")
	
	return(sim_data 	= sim_data) # plot(sim_data); cor(sim_data)[1,2]
}



# set.seed(10)
# rhos2 <- sapply(1:1000, function(r) {
    # sim_data <- simDataSD(n = 100, xi_1_mean = 5 , ev = seq.ev[1], suppliedSettings = list(sd_part = 1))
    # return(cor(sim_data)[1,2])})
# print("SD:")
# summary(rhos2)
