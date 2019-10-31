# simDataSC <- function(n, xi_1_mean, ev, suppliedSettings){
	#Simultes truncated multivariate normal ev <- 10
	#with dependency

	#default settings
    # settings <- list(sd_part = 1, upper = c(Inf, 1), lower = c(-Inf, .5), xi_1_d_cor = 0.8, d_mean = 0.75, d_var = 1)       
    
	# matching 				<- intersect(names(settings), names(suppliedSettings))
	# settings[matching] 	    <- suppliedSettings[matching]

	# means 		<- c(xi_1_mean, settings$d_mean)
	# cor_sigma   <- matrix(c(1, settings$xi_1_d_cor, settings$xi_1_d_cor, 1), ncol = 2)
	# cov_sigma 	<- cor2cov(cor_sigma, sd = c(sqrt(ev),sqrt(settings$d_var)))
	#?rtmvnorm
	# x 			<- rtmvnorm( n = n, mean = means, sigma = cov_sigma, upper = settings$upper, lower = settings$lower)

	# colnames(x) <- c("x", "d") # cor(x) cov2cor(sigma_dec)
	# return(sim_data 	= x) # plot(sim_data); plot(sim_data[, "d"]); sd(sim_data[, "d"])
# }




# set.seed(10)
# rhos2 <- sapply(1:1000, function(r) {
    # sim_data <- simDataSC(n = 1000, xi_1_mean = 5 , ev = seq.ev[1], suppliedSettings)
    # return(cor(sim_data)[1,2])})
# print("SC:")
# summary(rhos2)


# suppliedSettings$d_var 		<- 100
# set.seed(10)
# rhos2 <- sapply(1:100, function(r) {
    # sim_data <- simDataIC(n = 1000, xi_1_mean = 5 , ev = seq.ev[1], suppliedSettings)
    # return(var(sim_data[, "d"]))}) # plot(sim_data[, "d"]); sd(sim_data[, "d"])
# print("SC:")
# summary(rhos2)

scaleDomain <- function(x, a=0.5, b=1){
	# Transforms an arbitrary intervall to [a,b]
	((b-a)*(x - min(x)))/(max(x) - min(x)) + a
}

scaleDomain2 <- function(x, a=0.5, b=1){ # x <- pnorm(scores1, mean, mean = xi_1_mean, sd = sqrt(ev) ); plot(x); hist(x)
	# Cuts a [0,1] into [0.5, 1] by transforming all values bigger than 0.5 uniformly to [0.5,1] and taking all values smaller than 0.5 to 0.5 and 
	x[which(x >= a) ] <- (((b-a)*(x - min(x)))/(max(x) - min(x)) + a)[which(x > a) ]
	x[which(x < a) ] <- a
	return(x) # plot(x)
 }

 
scaleDomain3 <- function(x, a=0.5, b=1, positions){ # x <- pnorm(scores1, mean, mean = xi_1_mean, sd = sqrt(ev) ); plot(x); positions <- c(0.625, 0.75, 0.875, 1)
	# Cuts a [0,1] into [0.5, 1] by transforming all values bigger than 0.5 uniformly to [0.5,1] and taking all values smaller than 0.5 to 0.5 and 
	for(i in 1:(length(positions)-1)){
		x[which(x > rev(positions)[1+i] && x <= rev(positions)[i])] <- rev(positions)[i]
	}
	x[which(x > a && x <= rev(positions)[(legnth(positions)-1)])] <- positions[1]
	x[which(x <= a)] <- a
	return(x)
 }
 
# simDataSC <- function(n, xi_1_mean, ev, suppliedSettings){ # xi_1_mean = 5; n = 100
#Simulates with dependency via transformation scaleDomain
    
    #default settings
    # settings <- list(sigma_sc = 3)       
    	
    # if(hasArg(settings)){
          # suppliedSettings 		<- list(...)$settings
    	  # print(suppliedSettings)
          # matching 				<- intersect(names(settings), names(suppliedSettings))
          # settings[matching] 	<- suppliedSettings[matching]
    	# }
        # scores1 <- rnorm(n, mean = xi_1_mean, sd = sqrt(ev))
        # d 		<- scaleDomain( x = 0.75 * scores1 + rnorm(n, sd=sqrt(suppliedSettings$sigma_sc)) ) # 33.5 C: 

        # x       <- data.frame(scores1, d)
        # colnames(x) <- c("x", "d") # cor(x) cov2cor(sigma_dec)
	# return(sim_data 	= x) # plot(sim_data)
# }


simDataSC <- function(n, xi_1_mean, ev, suppliedSettings){ # xi_1_mean = 5; n = 1000
# Simulates dependency as standard transformation:
# Let X ~ f_X, then F(X) ~ UNIF[0,1]
# Where UNIF[0,1] will be transformed to UNIF(0.5, 1) and half of them to be 0.5
    
    # default settings
    settings <- list(sigma_sc = 3)       
    # 	
    if(hasArg(settings)){
          suppliedSettings 		<- list(...)$settings
    	  #print(suppliedSettings)
          matching 				<- intersect(names(settings), names(suppliedSettings))
          settings[matching] 	<- suppliedSettings[matching]
    }
	scores1 <- rnorm(n, mean = xi_1_mean, sd = sqrt(ev))
	d 		<- scaleDomain2(x = pnorm(scores1, mean, mean = xi_1_mean, sd = sqrt(ev) ))
	# hist(d)
	#d 		<- scaleDomain( x = 0.75 * scores1 + rnorm(n, sd=sqrt(suppliedSettings$sigma_sc)) ) # 33.5 C: 

	x       <- data.frame(scores1, d)
	colnames(x) <- c("x", "d") # cor(x) cov2cor(sigma_dec)
	return(sim_data 	= x) # plot(d)
}


# set.seed(10)
# rhos2 <- sapply(1:1000, function(r) {
    # sim_data <- simDataSC(n = 1000, xi_1_mean = 5 , ev = seq.ev[1], suppliedSettings)
    # return(cor(sim_data)[1,2])})
# print("SC:")
# summary(rhos2) # plot(sim_data <- simDataSC(n = 1000, xi_1_mean = 5 , ev = seq.ev[1], suppliedSettings))

# suppliedSettings$d_var 		<- 100
# set.seed(10)
# rhos <- sapply(1:100, function(r) {
    # sim_data <- simDataSC(n = 500, xi_1_mean = 5 , ev = seq.ev[1], suppliedSettings)
    # return(sd(sim_data[, "d"]))}) # plot(sim_data[, "d"]); sd(sim_data[, "d"]); hist(sim_data[, "d"])
# print("SC:")
# summary(rhos)

# simDataSC <- function(n, xi_1_mean, ev, suppliedSettings){ # xi_1_mean = 5; n = 1000
# Simulates dependency as standard transformation:
# Let X ~ f_X, then F(X) ~ UNIF[0,1]
# Where UNIF[0,1] will be transformed to MULTINOM{0.5, 0.625, 0.75, 0.875, 1} and half of them to be 0.5
    
    #default settings
    # settings <- list(sigma_sc = 3)       
    	
    # if(hasArg(settings)){
          # suppliedSettings 		<- list(...)$settings
    	  #print(suppliedSettings)
          # matching 				<- intersect(names(settings), names(suppliedSettings))
          # settings[matching] 	<- suppliedSettings[matching]
    # }
	# scores1 <- rnorm(n, mean = xi_1_mean, sd = sqrt(ev))
	# d 		<- scaleDomain3(pnorm(scores1, mean, mean = xi_1_mean, sd = sqrt(ev) ), settings$positions)
	#hist(d)
	#d 		<- scaleDomain( x = 0.75 * scores1 + rnorm(n, sd=sqrt(suppliedSettings$sigma_sc)) ) # 33.5 C: 

	# x       <- data.frame(scores1, d)
	# colnames(x) <- c("x", "d") # cor(x) cov2cor(sigma_dec)
	# return(sim_data 	= x) # plot(d)
# }

