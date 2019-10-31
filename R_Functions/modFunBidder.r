
# modFunBidder <- function(y, H, threshold = 0.02){ # y <- data[[1]][[2]][,6]; H =1; plot(c(y, fc), type = "l"); points(length(y), fc, col = "red", pch =16)  
	# goBack		<- attributes(bf)[[1]]
	# nPar		<- attributes(bf)[[2]]
	# T 			<- dim(y)[1]
	# y_max 		<- y[ ,"MaximumCapacityPrice"]
	# names(y_max)<- y[, "Date"]
	
	# depends on the bid_function: how many lags are included to estimate T; usually at least 8
	 		
	# par 		<- runif(nPar, -1, 1)
	# low			<- rep(-5, times = nPar)
	# up			<- rep(5, times = nPar)
	
	# total_loss(par, AL, y, y_max, z_weights, T, bf = bf, goBack = 6)
	# system.time(sol<- GenSA(	par=par,
								# fn = total_loss,  AL = AL, y = y, y_max = y_max,
								# T = T, bf = bf, goBack = goBack,
								# lower=low, upper=up,
								# control=list(maxit=maxit, verbose = FALSE , temperature = 500)))
  
  

  # add_y <- matrix(as.matrix(y[T, 2:4]), nrow = H, ncol = (dim(y)[2]-1), byrow = TRUE)
  # y_fc  <- rbind(as.matrix(y[, 2:4]), add_y)
  # fc <- bf(par = sol$par, y = y_fc, goBack = goBack)
  # fc[fc < threshold] <- 0
	
  # plot(c(y[, "AverageCapacityPrice"], rep(NA, times = H)), type = "l")
	# lines(c(rep(NA, times = goBack), fc), col = "red")
  # length(fc)
	# return(list(model = sol$par, forecast =  list(pred = fc[(T+1-goBack):(T-goBack+H)], se = fc)))
# }


modGenSaBidder <-function(t, dates, bdQuan, bdMax, bdMW, bdDem, 
                     biddata, bidcurves, bidcurves_smoothed,
                     H = 1, K = "all"){
    # 
    if(K == "all"){
        K <- t-1
    }
    
	 
	y_t1	
	
	
	
	fc <- K
    names(fc) <- dates[t+1]
    return(list(fc = fc 
                #,estData = estData
    ))
}



