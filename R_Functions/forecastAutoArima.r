						
forecastAutoArima <- function(ind, pmhtnt, 
							xreg_ver = "x1", 
							dates,
							biddata,
							ver = "v1",
							md_dis){ # ind <- T-1
	
	# This function estimates the conditional functional mean one step ahead and evaluates it at the mw PosPerc
	
	if(ver == "v1"){
		# Get statistic up to ind; v1 = gavg  
		statistic 	<- biddata[[pmhtnt]]$quantiles$gavg[1:ind] # length(gavg)
		
	} else if (ver == "v2"){
		# Get statistic up to ind; v2 = min  
		statistic 	<- biddata[[pmhtnt]]$quantiles$min[1:ind] # length(gavg)
		
	} else if (ver == "v3"){
		# Get statistic up to ind; v3 = max  
		statistic 	<- biddata[[pmhtnt]]$quantiles$max[1:ind] # length(gavg)
		
	} else if (ver == "v4"){
		# Get statistic up to ind; v4 = efg  
		statistic 	<- biddata[[pmhtnt]]$quantiles$efg[1:ind] # length(gavg)
		
	} else if (ver == "v5"){
		# Get statistic up to ind; v5 = efg  
		statistic 	<- biddata[[pmhtnt]]$quantiles$ezgw[1:ind] # length(gavg)
		
	} else if (ver == "v6"){
		# Get statistic up to ind; v5 = efg  
		statistic 	<- biddata[[pmhtnt]]$quantiles$median[1:ind] # length(gavg)
		
	} else {
		stop("Problem with versions")
	}
	
	# Regressor combinations 
	if(xreg_ver == "x1"){
		# max mw
		xreg <-	cbind(biddata[[pmhtnt]]$maxMW)
	} else if (xreg_ver == "x2"){
		# max mw + christmas 
		xreg <-	cbind(biddata[[pmhtnt]]$quantiles$christmas,biddata[[pmhtnt]]$maxMW)
	} else if (xreg_ver == "x3"){
		# max + min 
		xreg <-	cbind(biddata[[pmhtnt]]$quantiles$max,biddata[[pmhtnt]]$quantiles$min)
	} else if (xreg_ver == "x4"){
		# spread + max 
		xreg <-	cbind(biddata[[pmhtnt]]$quantiles$spread ,biddata[[pmhtnt]]$quantiles$max)
	} else if (xreg_ver == "x5"){
		# spread + christmas 
		xreg <-	cbind(biddata[[pmhtnt]]$quantiles$spread , biddata[[pmhtnt]]$quantiles$christmas)
	} else if (xreg_ver == "x6"){
		# almost everything  
		xreg <-	cbind(biddata[[pmhtnt]]$quantiles$max, biddata[[pmhtnt]]$quantiles$spread , biddata[[pmhtnt]]$quantiles$christmas, biddata[[pmhtnt]]$maxMW, 
						biddata[[pmhtnt]]$holidays[ ,2])
	} else if (xreg_ver == "x7"){
		# almost everything  
		xreg <-	cbind(biddata[[pmhtnt]]$quantiles$median, biddata[[pmhtnt]]$maxMW)
	}else {
		
		stop("Problem with reg versions")
	}	
	regressors 	<- xreg[1:ind, ]
		
	# Predict statistic 
	tryCatch( 	{statistic_fit 	<- auto.arima(statistic, max.p = 2 , max.q = 2, max.d = 1,  xreg = regressors, 
												ic = "aicc", allowdrift = TRUE, allowmean = TRUE)
				statistic_fc 	<- forecast(statistic_fit, xreg = matrix(xreg[(ind+1), ], nrow = 1))$mean}, 
				error = function( err ) return(statistic_fc <<- statistic[ind]),
				warning = function( warn ) return(statistic_fc <<- statistic[ind]))

	if(statistic_fc < 0) statistic_fc <- 0
	
	zuschlag 			<- as.numeric(statistic_fc <= biddata[[pmhtnt]]$quantiles$max[ind+1])
	
	# ind runs through forecastInd
	# These are the indices t for which we do forecasts t+1|t and compare them with real values in t+1
	# Name the list entry according to t+1 
	return(c(	fc = statistic_fc, 
				acc = zuschlag, 
				max = biddata[[pmhtnt]]$quantiles$max[ind+1]
				))
}


