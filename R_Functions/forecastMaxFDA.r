						
forecastMaxFDA <- function(ind, pmhtnt, 
							xreg_ver = "x1", 
							mwPosPerc = 0.9,
							dates,
							biddata,
							ver = "v1",
							ftc_Estimates_pmhtnt,
							md_dis){ # ind <- T-1
	
	# This function estimates the conditional functional mean one step ahead and evaluates it at the mw PosPerc
	# v3.x6.0.44
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
		# Get statistic up to ind; v6 = median  
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
	} else {
		
		stop("Problem with reg versions")
	}	
	regressors 	<- xreg[1:ind, ]
	
	
	# Get the Mean estimated with data up to ind
	T 			<- length(dates) 
	id 			<- ind - (T-100) + 1
	ftcMean 	<- ftc_Estimates_pmhtnt[[id]]$ftcMean
	
	# plot(md_dis, ftcMean, type = "l")
	
	# Predict statistic 
	tryCatch( 	{statistic_fit 	<- auto.arima(statistic, max.p = 2 , max.q = 2, max.d = 1,  xreg = regressors, 
												ic = "aicc", allowdrift = TRUE, allowmean = TRUE)
				statistic_fc 	<- forecast(statistic_fit, xreg = matrix(xreg[(ind+1), ], nrow = 1))$mean}, 
				error = function( err ) return(statistic_fc <<- statistic[ind]))
	
	
	# plot(biddata[[pmhtnt]]$quantiles$gavg, type = "l", ylim = c(0,100))
	# points(x= (ind+1), y = statistic_fc, col = "red", pch = 16, cex = 2)
	# lines(biddata[[pmhtnt]]$quantiles$max, col = "blue", lwd = 2)
	# abline(h= 0, lwd = 2)
	
	
	# Calculate the difference between the angle point and the mean of the functional curve on the same domain in ind 
	maxMW 				<- biddata[[pmhtnt]]$maxMW[ind]
	
	ftcMean_Index 		<- which.min(md_dis < maxMW) # md_dis[ftcMean_Index]
	
	statistc_ftcMean 	<- mean(ftcMean[1:ftcMean_Index])
	
	ftcMean_fc 			<- as.numeric(statistic_fc - statistc_ftcMean) + ftcMean# if ftcMean > fc -> 
	
	#plot(x = md_dis, y = ftcMean_fc, type = "l", lwd = 3)
	#lines(x = md_dis, y = bidcurves_smoothed[[pmhtnt]][[ind+1]]$constrained.curve(md_dis), col = "blue")
	
	mwPos 				<- biddata[[pmhtnt]]$maxMW[ind+1] * mwPosPerc
	
	fc_ftcMean_Index 	<- which.min(md_dis < mwPos) 
	fc_FDA 				<- ftcMean_fc[fc_ftcMean_Index]
	
	# If forecast is negative, bring it positive 
	if(fc_FDA < 0) fc_FDA <- 0
	
	zuschlag 			<- as.numeric(fc_FDA <= biddata[[pmhtnt]]$quantiles$max[ind+1])
	
	# ind runs through forecastInd
	# These are the indices t for which we do forecasts t+1|t and compare them with real values in t+1
	# Name the list entry according to t+1 
	return(c(	fc = fc_FDA, 
				fc_autoArima = statistic_fc, 
				acc = zuschlag, 
				max = biddata[[pmhtnt]]$quantiles$max[ind+1]
				))
	
}


