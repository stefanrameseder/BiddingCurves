						
forecastProphet <- function(ind, pmhtnt, 
							xreg_ver = "x1", 
							dates,
							biddata,
							ver = "v1",
							md_dis,
							growth = "linear"){ # ind <- T-1
	
	# This function estimates the conditional functional mean one step ahead and evaluates it at the mw PosPerc
	
	if(ver == "v1"){
		# Get statistic up to ind; v1 = gavg  
		statistic 	<- biddata[[pmhtnt]]$quantiles$gavg[1:ind] # length(gavg)
		
	} else if (ver == "v2"){
		# Get statistic up to ind; v2 = min  
		statistic 	<- biddata[[pmhtnt]]$quantiles$min[1:ind] # length(gavg)
		
	} else if (ver == "v3"){
		# Get statistic up to ind; v3 = max  
		statistic 	<- biddata[[pmhtnt]]$quantiles$max[1:ind] 
		
	} else if (ver == "v4"){
		# Get statistic up to ind; v4 = efg  
		statistic 	<- biddata[[pmhtnt]]$quantiles$efg[1:ind] 
		
	} else if (ver == "v5"){
		# Get statistic up to ind; v5 = efg  
		statistic 	<- biddata[[pmhtnt]]$quantiles$ezgw[1:ind] 
		
	} else if (ver == "v6"){
		# Get statistic up to ind; v5 = efg  
		statistic 	<- biddata[[pmhtnt]]$quantiles$median[1:ind] # length(gavg)
		
	}else {
		stop("Problem with versions")
	}
	
	christmas_dates <- dates[which(biddata[[pmhtnt]]$quantiles$christmas == 1)]
	
	mw_changes 	  	<- biddata[[pmhtnt]]$maxMW[-1] - biddata[[pmhtnt]]$maxMW[-length(dates)]
	mw_changes_dates <- (dates[-1])[which(mw_changes != 0)]
	pos_change_dates <- (dates[-1])[which(mw_changes > 0)]
	neg_change_dates <- (dates[-1])[which(mw_changes < 0)]
	
	# Holiday combinations 
	if(xreg_ver == "x1"){
		# christmas only 
		holidays		<- data.frame(  ds = christmas_dates, holiday = "christmas")
	} else if (xreg_ver == "x2"){
		# mw changes  
		holidays 		<- data.frame( ds = mw_changes_dates, holiday = "mw_changes")
	} else if (xreg_ver == "x3"){
		# pos changes 
		holidays 		<- data.frame( ds = pos_change_dates, holiday = "pos_mw_changes")
	} else if (xreg_ver == "x4"){
		# neg changes 
		holidays 		<- data.frame( ds = pos_change_dates, holiday = "neg_mw_changes")
	} else if (xreg_ver == "x5"){
		# changes & christmas 
		holidays 		<- rbind( 	data.frame(  ds = christmas_dates, holiday = "christmas"),
									data.frame( ds = pos_change_dates, holiday = "mw_changes"))
		
	} else if (xreg_ver == "x6"){
		# all changes  
		holidays 		<- rbind( 	data.frame(  ds = christmas_dates, holiday = "christmas"),
									data.frame( ds = pos_change_dates, holiday = "pos_mw_changes"),
									data.frame( ds = pos_change_dates, holiday = "neg_mw_changes"))
	} else {
		stop("Problem with reg versions")
	}	
	
	
	df_for_prophet 	<- data.frame(	ds = as.Date(dates[1:ind]),
									y = statistic) 
	
	
									
	m <-				prophet(df = df_for_prophet, holidays = holidays,
								growth,
								n.changepoints = 20, changepoint.range = 0.5,
								yearly.seasonality = "auto", weekly.seasonality = FALSE,
								daily.seasonality = FALSE, 
								seasonality.mode = "additive")
	
	future			<- make_future_dataframe(m, periods = 1)
	
	forecast		<- predict(m, future)
	
	statistic_fc	<- forecast$yhat[ind+1]
	
	# plot(m, forecast)
	
	if(statistic_fc < 0) statistic_fc <- 0
	
	zuschlag 			<- as.numeric(statistic_fc <= biddata[[pmhtnt]]$quantiles$max[ind+1])
	# biddata[[pmhtnt]]$quantiles$max[1:ind+1]
	
	# ind runs through forecastInd
	# These are the indices t for which we do forecasts t+1|t and compare them with real values in t+1
	# Name the list entry according to t+1 
	return(c(	fc = statistic_fc, 
				acc = zuschlag, 
				max = biddata[[pmhtnt]]$quantiles$max[ind+1]
				))
}


