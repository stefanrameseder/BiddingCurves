forecastCP    		<- function(y, H, K, B, Model = "AutoArima", timeSeries = "MaximumCapacityPrice"){
	# Forecasts one data series y and evaluates the forecasts based on model choice/estimation M1
	# Input: 
	# - y = univariate data series; y = data[[2]][[1]], dim(y)
	# - H = 1: Forecast Horizon
	# - K = 365: Estimation Horizon
	# - B = 365: Backtesting Horizon
	# - model = "AutoArima", "ExpSmoothing", Model = "PrivateBidding"
	# - timeSeries = "MinimumCapacityPrice", "AverageCapacityPrice", "MaximumCapacityPrice"
	# Output:
	# - model = a list with all models chosen 
	# - forecast = a list wit h
	if(!all(c("Date", "MinimumCapacityPrice", "AverageCapacityPrice", "MaximumCapacityPrice") %in% colnames(y))){
		stop("Cannot find all columns for applying Model 1")
	}
	if( (T = nrow(y)) < (K + B +1) ){ # K <- 40
		stop("Not enough datarows for defined estimation procedure")
	}

	model_summary_l     <- list()
	forecast_summary_l  <- list()
	
	
	# Backtesting
	for(b in rev(-(-1:(B-1)))){ # b <- -1 
		
		obs   <- (T + b - K) : (T + b-1) # length(obs) 
		ts    <- y[obs , c("Date", "MinimumCapacityPrice", "AverageCapacityPrice", "MaximumCapacityPrice")] 
		#dim(ts)
		
		model <- get(Model)(y = ts[, timeSeries], H = H) # length(timeSeries) Model = "PrivateBidding"
		
		forecast 	<- model[["forecast"]]
		
		## The listname as forecasting date, i.e. estimation up to T, listname = date [T+1]
		listname	<- format(as.Date( y[ (max(obs)) , "Date"] , format = "%d.%m.%Y") + 1, format="%d.%m.%Y")
		
		model_summary_l[[listname]]    <- model[["model"]]
		
		if( b != 1){ # for old forecast comparison
			cor_max_acc <- y[which( y[, "Date"] == listname) , "MaximumCapacityPrice"] # corresponding max accepted price
			cor_avg_acc <- y[which( y[, "Date"] == listname) , "AverageCapacityPrice"] # corresponding avg accepted price
			forecast_summary_l[[listname]] <- list(fc = as.numeric(forecast[[1]]), sd = NA, acc = as.numeric(cor_max_acc >= forecast$pred[1]), dev = abs(cor_max_acc - as.numeric(forecast$pred[1])), max_acc = cor_max_acc, wavg = cor_avg_acc)
		} else if( b >= 1){ # for future prediction
			forecast_summary_l[[listname]] <-  list(fc = as.numeric(forecast[[1]]), sd = NA)
		}		
		
	}
	
	return(list(model = model_summary_l, forecast = forecast_summary_l))
}

