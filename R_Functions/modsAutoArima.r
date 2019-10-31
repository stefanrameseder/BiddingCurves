
modsAutoArima <- function(y, H, threshold = 0.02){ # y <- data[[1]][[2]][,6]; H =1; plot(c(y, fc), type = "l"); points(length(y), fc, col = "red", pch =16)  
	y 		<- ts(y, freq = 7) # Create time series of frequency 7, i.e. 7 units per week
	model 	<- auto.arima(	y, d=0, D=1, max.p=3, max.q=3, max.P=2, max.Q=2, max.order=12,  
							start.p=3, start.q=3, start.P=1, start.Q=1, stationary=TRUE, 
							seasonal=TRUE, ic="bic", stepwise=FALSE, trace=FALSE, allowdrift=TRUE, 
							#lambda=NULL, parallel=FALSE, num.cores=2)
							lambda=NULL, parallel = TRUE, num.cores=4)
	if(any(( fc  <- round(forecast(model, h = H)$mean, 2)) < threshold)){ # if fc is smaller than 20 cent
	  fc[fc< threshold] <- 0
	}
	return(list(model = model, forecast =  list(pred = fc, se = forecast(model, h = H)$mean)))
}

