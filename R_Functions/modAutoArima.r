modAutoArima <- function(y, H, threshold = 0.02){
	model 	<- auto.arima(	y, d=0, D=0, max.p=3, max.q=3, max.P=2, max.Q=2, max.order=8,  
						start.p=3, start.q=3, start.P=2, start.Q=2, stationary=TRUE, 
						seasonal=FALSE, ic="bic", stepwise=FALSE, trace=FALSE, allowdrift=TRUE, 
						#lambda=NULL, parallel=FALSE, num.cores=2)
						lambda=NULL, parallel = TRUE, num.cores=4)
	if(any(( fc	<- round(forecast(model, h = H)$mean, 2)) < threshold)){ # if fc is smaller than 20 cent
		fc[fc< threshold] <- 0
	}
	return(list(model = model, forecast =  list(pred = fc, se = forecast(model, h = H)$mean)))
}
